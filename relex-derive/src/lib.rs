use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use regex_compiler::ast::Regex;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    Data, DataEnum, DeriveInput, ExprClosure, Fields, Ident, LitStr, Token,
};

use regex_compiler::bytecode::ToBytecode;

#[derive(Debug)]
struct SpannedRegex {
    _span: Span,
    regex: Regex,
}

impl SpannedRegex {
    fn new(span: Span, regex: Regex) -> Self {
        Self { _span: span, regex }
    }
}

struct MatchesAttributeMetadata {
    pattern: SpannedRegex,
    action: Option<ExprClosure>,
}

impl std::fmt::Debug for MatchesAttributeMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MatchesAttributeMetadata")
            .field("pattern", &self.pattern)
            .field("action", &self.action.to_token_stream())
            .finish()
    }
}

impl Parse for MatchesAttributeMetadata {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        let spanned_regex = if lookahead.peek(LitStr) {
            let pat: LitStr = input.parse()?;
            let span = pat.span();

            regex_compiler::parser::parse(format!("({})", &pat.value()))
                .map(|regex| SpannedRegex::new(span, regex))
                .map_err(|e| syn::Error::new_spanned(pat, e))?
        } else {
            return Err(lookahead.error());
        };

        // check whether a handler closure has been provided.
        if input.is_empty() {
            syn::Result::Ok(MatchesAttributeMetadata {
                pattern: spanned_regex,
                action: None,
            })
        } else {
            let _separator: Token![,] = input.parse()?;
            let action: ExprClosure = input.parse()?;

            syn::Result::Ok(MatchesAttributeMetadata {
                pattern: spanned_regex,
                action: Some(action),
            })
        }
    }
}

/// Represents all supported attributes for the token.
#[derive(Debug)]
enum LexerAttributeMetadata {
    Matches(MatchesAttributeMetadata),
}

/// Stores all supported lexer attributes for a given Token variant.
struct TokenVariantMetadata {
    variant_ident: Ident,
    attr_metadata: LexerAttributeMetadata,
}

impl TokenVariantMetadata {
    fn new(variant_ident: Ident, attr_metadata: LexerAttributeMetadata) -> Self {
        Self {
            variant_ident,
            attr_metadata,
        }
    }
}

/// Stores the enum and its metadata for generating the tokenizer for each
/// variant.
struct TokenizerVariants {
    /// Represents the Identifier for the Token enum.
    enum_ident: Ident,
    variant_metadata: Vec<TokenVariantMetadata>,
}

impl TokenizerVariants {
    fn new(enum_ident: Ident, variant_metadata: Vec<TokenVariantMetadata>) -> Self {
        Self {
            enum_ident,
            variant_metadata,
        }
    }
}

fn parse(input: DeriveInput) -> Result<TokenizerVariants, syn::Error> {
    let input_span = input.span();
    let tok_enum_name = input.ident;
    let enum_variants = match input.data {
        Data::Enum(DataEnum { variants, .. }) => variants,
        _ => {
            return Err(syn::Error::new(
                input_span,
                "derive macro only works on enums",
            ))
        }
    };

    // token enum variants with their tokenizer metadata parsed.
    enum_variants
        .into_iter()
        .map(|variant| {
            let variant_span = variant.span();
            let variant_ident = variant.ident;
            let variant_fields = variant.fields;

            let attr_kind = variant
                .attrs
                .iter()
                .filter(|attr| attr.path.is_ident("matches"))
                .map(|attr| {
                    attr.parse_args_with(MatchesAttributeMetadata::parse)
                        .and_then(|mam| {
                            let mam_output = mam.action.as_ref().map(|action| &action.output);
                            match (variant_fields.clone(), mam_output) {
                                // an unamed struct with one field
                                (Fields::Unnamed(f), Some(_)) if f.unnamed.len() == 1 => Ok(mam),
                                // an empty filed
                                (Fields::Unit, None) => Ok(mam),
                                (l, _) => Err(syn::Error::new(
                                    l.span(),
                                    format!(
                                        "variant({}) expects exactly 1 unnamed field, got {}",
                                        &variant_ident,
                                        l.len()
                                    ),
                                )),
                            }
                        })
                        .map(LexerAttributeMetadata::Matches)
                });

            let mut variant_match_attr = attr_kind.collect::<Result<Vec<_>, _>>()?;
            if variant_match_attr.len() == 1 {
                let attr = variant_match_attr.pop().unwrap();
                Ok(TokenVariantMetadata::new(variant_ident, attr))
            } else {
                Err(syn::Error::new(
                    variant_span,
                    "expect exactly one match attribute specified",
                ))
            }
        })
        .collect::<Result<_, _>>()
        .map(|enriched_token_variants| {
            TokenizerVariants::new(tok_enum_name, enriched_token_variants)
        })
}

/// Represents the generated header that is present in every lexer.
struct CodeGenHeader;

impl ToTokens for CodeGenHeader {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let header_stream = quote! {
            use regex_runtime::{Instructions, SaveGroupSlot};

            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub struct Span {
                start: usize,
                end: usize,
            }

            impl From<Span> for std::ops::Range<usize> {
                fn from(span: Span) -> Self {
                    let start = span.start;
                    let end = span.end;

                    start..end
                }
            }

            impl From<std::ops::Range<usize>> for Span {
                fn from(range: std::ops::Range<usize>) -> Self {
                    Self {
                        start: range.start,
                        end: range.end,
                    }
                }
            }
        };

        tokens.extend(header_stream)
    }
}

struct CodeGenSpannedToken<'a> {
    enum_name: &'a Ident,
}

impl<'a> CodeGenSpannedToken<'a> {
    fn new(enum_name: &'a Ident) -> Self {
        Self { enum_name }
    }
}

impl<'a> ToTokens for CodeGenSpannedToken<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let tok_enum_name = &self.enum_name;

        let spanned_token = quote! {
            #[derive(Debug)]
            pub struct SpannedToken {
                span: Span,
                variant: #tok_enum_name,
            }

            impl SpannedToken {
                pub fn new(span: Span, variant: #tok_enum_name) -> Self {
                    Self { span, variant }
                }

                pub fn as_span(&self) -> Span {
                    self.span
                }

                pub fn to_variant(self) -> #tok_enum_name {
                    self.variant
                }
            }

            pub fn token_stream_from_input(input: &str) -> Result<TokenStream<'_>, String> {
                use regex_runtime::bytecode::FromBytecode;

                let program = Instructions::from_bytecode(PROG_BINARY).map_err(|e| e.to_string())?;
                Ok(TokenStream::new(program, input))
            }
        };

        tokens.extend(spanned_token)
    }
}

fn codegen(input: DeriveInput) -> syn::Result<TokenStream> {
    let token_metadata = parse(input)?;
    let tok_enum_name = &token_metadata.enum_ident;

    let expression_matchers = token_metadata
        .variant_metadata
        .iter()
        .enumerate()
        .map(|(expr_id, other)| {
            let id = u32::try_from(expr_id).unwrap();

            (id, other)
        })
        .map(
            |(
                expr_id,
                TokenVariantMetadata {
                    variant_ident,
                    attr_metadata,
                },
            )| match &attr_metadata {
                LexerAttributeMetadata::Matches(mam) => {
                    let optional_action = &mam.action;

                    match optional_action {
                        Some(action) => quote! {
                            #expr_id => (#action)(val).map(#tok_enum_name::#variant_ident),
                        },
                        None => quote! {
                            #expr_id => Some(#tok_enum_name::#variant_ident),
                        },
                    }
                }
            },
        )
        .collect::<TokenStream>();

    let prog_patterns = token_metadata.variant_metadata.into_iter().fold(
        vec![],
        |mut acc, TokenVariantMetadata { attr_metadata, .. }| match attr_metadata {
            LexerAttributeMetadata::Matches(mam) => {
                acc.push(mam.pattern.regex);
                acc
            }
        },
    );

    let instructions = regex_compiler::compile_many(prog_patterns)
        .map(|insts| insts.to_bytecode())
        .unwrap();

    let byte_count = instructions.len();

    let instructions = instructions
        .into_iter()
        .map(|byte| quote! {#byte,})
        .collect::<TokenStream>();

    let program = quote! {
        const PROG_BINARY: [u8; #byte_count] = [ #instructions ];
    };

    let token_stream = quote! {
            pub struct TokenStream<'a> {
                input_stream: &'a str,
                program: Instructions,
                offset: usize,
            }

            impl<'a> TokenStream<'a> {
                pub fn new(program: Instructions, input_stream: &'a str) -> Self {
                    Self {
                        input_stream,
                        program,
                        offset: 0,
                    }
                }
            }

            impl<'a> Iterator for TokenStream<'a> {
                type Item = SpannedToken;

                #[allow(clippy::redundant_closure_call)]
                fn next(&mut self) -> Option<Self::Item> {
                    let res = regex_runtime::run::<1>(&self.program, self.input_stream);

                    let (tok, next_input, next_offset) = match res {
                        Some(
                            [SaveGroupSlot::Complete {
                                expression_id,
                                start,
                                end,
                            }],
                        ) => {
                            let val = self.input_stream.get(start..end)?;

                            let variant = match expression_id {
                                #expression_matchers
                                _ => unreachable!(),
                            };

                            let next_input = self.input_stream.get(end..)?;
                            // the next match should always start at 0, so the end value marks consumed chars.
                            let consumed = end;

                            let adjusted_start = self.offset + start;
                            let adjusted_end = self.offset + end;

                            variant
                                .map(|tv| SpannedToken::new(Span::from(adjusted_start..adjusted_end), tv))
                                .map(|tok| (tok, next_input, consumed))
                        }

                        _ => None,
                    }?;

                    // advance the stream
                    self.input_stream = next_input;
                    self.offset += next_offset;
                    Some(tok)
                }
            }
    };

    Ok(CodeGenHeader
        .to_token_stream()
        .into_iter()
        .chain(CodeGenSpannedToken::new(tok_enum_name).to_token_stream())
        .chain(program.into_iter())
        .chain(token_stream.into_iter())
        .collect())
}

#[proc_macro_derive(Relex, attributes(matches))]
pub fn relex(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    codegen(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

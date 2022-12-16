use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    Data, DataEnum, DeriveInput, ExprClosure, Fields, Ident, LitStr, Token,
};

use regex_compiler::bytecode::ToBytecode;

/// Represent a regex ast with an associated span.
#[derive(Debug)]
struct SpannedRegex {
    _span: Span,
    regex: regex_compiler::ast::Regex,
}

impl SpannedRegex {
    fn new(span: Span, regex: regex_compiler::ast::Regex) -> Self {
        Self { _span: span, regex }
    }
}

#[derive(Debug)]
enum OptionalAction {
    Closure(ExprClosure),
    Fn(Ident),
    None,
}

/// Parsed metatdata for the `Matches` attribute.
struct MatchesAttributeMetadata {
    /// Represents the defined regular expression used to match a token.
    pattern: SpannedRegex,
    /// An optional action for converting the matched pattern to a
    /// corresponding token field.
    action: OptionalAction,
}

impl std::fmt::Debug for MatchesAttributeMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let action_field_data = match &self.action {
            OptionalAction::Closure(c) => format!("Closure({:?})", c.to_token_stream()),
            OptionalAction::Fn(f) => format!("Fn({:?})", f.to_token_stream()),
            other => format!("{:?}", other),
        };

        f.debug_struct("MatchesAttributeMetadata")
            .field("pattern", &self.pattern)
            .field("action", &action_field_data)
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
                action: OptionalAction::None,
            })
        } else {
            let _separator: Token![,] = input.parse()?;
            let expr_closure_action = input.parse::<ExprClosure>();
            match expr_closure_action {
                Ok(action) => syn::Result::Ok(MatchesAttributeMetadata {
                    pattern: spanned_regex,
                    action: OptionalAction::Closure(action),
                }),
                Err(_) => {
                    let action = input.parse::<Ident>().map_err(|e| {
                        let span = e.span();
                        syn::Error::new(span, "expected either a closure or a function identifier")
                    })?;
                    syn::Result::Ok(MatchesAttributeMetadata {
                        pattern: spanned_regex,
                        action: OptionalAction::Fn(action),
                    })
                }
            }
        }
    }
}

/// Parsed metatdata for the `skips` attribute.
#[derive(Debug)]
#[allow(unused)]
struct SkipAttributeMetadata {
    /// Represents the defined regular expression used to match a token.
    pattern: SpannedRegex,
}

impl Parse for SkipAttributeMetadata {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let input_span = input.span();
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
            syn::Result::Ok(SkipAttributeMetadata {
                pattern: spanned_regex,
            })
        } else {
            Err(syn::Error::new(
                input_span,
                "expected no additional attributes after the pattern",
            ))
        }
    }
}

#[derive(Debug)]
enum LexerAttributeKind {
    Matches,
    Skip,
}

/// Represents all supported attributes for the token.
#[derive(Debug)]
enum LexerAttributeMetadata {
    Matches(MatchesAttributeMetadata),
    Skip(SkipAttributeMetadata),
}

/// Stores all supported lexer attributes for a given Token variant.
struct TokenVariantMetadata {
    ident: Ident,
    attr_metadata: LexerAttributeMetadata,
}

impl TokenVariantMetadata {
    fn new(ident: Ident, attr_metadata: LexerAttributeMetadata) -> Self {
        Self {
            ident,
            attr_metadata,
        }
    }
}

/// Stores the enum and its metadata for generating the tokenizer for each
/// variant.
struct TokenizerVariants {
    span: Span,
    /// Represents the Identifier for the Token enum.
    enum_ident: Ident,
    variant_metadata: Vec<TokenVariantMetadata>,
}

impl TokenizerVariants {
    fn new(span: Span, enum_ident: Ident, variant_metadata: Vec<TokenVariantMetadata>) -> Self {
        Self {
            span,
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
                .filter_map(|attr| {
                    if attr.path.is_ident("matches") {
                        Some((LexerAttributeKind::Matches, attr))
                    } else if attr.path.is_ident("skip") {
                        Some((LexerAttributeKind::Skip, attr))
                    } else {
                        None
                    }
                })
                .map(|(kind, attr)| match kind {
                    LexerAttributeKind::Matches => {
                        attr.parse_args_with(MatchesAttributeMetadata::parse)
                            .and_then(|mam| {
                                match variant_fields.clone() {
                                    // an unamed struct with one field
                                    Fields::Unnamed(f) if f.unnamed.len() == 1 => Ok(mam),
                                    // an empty filed
                                    Fields::Unit => Ok(mam),
                                    l => Err(syn::Error::new(
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
                    }
                    LexerAttributeKind::Skip => attr
                        .parse_args_with(SkipAttributeMetadata::parse)
                        .map(LexerAttributeMetadata::Skip),
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
            TokenizerVariants::new(input_span, tok_enum_name, enriched_token_variants)
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

/// Generates the enum used to capture all token variants at runtime.
struct CodeGenSpannedToken<'a> {
    enum_ident: &'a Ident,
}

impl<'a> CodeGenSpannedToken<'a> {
    fn new(enum_ident: &'a Ident) -> Self {
        Self { enum_ident }
    }
}

impl<'a> ToTokens for CodeGenSpannedToken<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let tok_enum_ident = self.enum_ident;

        let spanned_token = quote! {
            #[derive(Debug)]
            pub struct SpannedToken {
                span: Span,
                variant: #tok_enum_ident,
            }

            impl SpannedToken {
                pub fn new(span: Span, variant: #tok_enum_ident) -> Self {
                    Self { span, variant }
                }

                pub fn as_span(&self) -> Span {
                    self.span
                }

                pub fn to_variant(self) -> #tok_enum_ident {
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

/// Captures the generated regex binary to be embedded in the program.
struct CodeGenProgram {
    program: regex_runtime::Instructions,
}

impl CodeGenProgram {
    fn new(program: regex_runtime::Instructions) -> Self {
        Self { program }
    }
}

impl ToTokens for CodeGenProgram {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let instructions = self.program.to_bytecode();

        let byte_count = instructions.len();

        let instructions = instructions
            .into_iter()
            .map(|byte| quote! {#byte,})
            .collect::<TokenStream>();

        let program_tokens = quote! {
            const PROG_BINARY: [u8; #byte_count] = [ #instructions ];
        };

        tokens.extend(program_tokens)
    }
}

/// Captures the `TokenStream`, not to be confused with
/// `proc-macro::TokenStream` to provide a stream of lexed tokens at runtime.
struct CodeGenTokenStream {
    matchers: TokenStream,
}

impl CodeGenTokenStream {
    fn new(token_metadata: &TokenizerVariants) -> Self {
        let tok_enum_ident = &token_metadata.enum_ident;

        let matchers = token_metadata
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
                        ident: variant_ident,
                        attr_metadata,
                    },
                )| match &attr_metadata {
                    LexerAttributeMetadata::Matches(MatchesAttributeMetadata { action: optional_action , ..}) => {
                        match optional_action {
                            OptionalAction::Closure(action) => quote! {
                                #expr_id => match (#action)(val) {
                                    Some(res) => TokenStreamOutput::Match(#tok_enum_ident::#variant_ident(res)),
                                    None => TokenStreamOutput::NoMatch,
                                }
                            },
                            OptionalAction::Fn(action) => quote! {
                                #expr_id => match (#action)(val) {
                                    Some(res) => TokenStreamOutput::Match(#tok_enum_ident::#variant_ident(res)),
                                    None => TokenStreamOutput::NoMatch,
                                }
                            },
                            OptionalAction::None => quote! {
                                #expr_id => TokenStreamOutput::Match(#tok_enum_ident::#variant_ident),
                            },
                        }
                    }
                    LexerAttributeMetadata::Skip(_) => quote! {
                        #expr_id => TokenStreamOutput::Skip(#tok_enum_ident::#variant_ident),
                    },
                },
            )
            .collect::<TokenStream>();

        Self { matchers }
    }
}

impl ToTokens for CodeGenTokenStream {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let expression_matchers = &self.matchers;

        let token_stream = quote! {
                enum TokenStreamOutput<T> {
                    Match(T),
                    Skip(T),
                    NoMatch,
                }

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

                    fn next(&mut self) -> Option<Self::Item> {
                        loop {
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

                                    match variant {
                                        TokenStreamOutput::Match(tv) => Some(TokenStreamOutput::Match(SpannedToken::new(Span::from(adjusted_start..adjusted_end), tv))),
                                        TokenStreamOutput::Skip(tv) => Some(TokenStreamOutput::Skip(SpannedToken::new(Span::from(adjusted_start..adjusted_end), tv))),
                                        TokenStreamOutput::NoMatch => None,
                                    }.map(|tok| (tok, next_input, consumed))
                                }

                                _ => None,
                            }?;

                            // advance the stream
                            self.input_stream = next_input;
                            self.offset += next_offset;

                            match tok {
                                TokenStreamOutput::Skip(t) => continue,
                                TokenStreamOutput::NoMatch => return None,
                                TokenStreamOutput::Match(t) => return Some(t),

                            }
                        }
                    }
                }
        };

        tokens.extend(token_stream)
    }
}

/// Generates a runtime lexer from a set of variants.
fn codegen(token_metadata: TokenizerVariants) -> syn::Result<TokenStream> {
    let tok_enum_ident = &token_metadata.enum_ident;
    let tok_enum_span = token_metadata.span;

    // generate the tokenizer generator from the parsed metadata.
    let codegen_token_stream = CodeGenTokenStream::new(&token_metadata);

    // Compile all regular expressions into the state machine and convert to a
    // binary representation for embedding.
    let codegen_program = {
        let prog_patterns = token_metadata
            .variant_metadata
            .into_iter()
            .map(
                |TokenVariantMetadata { attr_metadata, .. }| match attr_metadata {
                    LexerAttributeMetadata::Matches(MatchesAttributeMetadata {
                        pattern, ..
                    }) => pattern.regex,
                    LexerAttributeMetadata::Skip(SkipAttributeMetadata { pattern }) => {
                        pattern.regex
                    }
                },
            )
            .collect::<Vec<_>>();

        regex_compiler::compile_many(prog_patterns)
            .map_err(|e| syn::Error::new(tok_enum_span, e))
            .map(CodeGenProgram::new)
    }?;

    // Join all the token streams into a single stream.
    Ok(CodeGenHeader
        .to_token_stream()
        .into_iter()
        .chain(CodeGenSpannedToken::new(tok_enum_ident).to_token_stream())
        .chain(codegen_program.to_token_stream())
        .chain(codegen_token_stream.to_token_stream())
        .collect())
}

/// The dispatcher method for tokens annotated with the Relex derive.
#[proc_macro_derive(Relex, attributes(matches, skip))]
pub fn relex(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    parse(input)
        .and_then(codegen)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

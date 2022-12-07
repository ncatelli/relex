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

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    span: Span,
    variant: TokenVariant,
}

impl Token {
    pub fn new(span: Span, variant: TokenVariant) -> Self {
        Self { span, variant }
    }

    pub fn as_span(&self) -> Span {
        self.span
    }

    pub fn to_variant(self) -> TokenVariant {
        self.variant
    }
}

pub fn token_stream_from_input(input: &str) -> Result<TokenStream<'_>, String> {
    use regex_runtime::bytecode::FromBytecode;

    let program = Instructions::from_bytecode(PROG_BINARY).map_err(|e| e.to_string())?;
    Ok(TokenStream::new(program, input))
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

const PROG_BINARY: [u8; 416] = [
    240, 240, 0, 0, 1, 0, 0, 0, 23, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 26, 26, 0, 0, 1, 0, 0, 0, 48, 0, 0, 0, 57, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0,
    1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0,
    0, 18, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 9, 0,
    0, 0, 11, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 8,
    0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0,
    0, 0, 59, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
#[derive(Debug, PartialEq, Eq)]
pub enum TokenVariant {
    Number(i32),
    Plus,
    Semicolon,
}
impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

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
                    0 => val.parse::<i32>().ok().map(TokenVariant::Number),
                    1 => Some(TokenVariant::Plus),
                    2 => Some(TokenVariant::Semicolon),

                    _ => unreachable!(),
                };

                let next_input = self.input_stream.get(end..)?;
                // the next match should always start at 0, so the end value marks consumed chars.
                let consumed = end;

                let adjusted_start = self.offset + start;
                let adjusted_end = self.offset + end;

                variant
                    .map(|tv| Token::new(Span::from(adjusted_start..adjusted_end), tv))
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

fn main() -> Result<(), String> {
    let token_stream = token_stream_from_input("1+25;")?;

    for tok in token_stream {
        println!("{:?}", tok);
    }

    Ok(())
}

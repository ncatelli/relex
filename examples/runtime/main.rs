use regex_runtime::{bytecode::FromBytecode, Instructions, SaveGroupSlot};

const TEST_BIN: [u8; 416] = [
    240, 240, 0, 0, 1, 0, 0, 0, 23, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 26, 26, 0, 0, 1, 0, 0, 0, 48, 0, 0, 0, 57, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0,
    1, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 15, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 43,
    0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 59, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0,
    0, 23, 0, 0, 0, 21, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0,
    0, 0, 20, 0, 0, 0, 0, 0, 0, 0,
];

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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum TokenVariant {
    // "^([0-9]+)"
    Number(i32),
    // "^(+)"
    Plus,
    // "^(;)"
    Semicolon,
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
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut curr_input = self.input_stream;
        let mut consumed = self.offset;
        let res = regex_runtime::run::<1>(&self.program, curr_input);

        let res = match res {
            Some(
                [SaveGroupSlot::Complete {
                    expression_id,
                    start,
                    end,
                }],
            ) => {
                let matching_value = &curr_input[start..end];

                let variant = match expression_id {
                    0 => matching_value
                        .parse::<i32>()
                        .map_err(|e| e.to_string())
                        .map(TokenVariant::Number)
                        .ok(),
                    1 => Some(TokenVariant::Plus),
                    2 => Some(TokenVariant::Semicolon),
                    _ => None,
                };

                curr_input = &curr_input[end..];
                consumed = end.saturating_sub(start);

                let adjusted_start = self.offset + start;
                let adjusted_end = self.offset + end;

                variant.map(|tv| Token::new(Span::from(adjusted_start..adjusted_end), tv))
            }

            _ => None,
        };

        // advance the stream
        self.input_stream = curr_input;
        self.offset += consumed;
        res
    }
}

fn main() -> Result<(), String> {
    let program = Instructions::from_bytecode(TEST_BIN).map_err(|e| e.to_string())?;
    let input = "1+25;";
    let token_stream = TokenStream::new(program, input);

    for token in token_stream {
        println!("{:?} ", &token);
    }

    Ok(())
}

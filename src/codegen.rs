use crate::ast::CaptureType;

use super::ast;

trait ToRust {
    type Error;

    fn to_rust_code(&self) -> Result<String, Self::Error>;
}

impl ToRust for CaptureType {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        use ast::{IntTypeBitWidth, IntTypeSign};

        let repr = match self {
            CaptureType::String => "String".to_string(),
            CaptureType::Bool => "bool".to_string(),
            CaptureType::Int(it) => {
                let signedness = match it.sign {
                    IntTypeSign::Signed => "i",
                    IntTypeSign::Unsigned => "u",
                };
                let width = match it.width {
                    IntTypeBitWidth::Eight => "8",
                    IntTypeBitWidth::Sixteen => "16",
                    IntTypeBitWidth::ThirtyTwo => "32",
                    IntTypeBitWidth::SixtyFour => "64",
                };

                format!("{}{}", signedness, width)
            }
        };

        Ok(repr)
    }
}

struct Token<'a>(Vec<TokenVariant<'a>>);

impl<'a> ToRust for Token<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let prefix = "#[derive(Debug, Clone, PartialEq)]\npub enum Token {\n".to_string();
        let suffix = "}".to_string();

        let variants = self
            .0
            .iter()
            .map(|variant| variant.to_rust_code().map(|s| format!("{},\n", s)));

        [Ok(prefix)]
            .into_iter()
            .chain(variants)
            .chain([Ok(suffix)].into_iter())
            .collect()
    }
}

struct TokenVariant<'a> {
    id: &'a str,
    captures: Vec<&'a CaptureType>,
}

impl<'a> ToRust for TokenVariant<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let variant = if self.captures.is_empty() {
            self.id.to_string()
        } else {
            let captures: Result<Vec<_>, _> =
                self.captures.iter().map(|ct| ct.to_rust_code()).collect();
            let captures = captures.map_err(|_| ())?;

            format!("{}({})", self.id, captures.join(", "))
        };

        Ok(variant)
    }
}

pub fn codegen(_rules: ast::Rules) -> Result<String, String> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_generate_tokens_without_error() {
        use crate::ast::*;
        let int8_ct = CaptureType::Int(IntType {
            sign: IntTypeSign::Signed,
            width: IntTypeBitWidth::Eight,
        });
        let uint64_ct = CaptureType::Int(IntType {
            sign: IntTypeSign::Unsigned,
            width: IntTypeBitWidth::SixtyFour,
        });
        let str_ct = CaptureType::String;
        let bool_ct = CaptureType::Bool;

        let inputs = vec![
            Token(vec![TokenVariant {
                id: "Test",
                captures: vec![],
            }]),
            Token(vec![TokenVariant {
                id: "Test",
                captures: vec![&int8_ct],
            }]),
            Token(vec![TokenVariant {
                id: "Test",
                captures: vec![&int8_ct, &bool_ct, &str_ct, &uint64_ct],
            }]),
        ];

        for (test, input) in inputs.into_iter().enumerate() {
            assert_eq!((test, true), (test, input.to_rust_code().is_ok()));
        }
    }
}

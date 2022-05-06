use std::fmt::Write;

#[derive(Debug, PartialEq)]
pub struct Rules(pub Vec<Rule>);

#[derive(Debug, PartialEq)]
pub struct Rule {
    identifier: Identifier,
    capture_type: Option<CaptureType>,
    pattern: Pattern,
    action: Action,
}

impl Rule {
    pub fn new(
        identifier: Identifier,
        capture_type: Option<CaptureType>,
        pattern: Pattern,
        action: Action,
    ) -> Self {
        Self {
            identifier,
            capture_type,
            pattern,
            action,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn try_new(id: String) -> Option<Self> {
        let is_valid = id.chars().all(|c| c.is_alphabetic() || c == '_');
        if is_valid {
            Some(Identifier(id))
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CaptureType(pub Vec<CaptureTypeItem>);

#[derive(Debug, PartialEq)]
pub enum CaptureTypeItem {
    String,
    Bool,
    Int(IntType),
}

#[derive(Debug, PartialEq)]
pub struct IntType {
    pub sign: IntTypeSign,
    pub width: IntTypeBitWidth,
}

impl IntType {
    pub fn new(sign: IntTypeSign, width: IntTypeBitWidth) -> Self {
        Self { sign, width }
    }
}

#[derive(Debug, PartialEq)]
pub enum IntTypeSign {
    Signed,
    Unsigned,
}

#[derive(Debug, PartialEq)]
pub enum IntTypeBitWidth {
    Eight,
    Sixteen,
    ThirtyTwo,
    SixtyFour,
}

#[derive(Debug, PartialEq)]
pub struct Pattern(pub PatternItem);

impl From<Pattern> for String {
    fn from(src: Pattern) -> Self {
        src.0.to_string()
    }
}

#[derive(Debug, PartialEq)]
pub struct PatternItem(pub Vec<Char>);

impl From<PatternItem> for String {
    fn from(src: PatternItem) -> Self {
        src.to_string()
    }
}

impl std::fmt::Display for PatternItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.0.iter().map(|c| c.as_char()) {
            f.write_char(c)?
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Action(pub ActionItem);

impl From<Action> for String {
    fn from(src: Action) -> Self {
        src.0.to_string()
    }
}

#[derive(Debug, PartialEq)]
pub struct ActionItem(pub Vec<Char>);

impl From<ActionItem> for String {
    fn from(src: ActionItem) -> Self {
        src.to_string()
    }
}

impl std::fmt::Display for ActionItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.0.iter().map(|c| c.as_char()) {
            f.write_char(c)?
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Char(pub char);

impl Char {
    pub fn as_char(&self) -> char {
        self.0
    }
}

impl From<Char> for char {
    fn from(src: Char) -> Self {
        src.as_char()
    }
}

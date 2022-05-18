use std::fmt::Write;

#[derive(Debug, PartialEq)]
pub struct Rules(pub Vec<Rule>);

impl AsRef<[Rule]> for Rules {
    fn as_ref(&self) -> &[Rule] {
        self.0.as_ref()
    }
}

#[derive(Debug, PartialEq)]
pub struct Rule {
    pub identifier: Identifier,
    pub capture: Option<Capture>,
    pub pattern: Pattern,
    pub action: Action,
}

impl Rule {
    pub fn new(
        identifier: Identifier,
        capture: Option<Capture>,
        pattern: Pattern,
        action: Action,
    ) -> Self {
        Self {
            identifier,
            capture,
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
            Some(Self(id))
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Capture(pub Vec<CaptureItem>);

#[derive(Debug, PartialEq)]
pub struct CaptureItem {
    pub identifier: CaptureIdentifier,
    pub ty: CaptureType,
}

impl CaptureItem {
    pub fn new(identifier: CaptureIdentifier, ty: CaptureType) -> Self {
        Self { identifier, ty }
    }
}

#[derive(Debug, PartialEq)]
pub enum CaptureType {
    String,
    Bool,
    Int(IntType),
}

#[derive(Debug, PartialEq)]
pub struct CaptureIdentifier(pub String);

impl CaptureIdentifier {
    pub fn try_new(id: String) -> Option<Self> {
        let is_valid = id.chars().all(|c| c.is_alphabetic() || c == '_');
        if is_valid {
            Some(Self(id))
        } else {
            None
        }
    }
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

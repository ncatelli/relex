#[derive(Debug, PartialEq)]
pub struct Rules(Vec<Rule>);

#[derive(Debug, PartialEq)]
pub struct Rule {
    name: Name,
    capture_type: Option<CaptureType>,
    pattern: Pattern,
    action: Action,
}

#[derive(Debug, PartialEq)]
pub struct Name(Identifier);

#[derive(Debug, PartialEq)]
pub struct CaptureType(Vec<CaptureTypeItem>);

#[derive(Debug, PartialEq)]
pub enum CaptureTypeItem {
    String,
    Bool,
    Int(IntType),
}

#[derive(Debug, PartialEq)]
pub struct IntType {
    sign: IntTypeSign,
    width: IntTypeBitWidth,
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
pub struct Pattern(PatternItem);

#[derive(Debug, PartialEq)]
pub struct PatternItem(Vec<Char>);

#[derive(Debug, PartialEq)]
pub struct Action(ActionItem);

#[derive(Debug, PartialEq)]
pub struct ActionItem(Vec<Char>);

#[derive(Debug, PartialEq)]
pub struct Identifier(String);

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
pub struct Char(char);

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

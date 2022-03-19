pub enum Ast {
    Integer(Integer),
}

// Quantifiers

pub enum Quantifier {
    Eager(QuantifierType),
    Lazy(QuantifierType),
}

/// Signifies that a type is representative of a Regex Quantifier and can be
/// converted to a `QualifierType` variant.
pub trait IsQuantifierType: Into<QuantifierType> {}

/// Represents all variants of quantifier types
pub enum QuantifierType {
    MatchExactRange(Integer),
    MatchAtleastRange(Integer),
    MatchBetweenRange {
        lower_bound: Integer,
        upper_bound: Integer,
    },
    /// Represents a quantifier representing a match of zero or more of the
    /// preceeding field. Represented by the `*` quantifier.
    ZeroOrMore,
    /// Represents a quantifier representing a match of one or more of the
    /// preceeding field. Represented by the `+` quantifier.
    OneOrMore,
    /// Represents an optional quantifier representing a match of zero or one
    /// field. Represented by the `?` quantifier.
    ZeroOrOne,
}

impl From<ZeroOrMoreQuantifier> for QuantifierType {
    fn from(_: ZeroOrMoreQuantifier) -> Self {
        QuantifierType::ZeroOrMore
    }
}

impl From<OneOrMoreQuantifier> for QuantifierType {
    fn from(_: OneOrMoreQuantifier) -> Self {
        QuantifierType::OneOrMore
    }
}

impl From<ZeroOrOneQuantifier> for QuantifierType {
    fn from(_: ZeroOrOneQuantifier) -> Self {
        QuantifierType::ZeroOrOne
    }
}

impl From<RangeQuantifier> for QuantifierType {
    fn from(src: RangeQuantifier) -> Self {
        let lower_bound = src.lower_bound;
        let upper_bound = src.upper_bound;

        match (lower_bound, upper_bound) {
            (lower, None) => QuantifierType::MatchExactRange(lower.0),
            (lower, Some(None)) => QuantifierType::MatchAtleastRange(lower.0),
            (lower, Some(Some(upper))) => QuantifierType::MatchBetweenRange {
                lower_bound: lower.0,
                upper_bound: upper.0,
            },
        }
    }
}

/// Represents an optional modifier for the quantifier represented by the `?`
/// quantifier.
pub struct LazyModifier;

/// A Regex Range Qualifier representable by the following three expressions.
/// `{n}`: Match exactly.
/// `{n,}`: Match atleast.
/// `{n,m}` Match between range.
pub struct RangeQuantifier {
    lower_bound: RangeQuantifierLowerBound,
    upper_bound: Option<Option<RangeQuantifierUpperBound>>,
}

impl RangeQuantifier {
    pub fn new(
        lower_bound: RangeQuantifierLowerBound,
        upper_bound: Option<Option<RangeQuantifierUpperBound>>,
    ) -> Self {
        Self {
            lower_bound,
            upper_bound,
        }
    }
}

impl IsQuantifierType for RangeQuantifier {}

/// A lower-bound representation of a range qualifier.
/// `{n,m}` representing the `n` in the previous expression.
pub struct RangeQuantifierLowerBound(pub Integer);

/// An upper-bound representation of a range qualifier.
/// `{n,m}` representing the `m` in the previous expression.
pub struct RangeQuantifierUpperBound(pub Integer);

/// Represents a quantifier representing a match of zero or more of the
/// preceeding field. Represented by the `*` quantifier.
pub struct ZeroOrMoreQuantifier;

impl IsQuantifierType for ZeroOrMoreQuantifier {}

/// Represents a quantifier representing a match of one or more of the
/// preceeding field. Represented by the `+` quantifier.
pub struct OneOrMoreQuantifier;

impl IsQuantifierType for OneOrMoreQuantifier {}

/// Represents an optional quantifier representing a match of zero or one
/// field. Represented by the `?` quantifier.
pub struct ZeroOrOneQuantifier;

impl IsQuantifierType for ZeroOrOneQuantifier {}

// Backreference

pub struct Backreference(pub Integer);

// Terminals

#[repr(transparent)]
pub struct Integer(pub isize);

impl Integer {
    pub fn as_isize(&self) -> isize {
        self.0
    }
}

impl From<Integer> for isize {
    fn from(src: Integer) -> Self {
        src.as_isize()
    }
}

use std::sync::atomic::{AtomicUsize, Ordering};

use super::ast;
use relex_runtime::*;

/// Tracks an incrementing identifier for save groups.
static SAVE_GROUP_ID: AtomicUsize = AtomicUsize::new(0);

/// A internal representation of the `relex_runtime::Opcode` type, with relative
/// addressing.
///
/// ## Note
/// This type is meant to exist only internally and should be
/// refined to the `relex_runtime::Opcode type
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
enum RelativeOpcode {
    Any,
    Consume(char),
    ConsumeSet(CharacterSet),
    Split(i32, i32),
    Jmp(i32),
    StartSave(usize),
    EndSave(usize),
    Match,
}

impl RelativeOpcode {
    fn into_opcode_with_index(self, sets: &mut Vec<CharacterSet>, idx: u32) -> Option<Opcode> {
        match self {
            RelativeOpcode::Any => Some(Opcode::Any),
            RelativeOpcode::Consume(c) => Some(Opcode::Consume(InstConsume::new(c))),
            RelativeOpcode::Split(rel_x, rel_y) => {
                let signed_idx = idx as i32;
                let x: u32 = (signed_idx + rel_x).try_into().ok()?;
                let y: u32 = (signed_idx + rel_y).try_into().ok()?;

                Some(Opcode::Split(InstSplit::new(
                    InstIndex::from(x),
                    InstIndex::from(y),
                )))
            }
            RelativeOpcode::Jmp(rel_jmp_to) => {
                let signed_idx = idx as i32;
                let jmp_to: u32 = (signed_idx + rel_jmp_to).try_into().ok()?;

                Some(Opcode::Jmp(InstJmp::new(InstIndex::from(jmp_to))))
            }
            RelativeOpcode::StartSave(slot) => Some(Opcode::StartSave(InstStartSave::new(slot))),
            RelativeOpcode::EndSave(slot) => Some(Opcode::EndSave(InstEndSave::new(slot))),
            RelativeOpcode::Match => Some(Opcode::Match),
            RelativeOpcode::ConsumeSet(char_set) => {
                let found = sets.iter().position(|set| set == &char_set);
                let set_idx = match found {
                    Some(set_idx) => set_idx,
                    None => {
                        let set_idx = sets.len();
                        sets.push(char_set);
                        set_idx
                    }
                };

                Some(Opcode::ConsumeSet(InstConsumeSet { idx: set_idx }))
            }
        }
    }

    fn into_opcode_with_index_unchecked(self, sets: &mut Vec<CharacterSet>, idx: u32) -> Opcode {
        self.into_opcode_with_index(sets, idx).unwrap()
    }
}

type RelativeOpcodes = Vec<RelativeOpcode>;

pub fn compile(regex_ast: ast::Regex) -> Result<Instructions, String> {
    let suffix = [RelativeOpcode::Match];

    let relative_ops: Result<RelativeOpcodes, _> = match regex_ast {
        ast::Regex::StartOfStringAnchored(expr) => {
            expression(expr).map(|expr| expr.into_iter().chain(suffix.into_iter()).collect())
        }
        ast::Regex::Unanchored(expr) => expression(expr).map(|expr| {
            // match anything
            let prefix = [
                RelativeOpcode::Split(3, 1),
                RelativeOpcode::Any,
                RelativeOpcode::Jmp(-2),
            ];

            prefix
                .into_iter()
                .chain(expr.into_iter())
                .chain(suffix.into_iter())
                .collect()
        }),
    };

    relative_ops
        .map(|rel_ops| {
            let (sets, absolute_insts) = rel_ops
                .into_iter()
                .enumerate()
                // truncate idx to a u32
                .map(|(idx, sets)| (idx as u32, sets))
                .fold((vec![], vec![]), |(mut sets, mut insts), (idx, opcode)| {
                    let absolute_opcode = opcode.into_opcode_with_index_unchecked(&mut sets, idx);
                    insts.push(absolute_opcode);

                    (sets, insts)
                });

            (sets, absolute_insts)
        })
        .map(|(sets, insts)| Instructions::new(sets, insts))
}

fn expression(expr: ast::Expression) -> Result<RelativeOpcodes, String> {
    let ast::Expression(subexprs) = expr;

    let compiled_subexprs = subexprs
        .into_iter()
        .map(subexpression)
        .collect::<Result<Vec<_>, _>>()?;

    alternations_for_supplied_relative_opcodes(compiled_subexprs)
}

fn subexpression(subexpr: ast::SubExpression) -> Result<RelativeOpcodes, String> {
    let ast::SubExpression(items) = subexpr;

    items
        .into_iter()
        .map(|subexpr_item| match subexpr_item {
            ast::SubExpressionItem::Match(m) => match_item(m),
            ast::SubExpressionItem::Group(g) => group(g),
            ast::SubExpressionItem::Anchor(_) => todo!(),
            ast::SubExpressionItem::Backreference(_) => unimplemented!(),
        })
        .collect::<Result<Vec<_>, _>>()
        .map(|opcodes| opcodes.into_iter().flatten().collect())
}

macro_rules! generate_range_quantifier_block {
    (eager, $min:expr, $consumer:expr) => {
        $consumer
            .clone()
            .into_iter()
            .cycle()
            .take($consumer.len() * $min as usize)
            .into_iter()
            // jump past end of expression
            .chain(vec![RelativeOpcode::Split(1, (($consumer.len() + 2) as i32))].into_iter())
            .chain($consumer.clone().into_iter())
            // return to split
            .chain(vec![RelativeOpcode::Jmp(-($consumer.len() as i32) - 1)].into_iter())
            .collect()
    };

    (lazy, $min:expr, $consumer:expr) => {
        $consumer
            .clone()
            .into_iter()
            .cycle()
            .take($consumer.len() * $min as usize)
            .into_iter()
            // jump past end of expression
            .chain(vec![RelativeOpcode::Split((($consumer.len() + 2) as i32), 1)].into_iter())
            .chain($consumer.clone().into_iter())
            // return to split
            .chain(vec![RelativeOpcode::Jmp(-($consumer.len() as i32) - 1)].into_iter())
            .collect()
    };

    (eager, $min:expr, $max:expr, $consumer:expr) => {
        $consumer
            .clone()
            .into_iter()
            .cycle()
            .take($consumer.len() * $min as usize)
            .into_iter()
            .chain((0..($max - $min)).flat_map(|_| {
                vec![RelativeOpcode::Split(1, ($consumer.len() as i32) + 1)]
                    .into_iter()
                    .chain($consumer.clone().into_iter())
            }))
            .collect()
    };

    (lazy, $min:expr, $max:expr, $consumer:expr) => {
        $consumer
            .clone()
            .into_iter()
            .cycle()
            .take($consumer.len() * $min as usize)
            .into_iter()
            .chain((0..($max - $min)).flat_map(|_| {
                vec![RelativeOpcode::Split(($consumer.len() as i32) + 1, 1)]
                    .into_iter()
                    .chain($consumer.clone().into_iter())
            }))
            .collect()
    };
}

fn match_item(m: ast::Match) -> Result<RelativeOpcodes, String> {
    use ast::{
        Char, Integer, Match, MatchCharacter, MatchCharacterClass, MatchItem, Quantifier,
        QuantifierType,
    };

    match m {
        // Any character matchers
        Match::WithoutQuantifier {
            item: MatchItem::MatchAnyCharacter,
        } => Ok(vec![RelativeOpcode::Any]),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Eager(QuantifierType::ZeroOrOne),
        } => Ok(generate_range_quantifier_block!(
            eager,
            0,
            1,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Lazy(QuantifierType::ZeroOrOne),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            0,
            1,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Eager(QuantifierType::ZeroOrMore),
        } => Ok(generate_range_quantifier_block!(
            eager,
            0,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Lazy(QuantifierType::ZeroOrMore),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            0,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Eager(QuantifierType::OneOrMore),
        } => Ok(generate_range_quantifier_block!(
            eager,
            1,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Lazy(QuantifierType::OneOrMore),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            1,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(cnt))),
        } => Ok(vec![RelativeOpcode::Any; cnt as usize]),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Lazy(QuantifierType::MatchExactRange(Integer(cnt))),
        } => Ok(vec![RelativeOpcode::Any; cnt as usize]),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => Ok(generate_range_quantifier_block!(
            eager,
            cnt,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Lazy(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            cnt,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier:
                Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => Ok(generate_range_quantifier_block!(
            eager,
            lower,
            upper,
            vec![RelativeOpcode::Any]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier:
                Quantifier::Lazy(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            lower,
            upper,
            vec![RelativeOpcode::Any]
        )),

        // Character matchers
        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
        } => Ok(vec![RelativeOpcode::Consume(c)]),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Eager(QuantifierType::ZeroOrOne),
        } => Ok(generate_range_quantifier_block!(
            eager,
            0,
            1,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Lazy(QuantifierType::ZeroOrOne),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            0,
            1,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Eager(QuantifierType::ZeroOrMore),
        } => Ok(generate_range_quantifier_block!(
            eager,
            0,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Lazy(QuantifierType::ZeroOrMore),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            0,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Eager(QuantifierType::OneOrMore),
        } => Ok(generate_range_quantifier_block!(
            eager,
            1,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Lazy(QuantifierType::OneOrMore),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            1,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(cnt))),
        } => Ok(vec![RelativeOpcode::Consume(c); cnt as usize]),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Lazy(QuantifierType::MatchExactRange(Integer(cnt))),
        } => Ok(vec![RelativeOpcode::Consume(c); cnt as usize]),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => Ok(generate_range_quantifier_block!(
            eager,
            cnt,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Lazy(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            cnt,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier:
                Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => Ok(generate_range_quantifier_block!(
            eager,
            lower,
            upper,
            vec![RelativeOpcode::Consume(c)]
        )),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier:
                Quantifier::Lazy(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => Ok(generate_range_quantifier_block!(
            lazy,
            lower,
            upper,
            vec![RelativeOpcode::Consume(c)]
        )),

        // Character classes
        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
        } => character_class(cc),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Eager(QuantifierType::ZeroOrOne),
        } => character_class(cc)
            .map(|rel_ops| generate_range_quantifier_block!(eager, 0, 1, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Lazy(QuantifierType::ZeroOrOne),
        } => {
            character_class(cc).map(|rel_ops| generate_range_quantifier_block!(lazy, 0, 1, rel_ops))
        }
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Eager(QuantifierType::ZeroOrMore),
        } => character_class(cc).map(|rel_ops| generate_range_quantifier_block!(eager, 0, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Lazy(QuantifierType::ZeroOrMore),
        } => character_class(cc).map(|rel_ops| generate_range_quantifier_block!(lazy, 0, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Eager(QuantifierType::OneOrMore),
        } => character_class(cc).map(|rel_ops| generate_range_quantifier_block!(eager, 1, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Lazy(QuantifierType::OneOrMore),
        } => character_class(cc).map(|rel_ops| generate_range_quantifier_block!(lazy, 1, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(cnt))),
        } => character_class(cc).map(|rel_ops| {
            let multiple_of_len = rel_ops.len() * (cnt as usize);

            rel_ops.into_iter().cycle().take(multiple_of_len).collect()
        }),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Lazy(QuantifierType::MatchExactRange(Integer(cnt))),
        } => character_class(cc).map(|rel_ops| {
            let multiple_of_len = rel_ops.len() * (cnt as usize);

            rel_ops.into_iter().cycle().take(multiple_of_len).collect()
        }),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(lower))),
        } => character_class(cc)
            .map(|rel_ops| generate_range_quantifier_block!(eager, lower, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier: Quantifier::Lazy(QuantifierType::MatchAtLeastRange(Integer(lower))),
        } => character_class(cc)
            .map(|rel_ops| generate_range_quantifier_block!(lazy, lower, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier:
                Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => character_class(cc)
            .map(|rel_ops| generate_range_quantifier_block!(eager, lower, upper, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
            quantifier:
                Quantifier::Lazy(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => character_class(cc)
            .map(|rel_ops| generate_range_quantifier_block!(lazy, lower, upper, rel_ops)),

        // Character groups
        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
        } => character_group(cg),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Eager(QuantifierType::ZeroOrOne),
        } => character_group(cg)
            .map(|rel_ops| generate_range_quantifier_block!(eager, 0, 1, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Lazy(QuantifierType::ZeroOrOne),
        } => {
            character_group(cg).map(|rel_ops| generate_range_quantifier_block!(lazy, 0, 1, rel_ops))
        }
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Eager(QuantifierType::ZeroOrMore),
        } => character_group(cg).map(|rel_ops| generate_range_quantifier_block!(eager, 0, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Lazy(QuantifierType::ZeroOrMore),
        } => character_group(cg).map(|rel_ops| generate_range_quantifier_block!(lazy, 0, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Eager(QuantifierType::OneOrMore),
        } => character_group(cg).map(|rel_ops| generate_range_quantifier_block!(eager, 1, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Lazy(QuantifierType::OneOrMore),
        } => character_group(cg).map(|rel_ops| generate_range_quantifier_block!(lazy, 1, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(cnt))),
        } => character_group(cg).map(|rel_ops| {
            let multiple_of_len = rel_ops.len() * (cnt as usize);

            rel_ops.into_iter().cycle().take(multiple_of_len).collect()
        }),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Lazy(QuantifierType::MatchExactRange(Integer(cnt))),
        } => character_group(cg).map(|rel_ops| {
            let multiple_of_len = rel_ops.len() * (cnt as usize);

            rel_ops.into_iter().cycle().take(multiple_of_len).collect()
        }),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(lower))),
        } => character_group(cg)
            .map(|rel_ops| generate_range_quantifier_block!(eager, lower, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier: Quantifier::Lazy(QuantifierType::MatchAtLeastRange(Integer(lower))),
        } => character_group(cg)
            .map(|rel_ops| generate_range_quantifier_block!(lazy, lower, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier:
                Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => character_group(cg)
            .map(|rel_ops| generate_range_quantifier_block!(eager, lower, upper, rel_ops)),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(cg)),
            quantifier:
                Quantifier::Lazy(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => character_group(cg)
            .map(|rel_ops| generate_range_quantifier_block!(lazy, lower, upper, rel_ops)),

        // Unicode categories
        Match::WithQuantifier {
            item:
                MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClassFromUnicodeCategory(
                    _,
                )),
            quantifier: _,
        } => unimplemented!(),
        Match::WithoutQuantifier {
            item:
                MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClassFromUnicodeCategory(
                    _,
                )),
        } => unimplemented!(),
    }
}

fn character_group(cg: ast::CharacterGroup) -> Result<RelativeOpcodes, String> {
    let sets: Vec<RelativeOpcodes> = match cg {
        ast::CharacterGroup::NegatedItems(cgis) => cgis
            .into_iter()
            .map(character_group_item_to_set)
            .map(|set| set.invert_membership())
            .map(|set| vec![RelativeOpcode::ConsumeSet(set)])
            .collect(),

        ast::CharacterGroup::Items(cgis) => cgis
            .into_iter()
            .map(character_group_item_to_set)
            .map(|set| vec![RelativeOpcode::ConsumeSet(set)])
            .collect(),
    };

    alternations_for_supplied_relative_opcodes(sets)
}

fn character_group_item_to_set(cgi: ast::CharacterGroupItem) -> CharacterSet {
    use ast::Char;

    match cgi {
        ast::CharacterGroupItem::CharacterClassFromUnicodeCategory(_) => unimplemented!(),
        ast::CharacterGroupItem::CharacterClass(cc) => character_class_to_set(cc),
        ast::CharacterGroupItem::CharacterRange(Char(lower), Char(upper)) => {
            let alphabet = CharacterAlphabet::Range(lower..=upper);
            CharacterSet::inclusive(alphabet)
        }
        ast::CharacterGroupItem::Char(Char(c)) => {
            CharacterSet::inclusive(CharacterAlphabet::Explicit(vec![c]))
        }
    }
}

// character classes

/// A representation of a AnyWordClass character class, in character set format.
pub struct AnyWordClass;

impl AnyWordClass {
    const RANGES: [std::ops::RangeInclusive<char>; 4] =
        ['a'..='z', 'A'..='Z', '0'..='9', '_'..='_'];
}

impl CharacterSetRepresentable for AnyWordClass {}

impl From<AnyWordClass> for CharacterSet {
    fn from(_: AnyWordClass) -> Self {
        CharacterSet::inclusive(CharacterAlphabet::Ranges(AnyWordClass::RANGES.to_vec()))
    }
}

/// A representation of a AnyWordClassInverted character class, in character
/// set format.
pub struct AnyWordClassInverted;

impl CharacterSetRepresentable for AnyWordClassInverted {}

impl From<AnyWordClassInverted> for CharacterSet {
    fn from(_: AnyWordClassInverted) -> Self {
        CharacterSet::exclusive(CharacterAlphabet::Ranges(AnyWordClass::RANGES.to_vec()))
    }
}

/// A representation of a AnyDecimalDigitClass character class, in character
/// set format.
pub struct AnyDecimalDigitClass;

impl AnyDecimalDigitClass {
    const RANGE: std::ops::RangeInclusive<char> = '0'..='9';
}

impl CharacterSetRepresentable for AnyDecimalDigitClass {}

impl From<AnyDecimalDigitClass> for CharacterSet {
    fn from(_: AnyDecimalDigitClass) -> Self {
        CharacterSet::inclusive(CharacterAlphabet::Range(AnyDecimalDigitClass::RANGE))
    }
}

/// A representation of a AnyDecimalDigitClassInverted character class, in
/// character set format.
pub struct AnyDecimalDigitClassInverted;

impl CharacterSetRepresentable for AnyDecimalDigitClassInverted {}

impl From<AnyDecimalDigitClassInverted> for CharacterSet {
    fn from(_: AnyDecimalDigitClassInverted) -> Self {
        CharacterSet::exclusive(CharacterAlphabet::Range(AnyDecimalDigitClass::RANGE))
    }
}

fn character_class(cc: ast::CharacterClass) -> Result<RelativeOpcodes, String> {
    let set = character_class_to_set(cc);

    Ok(vec![RelativeOpcode::ConsumeSet(set)])
}

fn character_class_to_set(cc: ast::CharacterClass) -> CharacterSet {
    match cc {
        ast::CharacterClass::AnyWord => AnyWordClass.into(),
        ast::CharacterClass::AnyWordInverted => AnyWordClassInverted.into(),

        ast::CharacterClass::AnyDecimalDigit => AnyDecimalDigitClass.into(),
        ast::CharacterClass::AnyDecimalDigitInverted => AnyDecimalDigitClassInverted.into(),
    }
}

/// Generates alternations from a block of relative operations.
fn alternations_for_supplied_relative_opcodes(
    rel_ops: Vec<RelativeOpcodes>,
) -> Result<RelativeOpcodes, String> {
    let subexpr_cnt = rel_ops.len();

    let length_of_rel_ops: Vec<_> = rel_ops
        .iter()
        .enumerate()
        .map(|(idx, subexpr)| ((idx + 1 == subexpr_cnt), subexpr))
        .map(|(is_last, subexpr)| {
            // last alternation doesn't require a split prefix and jump suffix
            if is_last {
                subexpr.len()
            } else {
                subexpr.len() + 2
            }
        })
        .collect();

    let total_length_of_compiled_expr: usize = length_of_rel_ops.iter().sum();
    let start_end_offsets_by_subexpr: Vec<(usize, usize)> = length_of_rel_ops
        .iter()
        .fold(
            // add 1 to set end at first instruction of next expr
            (total_length_of_compiled_expr + 1, vec![]),
            |(offset_to_end, mut acc), &subexpr_len| {
                let new_offset_to_end = offset_to_end - subexpr_len;

                acc.push((subexpr_len, new_offset_to_end));
                (new_offset_to_end, acc)
            },
        )
        .1;

    let compiled_ops_with_applied_alternations = rel_ops
        .into_iter()
        .zip(start_end_offsets_by_subexpr.into_iter())
        .enumerate()
        .map(|(idx, (opcodes, (start, end)))| (idx as u32, opcodes, (start as i32, end as i32)))
        .map(|(idx, opcodes, start_end_offsets)| {
            let optional_next_offsets =
                ((idx + 1) != subexpr_cnt as u32).then(|| start_end_offsets);
            (optional_next_offsets, opcodes)
        })
        .flat_map(|(start_of_next, ops)| match start_of_next {
            Some((start_of_next_subexpr_offset, end_of_expr_offset)) => {
                [RelativeOpcode::Split(1, start_of_next_subexpr_offset)]
                    .into_iter()
                    .chain(ops.into_iter())
                    .chain([RelativeOpcode::Jmp(end_of_expr_offset)].into_iter())
                    .collect()
            }
            None => ops,
        })
        .collect();

    Ok(compiled_ops_with_applied_alternations)
}

// Groups

fn group(g: ast::Group) -> Result<RelativeOpcodes, String> {
    match g {
        ast::Group::Capturing { expression: expr } => {
            let save_group_id = SAVE_GROUP_ID.fetch_add(1, Ordering::SeqCst);
            let save_group_prefix = [RelativeOpcode::StartSave(save_group_id)];
            let save_group_suffix = [
                RelativeOpcode::EndSave(save_group_id),
                RelativeOpcode::Match,
            ];

            expression(expr).map(|insts| {
                save_group_prefix
                    .into_iter()
                    .chain(insts.into_iter())
                    .chain(save_group_suffix.into_iter())
                    .collect()
            })
        }
        ast::Group::CapturingWithQuantifier {
            expression: _,
            quantifier: _,
        } => todo!(),
        ast::Group::NonCapturing { expression: expr } => expression(expr).map(|insts| {
            insts
                .into_iter()
                .chain([RelativeOpcode::Match].into_iter())
                .collect()
        }),
        ast::Group::NonCapturingWithQuantifier {
            expression: _,
            quantifier: _,
        } => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;

    #[test]
    fn should_compile_unanchored_character_match() {
        // approximate to `ab`
        let regex_ast = Regex::Unanchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
            }),
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('b'))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('b')),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_anchored_character_match() {
        // approximate to `^ab`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
            }),
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('b'))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('b')),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_alternation() {
        // approximate to `^a|b`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![
            SubExpression(vec![SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
            })]),
            SubExpression(vec![SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('b'))),
            })]),
        ]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(3))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Jmp(InstJmp::new(InstIndex::from(4))),
                Opcode::Consume(InstConsume::new('b')),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_any_character_match() {
        // approximate to `.`
        let regex_ast = Regex::Unanchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchAnyCharacter,
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                Opcode::Any,
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_zero_or_more_quantified_item() {
        // approximate to `^.*`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::ZeroOrMore),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(3))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                Opcode::Match,
            ])),
            compile(regex_ast)
        );

        // approximate to `^a*`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::ZeroOrMore),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(3))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_one_or_more_quantified_item() {
        // approximate to `^.+`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::OneOrMore),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(4))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(1))),
                Opcode::Match,
            ])),
            compile(regex_ast)
        );

        // approximate to `^a+`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::OneOrMore),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(4))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Jmp(InstJmp::new(InstIndex::from(1))),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_match_zero_or_one_item() {
        // approximate to `^.?`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::ZeroOrOne),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(2))),
                Opcode::Any,
                Opcode::Match,
            ])),
            compile(regex_ast)
        );

        // approximate to `^a?`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::ZeroOrOne),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(2))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_exact_match_quantified_item() {
        // approximate to `^.{2}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(2))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default()
                .with_opcodes(vec![Opcode::Any, Opcode::Any, Opcode::Match,])),
            compile(regex_ast)
        );

        // approximate to `^a{2}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(2))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_match_at_least_quantified_item() {
        // approximate to `^.{2,}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(2))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Any,
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(5))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(2))),
                Opcode::Match
            ])),
            compile(regex_ast)
        );

        // approximate to `^a{2,}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(2))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(5))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Jmp(InstJmp::new(InstIndex::from(2))),
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_match_between_quantified_item() {
        // approximate to `^.{2,4}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(2),
                    upper_bound: Integer(4),
                }),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Any,
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(4))),
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(5), InstIndex::from(6))),
                Opcode::Any,
                Opcode::Match
            ])),
            compile(regex_ast)
        );

        // approximate to `^a{2,4}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(2),
                    upper_bound: Integer(4),
                }),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(4))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(5), InstIndex::from(6))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_character_classes() {
        // approximate to `^\w`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(
                    CharacterClass::AnyWord,
                )),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default()
                .with_sets(vec![CharacterSet::inclusive(CharacterAlphabet::Ranges(
                    vec!['a'..='z', 'A'..='Z', '0'..='9', '_'..='_',]
                ))])
                .with_opcodes(vec![
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Match,
                ])),
            compile(regex_ast)
        );

        // approximate to `^\d`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(
                    CharacterClass::AnyDecimalDigit,
                )),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default()
                .with_sets(vec![CharacterSet::inclusive(CharacterAlphabet::Range(
                    '0'..='9'
                ))])
                .with_opcodes(vec![
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Match,
                ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_character_classes_with_quantifiers() {
        let quantifier_and_expected_opcodes = vec![
            // approximate to `^\d?`
            (
                Quantifier::Eager(QuantifierType::ZeroOrOne),
                vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(2))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Match,
                ],
            ),
            // approximate to `^\d??`
            (
                Quantifier::Lazy(QuantifierType::ZeroOrOne),
                vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(1))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Match,
                ],
            ),
            // approximate to `^\d*`
            (
                Quantifier::Eager(QuantifierType::ZeroOrMore),
                vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(3))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                    Opcode::Match,
                ],
            ),
            // approximate to `^\d*?`
            (
                Quantifier::Lazy(QuantifierType::ZeroOrMore),
                vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                    Opcode::Match,
                ],
            ),
            // approximate to `^\d+`
            (
                Quantifier::Eager(QuantifierType::OneOrMore),
                vec![
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(4))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(1))),
                    Opcode::Match,
                ],
            ),
            // approximate to `^\d+?`
            (
                Quantifier::Lazy(QuantifierType::OneOrMore),
                vec![
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Split(InstSplit::new(InstIndex::from(4), InstIndex::from(2))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(1))),
                    Opcode::Match,
                ],
            ),
        ];

        for (id, (quantifier, expected_opcodes)) in
            quantifier_and_expected_opcodes.into_iter().enumerate()
        {
            let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
                SubExpressionItem::Match(Match::WithQuantifier {
                    item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(
                        CharacterClass::AnyDecimalDigit,
                    )),
                    quantifier,
                }),
            ])]));

            let res = compile(regex_ast);
            assert_eq!(
                (
                    id,
                    Ok(Instructions::default()
                        .with_sets(vec![CharacterSet::inclusive(CharacterAlphabet::Range(
                            '0'..='9'
                        ))])
                        .with_opcodes(expected_opcodes))
                ),
                (id, res)
            );
        }
    }

    #[test]
    fn should_compile_single_character_character_group() {
        // approximate to `^[a]`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(
                    CharacterGroup::Items(vec![CharacterGroupItem::Char(Char('a'))]),
                )),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default()
                .with_sets(vec![CharacterSet::inclusive(CharacterAlphabet::Explicit(
                    vec!['a']
                ))])
                .with_opcodes(vec![
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Match
                ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_compound_character_group() {
        // approximate to `^[az]`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(
                    CharacterGroup::Items(vec![
                        CharacterGroupItem::Char(Char('a')),
                        CharacterGroupItem::Char(Char('z')),
                    ]),
                )),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default()
                .with_sets(vec![
                    CharacterSet::inclusive(CharacterAlphabet::Explicit(vec!['a'],)),
                    CharacterSet::inclusive(CharacterAlphabet::Explicit(vec!['z'],))
                ])
                .with_opcodes(vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(3))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(4))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(1)),
                    Opcode::Match,
                ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_character_group_range() {
        // approximate to `^[0-9]`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(
                    CharacterGroup::Items(vec![CharacterGroupItem::CharacterRange(
                        Char('0'),
                        Char('9'),
                    )]),
                )),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default()
                .with_sets(vec![CharacterSet::inclusive(CharacterAlphabet::Range(
                    '0'..='9'
                )),])
                .with_opcodes(vec![
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Match,
                ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_character_groups_with_quantifiers() {
        let quantifier_and_expected_opcodes = vec![
            // approximate to `^[0-9]?`
            (
                Quantifier::Eager(QuantifierType::ZeroOrOne),
                vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(2))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Match,
                ],
            ),
            // approximate to `^[0-9]??`
            (
                Quantifier::Lazy(QuantifierType::ZeroOrOne),
                vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(1))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Match,
                ],
            ),
            // approximate to `^[0-9]*`
            (
                Quantifier::Eager(QuantifierType::ZeroOrMore),
                vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(1), InstIndex::from(3))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                    Opcode::Match,
                ],
            ),
            // approximate to `^[0-9]*?`
            (
                Quantifier::Lazy(QuantifierType::ZeroOrMore),
                vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                    Opcode::Match,
                ],
            ),
            // approximate to `^[0-9]+`
            (
                Quantifier::Eager(QuantifierType::OneOrMore),
                vec![
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(4))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(1))),
                    Opcode::Match,
                ],
            ),
            // approximate to `^[0-9]+?`
            (
                Quantifier::Lazy(QuantifierType::OneOrMore),
                vec![
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Split(InstSplit::new(InstIndex::from(4), InstIndex::from(2))),
                    Opcode::ConsumeSet(InstConsumeSet::member_of(0)),
                    Opcode::Jmp(InstJmp::new(InstIndex::from(1))),
                    Opcode::Match,
                ],
            ),
        ];

        for (id, (quantifier, expected_opcodes)) in
            quantifier_and_expected_opcodes.into_iter().enumerate()
        {
            let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
                SubExpressionItem::Match(Match::WithQuantifier {
                    item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterGroup(
                        CharacterGroup::Items(vec![CharacterGroupItem::CharacterRange(
                            Char('0'),
                            Char('9'),
                        )]),
                    )),
                    quantifier,
                }),
            ])]));

            let res = compile(regex_ast);
            assert_eq!(
                (
                    id,
                    Ok(Instructions::default()
                        .with_sets(vec![CharacterSet::inclusive(CharacterAlphabet::Range(
                            '0'..='9'
                        ))])
                        .with_opcodes(expected_opcodes))
                ),
                (id, res)
            );
        }
    }

    #[test]
    fn should_compile_capturing_group() {
        // reset save group id.
        SAVE_GROUP_ID.store(0, std::sync::atomic::Ordering::SeqCst);

        // approximate to `^(a)(b)`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Group(Group::Capturing {
                expression: Expression(vec![SubExpression(vec![SubExpressionItem::Match(
                    Match::WithoutQuantifier {
                        item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                    },
                )])]),
            }),
            SubExpressionItem::Group(Group::Capturing {
                expression: Expression(vec![SubExpression(vec![SubExpressionItem::Match(
                    Match::WithoutQuantifier {
                        item: MatchItem::MatchCharacter(MatchCharacter(Char('b'))),
                    },
                )])]),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::StartSave(InstStartSave::new(0)),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::EndSave(InstEndSave::new(0)),
                Opcode::Match,
                Opcode::StartSave(InstStartSave::new(1)),
                Opcode::Consume(InstConsume::new('b')),
                Opcode::EndSave(InstEndSave::new(1)),
                Opcode::Match,
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_nested_capturing_group() {
        // reset save group id.
        SAVE_GROUP_ID.store(0, std::sync::atomic::Ordering::SeqCst);

        // approximate to `^(a(b))`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Group(Group::Capturing {
                expression: Expression(vec![SubExpression(vec![
                    SubExpressionItem::Match(Match::WithoutQuantifier {
                        item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                    }),
                    SubExpressionItem::Group(Group::Capturing {
                        expression: Expression(vec![SubExpression(vec![
                            SubExpressionItem::Match(Match::WithoutQuantifier {
                                item: MatchItem::MatchCharacter(MatchCharacter(Char('b'))),
                            }),
                        ])]),
                    }),
                ])]),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::StartSave(InstStartSave::new(0)),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::StartSave(InstStartSave::new(1)),
                Opcode::Consume(InstConsume::new('b')),
                Opcode::EndSave(InstEndSave::new(1)),
                Opcode::Match,
                Opcode::EndSave(InstEndSave::new(0)),
                Opcode::Match,
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_non_capturing_group() {
        // approximate to `^(?:a)`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Group(Group::NonCapturing {
                expression: Expression(vec![SubExpression(vec![SubExpressionItem::Match(
                    Match::WithoutQuantifier {
                        item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                    },
                )])]),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::default().with_opcodes(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Match,
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }
}

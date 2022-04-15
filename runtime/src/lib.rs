use collections_ext::set::sparse::SparseSet;
use std::fmt::{Debug, Display};

/// Represents a defined match group for a pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SaveGroupSlot {
    None,
    Complete {
        slot_id: usize,
        start: usize,
        end: usize,
    },
}

impl SaveGroupSlot {
    /// returns a completed
    pub const fn complete(slot_id: usize, start: usize, end: usize) -> Self {
        Self::Complete {
            slot_id,
            start,
            end,
        }
    }
}

impl From<SaveGroup> for SaveGroupSlot {
    fn from(src: SaveGroup) -> Self {
        match src {
            SaveGroup::None => SaveGroupSlot::None,

            SaveGroup::Allocated { .. } => SaveGroupSlot::None,
            SaveGroup::Open { .. } => SaveGroupSlot::None,
            SaveGroup::Complete {
                slot_id,
                start,
                end,
            } => SaveGroupSlot::Complete {
                slot_id,
                start,
                end,
            },
        }
    }
}

impl Default for SaveGroupSlot {
    fn default() -> Self {
        SaveGroupSlot::None
    }
}

/// Represents a Save Group as tracked on an open thread
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SaveGroup {
    None,
    Allocated {
        slot_id: usize,
    },
    Open {
        slot_id: usize,
        start: usize,
    },
    Complete {
        slot_id: usize,
        start: usize,
        end: usize,
    },
}

impl SaveGroup {
    pub fn allocated(slot_id: usize) -> Self {
        Self::Allocated { slot_id }
    }

    pub fn open(slot_id: usize, start: usize) -> Self {
        Self::Open { slot_id, start }
    }

    pub fn complete(slot_id: usize, start: usize, end: usize) -> Self {
        Self::Complete {
            slot_id,
            start,
            end,
        }
    }
}

#[derive(Debug)]
pub struct Thread {
    save_group: SaveGroup,
    inst: InstIndex,
}

impl Thread {
    pub fn new(save_group: SaveGroup, inst: InstIndex) -> Self {
        Self { save_group, inst }
    }
}

#[derive(Debug)]
pub struct Threads {
    gen: SparseSet,
    threads: Vec<Thread>,
}

impl Threads {
    pub fn with_set_size(set_capacity: usize) -> Self {
        let ops = SparseSet::new(set_capacity);
        Self {
            threads: vec![],
            gen: ops,
        }
    }
}

impl Default for Threads {
    fn default() -> Self {
        let ops = SparseSet::new(0);
        Self {
            threads: vec![],
            gen: ops,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Instructions {
    program: Vec<Instruction>,
}

impl Instructions {
    #[must_use]
    pub fn new(program: Vec<Opcode>) -> Self {
        Self {
            program: program
                .into_iter()
                .enumerate()
                .map(|(id, opcode)| Instruction::new(id, opcode))
                .collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.program.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for inst in self.program.iter() {
            writeln!(f, "{:04}: {}", inst.id, inst.opcode)?
        }

        Ok(())
    }
}

impl std::ops::Index<InstIndex> for Instructions {
    type Output = Opcode;

    fn index(&self, index: InstIndex) -> &Self::Output {
        let idx = index.as_usize();
        &self.program[idx].opcode
    }
}

impl std::ops::IndexMut<InstIndex> for Instructions {
    fn index_mut(&mut self, index: InstIndex) -> &mut Self::Output {
        let idx = index.as_usize();
        &mut self.program[idx].opcode
    }
}

impl AsRef<[Instruction]> for Instructions {
    fn as_ref(&self) -> &[Instruction] {
        &self.program
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InstIndex(usize);

impl InstIndex {
    #[inline]
    fn as_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for InstIndex {
    fn from(ptr: usize) -> Self {
        Self(ptr)
    }
}

impl std::ops::Add<usize> for InstIndex {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        let new_ptr = self.0 + rhs;

        InstIndex::from(new_ptr)
    }
}

impl std::ops::Add<Self> for InstIndex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let new_ptr = self.0 + rhs.0;

        InstIndex::from(new_ptr)
    }
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
    id: usize,
    opcode: Opcode,
}

impl Instruction {
    #[must_use]
    pub fn new(id: usize, opcode: Opcode) -> Self {
        Self { id, opcode }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04}: {}", self.id, self.opcode)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Any,
    Consume(InstConsume),
    Split(InstSplit),
    Jmp(InstJmp),
    StartSave(InstStartSave),
    EndSave(InstEndSave),
    Match,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Match => std::fmt::Display::fmt(&InstMatch, f),
            Opcode::Consume(i) => std::fmt::Display::fmt(&i, f),
            Opcode::Split(i) => std::fmt::Display::fmt(&i, f),
            Opcode::Any => std::fmt::Display::fmt(&InstAny::new(), f),
            Opcode::Jmp(i) => std::fmt::Display::fmt(&i, f),
            Opcode::StartSave(i) => std::fmt::Display::fmt(&i, f),
            Opcode::EndSave(i) => std::fmt::Display::fmt(&i, f),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct InstMatch;

impl Display for InstMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Match: (END)",)
    }
}

#[derive(Debug, PartialEq)]
pub struct InstAny;

impl InstAny {
    pub const fn new() -> Self {
        Self
    }
}

impl Default for InstAny {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for InstAny {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Any")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstConsume {
    value: char,
}

impl InstConsume {
    #[must_use]
    pub fn new(value: char) -> Self {
        Self { value }
    }
}

impl Display for InstConsume {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Consume: {:?}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstSplit {
    x_branch: InstIndex,
    y_branch: InstIndex,
}

impl InstSplit {
    #[must_use]
    pub fn new(x: InstIndex, y: InstIndex) -> Self {
        Self {
            x_branch: x,
            y_branch: y,
        }
    }
}

impl Display for InstSplit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Split: ({:04}), ({:04})",
            self.x_branch.as_usize(),
            self.y_branch.as_usize()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstJmp {
    next: InstIndex,
}

impl InstJmp {
    pub fn new(next: InstIndex) -> Self {
        Self { next }
    }
}

impl Display for InstJmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "JumpAbs: ({:04})", self.next.as_usize())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstStartSave {
    slot_id: usize,
}

impl InstStartSave {
    #[must_use]
    pub fn new(slot_id: usize) -> Self {
        Self { slot_id }
    }
}

impl Display for InstStartSave {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "StartSave[{:04}]", self.slot_id,)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstEndSave {
    slot_id: usize,
}

impl InstEndSave {
    #[must_use]
    pub fn new(slot_id: usize) -> Self {
        Self { slot_id }
    }
}

impl Display for InstEndSave {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EndSave[{:04}]", self.slot_id,)
    }
}

fn get_at(input: &str, idx: usize) -> Option<char> {
    input[idx..].chars().next()
}

fn add_thread(
    program: &[Instruction],
    save_groups: &mut Vec<SaveGroupSlot>,
    mut thread_list: Threads,
    t: Thread,
    sp: usize,
    input: &str,
) -> Threads {
    let inst_idx = t.inst;
    let default_next_inst_idx = inst_idx + 1;

    // Don't visit states we've already added.
    let inst = match program.get(inst_idx.as_usize()) {
        // if the thread is already defined, return.
        Some(inst) if thread_list.gen.contains(&inst.id) => return thread_list,
        // if it's the end of the program without a match instruction, return.
        None => return thread_list,
        // Otherwise add the new thread.
        Some(inst) => {
            thread_list.gen.insert(inst.id);
            inst
        }
    };

    let opcode = &inst.opcode;
    match opcode {
        Opcode::Split(InstSplit { x_branch, y_branch }) => {
            let x = *x_branch;
            let y = *y_branch;
            thread_list = add_thread(
                program,
                save_groups,
                thread_list,
                Thread::new(t.save_group, x),
                sp,
                input,
            );

            add_thread(
                program,
                save_groups,
                thread_list,
                Thread::new(t.save_group, y),
                sp,
                input,
            )
        }
        Opcode::Jmp(InstJmp { next }) => add_thread(
            program,
            save_groups,
            thread_list,
            Thread::new(t.save_group, *next),
            sp,
            input,
        ),
        Opcode::StartSave(InstStartSave { slot_id }) => {
            let save_group = SaveGroup::Allocated { slot_id: *slot_id };

            add_thread(
                program,
                save_groups,
                thread_list,
                Thread::new(save_group, default_next_inst_idx),
                sp,
                input,
            )
        }
        Opcode::EndSave(InstEndSave { slot_id }) => {
            let closed_save = match t.save_group {
                SaveGroup::Open { slot_id, start } => SaveGroup::Complete {
                    slot_id,
                    start,
                    end: sp,
                },

                // this state should never be reached.
                _ => panic!("attempting to close an unopened save."),
            };
            let next = inst_idx + 1;

            save_groups[*slot_id] = SaveGroupSlot::from(closed_save);
            add_thread(
                program,
                save_groups,
                thread_list,
                Thread::new(closed_save, next),
                sp,
                input,
            )
        }
        _ => {
            thread_list.threads.push(t);
            thread_list
        }
    }
}

pub fn run<const SG: usize>(program: &[Instruction], input: &str) -> Option<Vec<SaveGroupSlot>> {
    use core::mem::swap;

    let input_len = input.len();
    let program_len = program.len();

    let mut input_idx = 0;
    let mut current_thread_list = Threads::with_set_size(program_len);
    let mut next_thread_list = Threads::with_set_size(program_len);
    // a running tracker of found matches
    let mut matches = 0;

    let mut sub = vec![SaveGroupSlot::None; SG];

    current_thread_list = add_thread(
        program,
        &mut sub,
        current_thread_list,
        Thread::new(SaveGroup::None, InstIndex::from(0)),
        input_idx,
        input,
    );

    'outer: while input_idx <= input_len {
        for thread in current_thread_list.threads.iter() {
            let save_group = thread.save_group;
            let next_char = get_at(input, input_idx);
            let inst_idx = thread.inst;
            let default_next_inst_idx = inst_idx + 1;
            let opcode = program.get(inst_idx.as_usize()).map(|i| &i.opcode);

            match opcode {
                Some(Opcode::Any) if next_char.is_none() => {
                    break;
                }
                Some(Opcode::Any) => {
                    let thread_local_save_group =
                        if let SaveGroup::Allocated { slot_id } = save_group {
                            SaveGroup::open(slot_id, input_idx)
                        } else {
                            save_group
                        };

                    next_thread_list = add_thread(
                        program,
                        &mut sub,
                        next_thread_list,
                        Thread::new(thread_local_save_group, default_next_inst_idx),
                        input_idx + 1,
                        input,
                    );
                }
                Some(Opcode::Consume(InstConsume { value })) if Some(*value) == next_char => {
                    let thread_local_save_group =
                        if let SaveGroup::Allocated { slot_id } = save_group {
                            SaveGroup::open(slot_id, input_idx)
                        } else {
                            save_group
                        };

                    next_thread_list = add_thread(
                        program,
                        &mut sub,
                        next_thread_list,
                        Thread::new(thread_local_save_group, default_next_inst_idx),
                        input_idx + 1,
                        input,
                    );
                }
                // next value doesn't match
                Some(Opcode::Consume(_)) => {
                    continue;
                }
                Some(Opcode::Match) => {
                    matches += 1;
                    continue;
                }
                None => {
                    break 'outer;
                }
                _ => continue,
            }
        }

        input_idx += 1;
        swap(&mut current_thread_list, &mut next_thread_list);
        next_thread_list.threads.clear();
        next_thread_list.gen.clear();

        if current_thread_list.threads.is_empty() {
            break 'outer;
        }
    }

    if matches > 0 {
        Some(sub)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_evaluate_simple_linear_match_expression() {
        let progs = vec![
            (
                Some(vec![SaveGroupSlot::complete(0, 0, 1)]),
                Instructions::new(vec![
                    Opcode::StartSave(InstStartSave::new(0)),
                    Opcode::Consume(InstConsume::new('a')),
                    Opcode::EndSave(InstEndSave::new(0)),
                    Opcode::Match,
                ]),
            ),
            (
                None,
                Instructions::new(vec![
                    Opcode::StartSave(InstStartSave::new(0)),
                    Opcode::Consume(InstConsume::new('b')),
                    Opcode::EndSave(InstEndSave::new(0)),
                    Opcode::Match,
                ]),
            ),
        ];

        let input = "aab";

        for (expected_res, prog) in progs {
            let res = run::<1>(&prog.program, input);
            assert_eq!(expected_res, res)
        }
    }

    #[test]
    fn should_evaluate_consecutive_diverging_match_expression() {
        let progs = vec![
            (
                vec![SaveGroupSlot::complete(0, 0, 2)],
                Instructions::new(vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                    Opcode::Any,
                    Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                    Opcode::StartSave(InstStartSave::new(0)),
                    Opcode::Consume(InstConsume::new('a')),
                    Opcode::Consume(InstConsume::new('a')),
                    Opcode::EndSave(InstEndSave::new(0)),
                    Opcode::Match,
                ]),
            ),
            (
                vec![SaveGroupSlot::complete(0, 1, 3)],
                Instructions::new(vec![
                    Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                    Opcode::Any,
                    Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                    Opcode::StartSave(InstStartSave::new(0)),
                    Opcode::Consume(InstConsume::new('a')),
                    Opcode::Consume(InstConsume::new('b')),
                    Opcode::EndSave(InstEndSave::new(0)),
                    Opcode::Match,
                ]),
            ),
        ];

        let input = "aab";

        for (test_num, (expected_res, prog)) in progs.into_iter().enumerate() {
            let res = run::<1>(&prog.program, input);
            assert_eq!((test_num, Some(expected_res)), (test_num, res))
        }
    }

    #[test]
    fn should_evaluate_multiple_save_groups_expression() {
        let (expected_res, prog) = (
            vec![
                SaveGroupSlot::complete(0, 0, 2),
                SaveGroupSlot::complete(1, 1, 3),
            ],
            Instructions::new(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                Opcode::Split(InstSplit::new(InstIndex::from(9), InstIndex::from(4))),
                Opcode::StartSave(InstStartSave::new(0)),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::EndSave(InstEndSave::new(0)),
                Opcode::Match,
                Opcode::StartSave(InstStartSave::new(1)),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('b')),
                Opcode::EndSave(InstEndSave::new(1)),
                Opcode::Match,
            ]),
        );

        let input = "aab";

        let res = run::<2>(&prog.program, input);
        assert_eq!(Some(expected_res), res)
    }

    #[test]
    fn should_evaluate_eager_match_exact_quantifier_expression() {
        let tests = vec![
            (vec![SaveGroupSlot::complete(0, 0, 2)], "aab"),
            (vec![SaveGroupSlot::complete(0, 0, 2)], "aaab"),
        ];

        let prog = Instructions::new(vec![
            Opcode::StartSave(InstStartSave::new(0)),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::EndSave(InstEndSave::new(0)),
            Opcode::Match,
        ]);

        for (case_id, (expected_res, input)) in tests.into_iter().enumerate() {
            let res = run::<1>(&prog.program, input);
            assert_eq!((case_id, Some(expected_res)), (case_id, res));
        }
    }

    #[test]
    fn should_evaluate_eager_match_atleast_quantifier_expression() {
        let tests = vec![
            (vec![SaveGroupSlot::complete(0, 0, 2)], "aab"),
            (vec![SaveGroupSlot::complete(0, 0, 3)], "aaab"),
        ];

        let prog = Instructions::new(vec![
            Opcode::StartSave(InstStartSave::new(0)),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::Split(InstSplit::new(InstIndex::from(4), InstIndex::from(6))),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::Jmp(InstJmp::new(InstIndex::from(3))),
            Opcode::EndSave(InstEndSave::new(0)),
            Opcode::Match,
        ]);

        for (case_id, (expected_res, input)) in tests.into_iter().enumerate() {
            let res = run::<1>(&prog.program, input);
            assert_eq!((case_id, Some(expected_res)), (case_id, res));
        }
    }

    #[test]
    fn should_evaluate_eager_match_between_quantifier_expression() {
        let tests = vec![
            (vec![SaveGroupSlot::complete(0, 0, 2)], "aab"),
            (vec![SaveGroupSlot::complete(0, 0, 3)], "aaab"),
            (vec![SaveGroupSlot::complete(0, 0, 4)], "aaaab"),
        ];

        let prog = Instructions::new(vec![
            Opcode::StartSave(InstStartSave::new(0)),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(4))),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::Split(InstSplit::new(InstIndex::from(5), InstIndex::from(6))),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::Consume(InstConsume::new('a')),
            Opcode::EndSave(InstEndSave::new(0)),
            Opcode::Match,
        ]);

        for (case_id, (expected_res, input)) in tests.into_iter().enumerate() {
            let res = run::<1>(&prog.program, input);
            assert_eq!((case_id, Some(expected_res)), (case_id, res));
        }
    }

    #[test]
    fn should_print_test_instructions() {
        let prog = Instructions::new(vec![
            Opcode::Consume(InstConsume::new('a')),
            Opcode::Consume(InstConsume::new('b')),
            Opcode::Match,
        ]);

        assert_eq!(
            "0000: Consume: 'a'
0001: Consume: 'b'
0002: Match: (END)\n",
            prog.to_string()
        )
    }
}

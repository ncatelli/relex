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
    pub fn complete(slot_id: usize, start: usize, end: usize) -> Self {
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

            SaveGroup::Allocated { slot_id } => SaveGroupSlot::None,
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
    ops: SparseSet,
    threads: Vec<Thread>,
}

impl Threads {
    #[must_use]
    pub fn new() -> Self {
        let ops = SparseSet::new(0);
        Self {
            threads: vec![],
            ops,
        }
    }

    pub fn with_set_size(set_capacity: usize) -> Self {
        let ops = SparseSet::new(set_capacity);
        Self {
            threads: vec![],
            ops,
        }
    }
}

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

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
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
        let new_ptr = self.as_usize() + rhs;

        InstIndex::from(new_ptr)
    }
}

impl std::ops::Add<Self> for InstIndex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let (lhs, rhs) = (self.as_usize(), rhs.as_usize());
        let new_ptr = lhs + rhs;

        InstIndex::from(new_ptr)
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Opcode {
    Any(InstAny),
    Consume(InstConsume),
    Split(InstSplit),
    Jmp(InstJmp),
    StartSave(InstStartSave),
    EndSave(InstEndSave),
    Match,
}
impl Opcode {
    fn is_match(&self) -> Option<()> {
        match self {
            Self::Match => Some(()),
            _ => None,
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Match => std::fmt::Display::fmt(&InstMatch, f),
            Opcode::Consume(i) => std::fmt::Display::fmt(&i, f),
            Opcode::Split(i) => std::fmt::Display::fmt(&i, f),
            Opcode::Any(i) => std::fmt::Display::fmt(&i, f),
            Opcode::Jmp(i) => std::fmt::Display::fmt(&i, f),
            Opcode::StartSave(i) => std::fmt::Display::fmt(&i, f),
            Opcode::EndSave(i) => std::fmt::Display::fmt(&i, f),
        }
    }
}

#[derive(Debug)]
struct InstMatch;

impl Display for InstMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Match: (END)",)
    }
}

#[derive(Debug)]
struct InstAny {
    next: InstIndex,
}

impl InstAny {
    fn new(next: InstIndex) -> Self {
        Self { next }
    }
}

impl Display for InstAny {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Any: ({:04})", self.next.as_usize())
    }
}

#[derive(Debug)]
struct InstConsume {
    value: char,
    next: InstIndex,
}

impl InstConsume {
    #[must_use]
    fn new(value: char, next: InstIndex) -> Self {
        Self { value, next }
    }
}

impl Display for InstConsume {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Consume: {:?}, ({:04})",
            self.value,
            self.next.as_usize()
        )
    }
}

#[derive(Debug)]
struct InstSplit {
    next1: InstIndex,
    next2: InstIndex,
}

impl InstSplit {
    #[must_use]
    fn new(next1: InstIndex, next2: InstIndex) -> Self {
        Self { next1, next2 }
    }
}

impl Display for InstSplit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Split: ({:04}), ({:04})",
            self.next1.as_usize(),
            self.next2.as_usize()
        )
    }
}

#[derive(Debug)]
struct InstJmp {
    next: InstIndex,
}

impl InstJmp {
    #[must_use]
    fn new(next: InstIndex) -> Self {
        Self { next }
    }
}

impl Display for InstJmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Jump: ({:04})", self.next.as_usize())
    }
}

#[derive(Debug)]
struct InstStartSave {
    slot_id: usize,
    next: InstIndex,
}

impl InstStartSave {
    #[must_use]
    fn new(slot_id: usize, next: InstIndex) -> Self {
        Self { slot_id, next }
    }
}

impl Display for InstStartSave {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "StartSave[{:04}]: ({:04})",
            self.slot_id,
            self.next.as_usize()
        )
    }
}

#[derive(Debug)]
struct InstEndSave {
    slot_id: usize,
    next: InstIndex,
}

impl InstEndSave {
    #[must_use]
    fn new(slot_id: usize, next: InstIndex) -> Self {
        Self { slot_id, next }
    }
}

impl Display for InstEndSave {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "EndSave[{:04}]: ({:04})",
            self.slot_id,
            self.next.as_usize()
        )
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
    // Don't visit states we've already added.
    let pc = t.inst;
    let ip = program.get(pc.as_usize());
    let inst = match ip {
        // if the tread is already defined return
        Some(inst) if thread_list.ops.contains(&inst.id) => return thread_list,
        // if it's the end of the program without a match instruction return.
        None => return thread_list,
        Some(inst) => {
            thread_list.ops.insert(inst.id);
            inst
        }
    };

    let opcode = &inst.opcode;
    match opcode {
        Opcode::Split(i) => {
            let x = i.next1;
            let y = i.next2;
            let child_thread1 = Thread::new(t.save_group, x);
            thread_list = add_thread(program, save_groups, thread_list, child_thread1, sp, input);

            let child_thread2 = Thread::new(t.save_group, y);
            add_thread(program, save_groups, thread_list, child_thread2, sp, input)
        }
        Opcode::Jmp(i) => {
            let next = i.next;

            let child_thread = Thread::new(t.save_group, next);
            add_thread(program, save_groups, thread_list, child_thread, sp, input)
        }
        Opcode::StartSave(i) => {
            let next = i.next;
            let slot = i.slot_id;
            let save_group = SaveGroup::Allocated { slot_id: slot };

            let child_thread = Thread::new(save_group, next);
            add_thread(program, save_groups, thread_list, child_thread, sp, input)
        }
        Opcode::EndSave(InstEndSave { slot_id, next }) => {
            let next = *next;
            let slot = *slot_id;
            let closed_save = match t.save_group {
                SaveGroup::Open { slot_id, start } => SaveGroup::Complete {
                    slot_id,
                    start,
                    end: sp,
                },

                _ => panic!("attempting to close an unopened save."),
            };

            save_groups[slot] = SaveGroupSlot::from(closed_save);

            let child_thread = Thread::new(closed_save, next);
            add_thread(program, save_groups, thread_list, child_thread, sp, input)
        }
        _ => {
            thread_list.threads.push(t);
            thread_list
        }
    }
}

pub fn run<const SG: usize>(program: &[Instruction], input: &str) -> Vec<SaveGroupSlot> {
    use core::mem::swap;

    let input_len = input.len();
    let program_len = program.len();

    let mut input_idx = 0;
    let mut current_thread_list = Threads::with_set_size(program_len);
    let mut next_thread_list = Threads::with_set_size(program_len);
    let mut sub = vec![SaveGroupSlot::None; SG];

    let start_thread = Thread::new(SaveGroup::None, InstIndex::from(0));
    current_thread_list = add_thread(
        program,
        &mut sub,
        current_thread_list,
        start_thread,
        input_idx,
        input,
    );

    'outer: while input_idx <= input_len {
        for thread in current_thread_list.threads.iter() {
            let save_group = thread.save_group;
            let next_char = get_at(input, input_idx);
            let inst_idx = thread.inst;
            let opcode = program.get(inst_idx.as_usize()).map(|i| &i.opcode);

            match opcode {
                Some(Opcode::Any(_)) if next_char.is_none() => {
                    break;
                }
                Some(Opcode::Any(i)) => {
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
                        Thread::new(thread_local_save_group, i.next),
                        input_idx + 1,
                        input,
                    );
                }
                Some(Opcode::Consume(InstConsume { value, next })) if Some(*value) == next_char => {
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
                        Thread::new(thread_local_save_group, *next),
                        input_idx + 1,
                        input,
                    );
                }
                // next value doesn't match
                Some(Opcode::Consume(_)) => {
                    break;
                }
                Some(Opcode::Match) => {
                    // set a condition breaking source_idx to break the outter while loop.
                    break 'outer;
                }
                None => {
                    break 'outer;
                }
                _ => continue,
            }
        }

        input_idx += 1;
        swap(&mut current_thread_list, &mut next_thread_list);
        next_thread_list.ops.clear();
    }

    sub
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pad_match_results_to(mut matches: Vec<SaveGroupSlot>, pad_to: usize) -> Vec<SaveGroupSlot> {
        let cur_len = matches.len();

        if pad_to > cur_len {
            matches.resize_with(pad_to, || SaveGroupSlot::None);
        }
        matches
    }

    #[test]
    fn should_evaluate_simple_linear_match_expression() {
        let progs = vec![
            (
                pad_match_results_to(vec![SaveGroupSlot::complete(0, 0, 1)], 1),
                Instructions::new(vec![
                    Opcode::StartSave(InstStartSave::new(0, InstIndex::from(1))),
                    Opcode::Consume(InstConsume::new('a', InstIndex::from(2))),
                    Opcode::EndSave(InstEndSave::new(0, InstIndex::from(3))),
                    Opcode::Match,
                ]),
            ),
            (
                pad_match_results_to(vec![SaveGroupSlot::complete(0, 0, 2)], 1),
                Instructions::new(vec![
                    Opcode::StartSave(InstStartSave::new(0, InstIndex::from(1))),
                    Opcode::Consume(InstConsume::new('a', InstIndex::from(2))),
                    Opcode::Consume(InstConsume::new('a', InstIndex::from(3))),
                    Opcode::EndSave(InstEndSave::new(0, InstIndex::from(4))),
                    Opcode::Match,
                ]),
            ),
            (
                pad_match_results_to(vec![SaveGroupSlot::complete(0, 2, 3)], 1),
                Instructions::new(vec![
                    Opcode::StartSave(InstStartSave::new(0, InstIndex::from(1))),
                    Opcode::Consume(InstConsume::new('b', InstIndex::from(2))),
                    Opcode::EndSave(InstEndSave::new(0, InstIndex::from(3))),
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
    fn should_evaluate_split_match_expression() {
        let (expected_res, prog) = (
            pad_match_results_to(vec![SaveGroupSlot::complete(0, 0, 2)], 1),
            Instructions::new(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(1))),
                Opcode::Any(InstAny::new(InstIndex::from(0))),
                Opcode::StartSave(InstStartSave::new(0, InstIndex::from(3))),
                Opcode::Consume(InstConsume::new('a', InstIndex::from(4))),
                Opcode::Consume(InstConsume::new('a', InstIndex::from(5))),
                Opcode::EndSave(InstEndSave::new(0, InstIndex::from(6))),
                Opcode::Match,
            ]),
        );

        let input = "aab";

        let res = run::<1>(&prog.program, input);
        assert_eq!(expected_res, res)
    }

    #[test]
    fn should_print_test_instructions() {
        let prog = Instructions::new(vec![
            Opcode::Consume(InstConsume::new('a', InstIndex::from(1))),
            Opcode::Consume(InstConsume::new('b', InstIndex::from(2))),
            Opcode::Match,
        ]);

        assert_eq!(
            "0000: Consume: 'a', (0001)
0001: Consume: 'b', (0002)
0002: Match: (END)\n",
            prog.to_string()
        )
    }
}

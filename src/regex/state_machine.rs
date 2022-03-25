use std::fmt::{Debug, Display};
use std::ops::Index;

use super::sparse_set::SparseSet;

pub trait Execute<T> {
    fn execute(self, thread: T) -> T;
}

pub trait ExecuteMut<T, R> {
    fn execute_mut(&mut self, operation: &T) -> R;
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct InputPtr(usize);

impl InputPtr {
    #[inline]
    fn as_usize(&self) -> usize {
        self.0
    }

    #[inline]
    fn next_pos(&self) -> Self {
        *self + 1
    }
}

impl std::ops::Add<usize> for InputPtr {
    type Output = InputPtr;

    fn add(self, rhs: usize) -> Self::Output {
        let new_ptr = self.as_usize() + rhs;
        InputPtr(new_ptr)
    }
}

enum Follow {
    InstPtr(InstPtr),
}

pub struct Threads {
    ops: SparseSet,
}

impl Threads {
    #[must_use]
    pub fn new() -> Self {
        let ops = SparseSet::new(0);
        Self { ops }
    }

    pub fn resize(&mut self, new_size: usize) {
        use core::mem::swap;
        let mut ops = SparseSet::new(new_size);
        for item in ops.iter().copied() {
            self.ops.insert(item)
        }

        swap(&mut self.ops, &mut ops);
    }
}

pub struct ThreadCache {
    nlist: Threads,
    clist: Threads,

    stack: Vec<Follow>,
}

impl ThreadCache {
    #[must_use]
    pub fn new() -> Self {
        Self {
            nlist: Threads::new(),
            clist: Threads::new(),
            stack: vec![],
        }
    }
}

pub struct Instructions<T: Debug> {
    program: Vec<Instruction<T>>,
}

impl<T: Debug> Instructions<T> {
    #[must_use]
    pub fn new(program: Vec<Instruction<T>>) -> Self {
        Self { program }
    }

    pub fn len(&self) -> usize {
        self.program.len()
    }
}

impl<T: Debug> Display for Instructions<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (pos, inst) in self.program.iter().enumerate() {
            writeln!(f, "{:04}: {}", pos, inst)?
        }

        Ok(())
    }
}

impl<T: Debug> std::ops::Index<InstPtr> for Instructions<T> {
    type Output = Instruction<T>;

    fn index(&self, index: InstPtr) -> &Self::Output {
        let idx = index.as_usize();
        &self.program[idx]
    }
}

impl<T: Debug> std::ops::IndexMut<InstPtr> for Instructions<T> {
    fn index_mut(&mut self, index: InstPtr) -> &mut Self::Output {
        let idx = index.as_usize();
        &mut self.program[idx]
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
struct InstPtr(usize);

impl InstPtr {
    #[inline]
    fn as_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for InstPtr {
    fn from(ptr: usize) -> Self {
        Self(ptr)
    }
}

pub enum Instruction<T: Debug> {
    Match,
    Consume(InstConsume<T>),
    Split(InstSplit),
}

impl<T: Debug> Instruction<T> {
    fn is_match(&self) -> Option<()> {
        match self {
            Self::Match => Some(()),
            _ => None,
        }
    }
}

impl<T: Debug> Display for Instruction<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Match => InstMatch.fmt(f),
            Instruction::Consume(i) => i.fmt(f),
            Instruction::Split(i) => i.fmt(f),
        }
    }
}

struct InstMatch;

impl Display for InstMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Match: (END)",)
    }
}

struct InstConsume<T> {
    value: T,
    next: InstPtr,
}

impl<T> InstConsume<T> {
    #[must_use]
    fn new(value: T, next: InstPtr) -> Self {
        Self { value, next }
    }
}

impl<T: Debug> Display for InstConsume<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Consume: {:?}, ({:04})",
            self.value,
            self.next.as_usize()
        )
    }
}

struct InstSplit {
    next1: InstPtr,
    next2: InstPtr,
}

impl InstSplit {
    #[must_use]
    fn new(next1: InstPtr, next2: InstPtr) -> Self {
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

pub struct StateMachine<'a, T, Input>
where
    T: Debug,
    Input: ?Sized,
{
    program: &'a Instructions<T>,
    input: &'a Input,
}

impl<'a, T, Input> StateMachine<'a, T, Input>
where
    T: Debug + Copy + Eq,
    Input: Index<usize, Output = T> + ?Sized,
{
    #[must_use]
    pub fn new(program: &'a Instructions<T>, input: &'a Input) -> Self {
        Self { program, input }
    }

    pub fn exec(
        cache: &mut ThreadCache,
        program: &'a Instructions<T>,
        input: &'a Input,
        quit_after_match: bool,
        at: InputPtr,
        end: InputPtr,
    ) -> bool {
        let mut machine = Self::new(program, input);
        cache.clist.resize(program.len());
        cache.nlist.resize(program.len());

        machine.exec_(cache, quit_after_match, at, end)
    }

    fn exec_(
        &mut self,
        cache: &mut ThreadCache,
        quit_after_match: bool,
        mut at: InputPtr,
        end: InputPtr,
    ) -> bool {
        let mut matched = false;
        let mut all_matched = false;

        cache.clist.ops.clear();
        cache.nlist.ops.clear();

        'LOOP: loop {
            if cache.clist.ops.is_empty() {
                self.add(&mut cache.stack, &mut cache.clist, InstPtr(0), at);
            }

            for i in 0..cache.clist.ops.len() {
                let ip = cache.clist.ops[i];
                if self.step(&mut cache.stack, &mut cache.nlist, InstPtr(ip), at) {
                    matched = true;
                    all_matched = true;

                    if quit_after_match {
                        break 'LOOP;
                    }
                }
            }
            if at.as_usize() >= end.as_usize() {
                break;
            }
            at = at.next_pos();
            core::mem::swap(&mut cache.clist, &mut cache.nlist);
            cache.nlist.ops.clear();
        }
        matched
    }

    fn step(
        &mut self,
        stack: &mut Vec<Follow>,
        thread_list: &mut Threads,
        ip: InstPtr,
        at: InputPtr,
    ) -> bool {
        use Instruction::*;

        match self.program[ip] {
            Match => true,
            Consume(ref inst) => {
                if self.input[at.as_usize()] == inst.value {
                    self.add(stack, thread_list, ip, at.next_pos());
                }

                false
            }
            Split(_) => false,
        }
    }

    fn add(
        &mut self,
        stack: &mut Vec<Follow>,
        thread_list: &mut Threads,
        start: InstPtr,
        at: InputPtr,
    ) -> InstPtr {
        stack.push(Follow::InstPtr(start));
        let mut ip = start;
        while let Some(frame) = stack.pop() {
            ip = match frame {
                Follow::InstPtr(ip) => self.add_step(stack, thread_list, ip, at),
            };
        }

        ip
    }

    fn add_step(
        &mut self,
        stack: &mut Vec<Follow>,
        thread_list: &mut Threads,
        start: InstPtr,
        _at: InputPtr,
    ) -> InstPtr {
        use Instruction::*;

        let mut ip = start;
        loop {
            // Don't visit states we've already added.
            if thread_list.ops.contains(ip.as_usize()) {
                return ip;
            } else {
                thread_list.ops.push(ip.as_usize());
            }

            match self.program[ip] {
                Split(ref inst) => {
                    stack.push(Follow::InstPtr(inst.next2));
                    ip = inst.next1;
                }
                Match | Consume(_) => {
                    return ip;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_evaluate_test_expression() {
        let prog = Instructions::new(vec![
            Instruction::Consume(InstConsume::new('a', InstPtr::from(1))),
            Instruction::Match,
        ]);
        let mut cache = ThreadCache::new();
        let input = ['a', 'a', 'b'];

        StateMachine::<char, [char]>::exec(
            &mut cache,
            &prog,
            input.as_ref(),
            true,
            InputPtr(0),
            InputPtr(2),
        );
    }

    #[test]
    fn should_print_test_instructions() {
        let prog = Instructions::new(vec![
            Instruction::Consume(InstConsume::new('a', InstPtr::from(1))),
            Instruction::Consume(InstConsume::new('b', InstPtr::from(2))),
            Instruction::Match,
        ]);

        assert_eq!(
            "0000: Consume: 'a', (0001)
0001: Consume: 'b', (0002)
0002: Match: (END)\n",
            prog.to_string()
        )
    }
}

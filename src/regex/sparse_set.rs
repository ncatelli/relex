pub struct SparseSet {
    dense: Vec<usize>,
    sparse: Box<[usize]>,
}

impl SparseSet {
    #[must_use]
    pub fn new(max_len: usize) -> Self {
        Self {
            dense: Vec::new(),
            sparse: vec![0; max_len].into_boxed_slice(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.dense.is_empty()
    }

    pub fn len(&self) -> usize {
        self.dense.len()
    }

    pub fn insert_unchecked(&mut self, val: usize) {
        let sparse_ptr = self.dense.len();
        self.dense.push(val);
        self.sparse[val] = sparse_ptr;
    }

    pub fn push_unchecked(&mut self, val: usize) {
        self.insert_unchecked(val)
    }

    pub fn pop(&mut self) -> Option<usize> {
        self.dense.pop()
    }

    pub fn contains(&self, val: usize) -> bool {
        let idx = self.sparse[val];
        self.dense.get(idx) == Some(&val)
    }

    pub fn clear(&mut self) {
        self.dense.clear()
    }
}

impl std::ops::Deref for SparseSet {
    type Target = [usize];

    fn deref(&self) -> &Self::Target {
        &self.dense
    }
}

impl std::fmt::Debug for SparseSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SparseSet({:?}", &self.dense)
    }
}

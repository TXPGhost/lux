use std::{fmt::Debug, hash::Hash, marker::PhantomData};

/// A handle to a value of type `T` stored inside an [Arena]
#[derive(Debug)]
pub struct Handle<T> {
    arena_idx: usize,
    phantom: PhantomData<T>,
}

impl<T> Handle<T> {
    /// Constructs a new handle from an index (use this carefully)
    pub fn from_idx(arena_idx: usize) -> Self {
        Self {
            arena_idx,
            phantom: PhantomData,
        }
    }

    /// Returns the arena index associated with this handle
    pub fn get_idx(&self) -> usize {
        self.arena_idx
    }

    /// Transmutes this handle into a handle of another type (use this carefully)
    pub fn transmute<U>(self) -> Handle<U> {
        Handle {
            arena_idx: self.arena_idx,
            phantom: PhantomData,
        }
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Handle<T> {}

impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.arena_idx == other.arena_idx
    }
}

impl<T> Eq for Handle<T> {}

impl<T> Hash for Handle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.arena_idx.hash(state);
    }
}

/// An arena of items of type `T` referenced by index
#[derive(Debug)]
pub struct Arena<T: Debug> {
    data: Vec<Option<T>>,
}

impl<T: Debug> Arena<T> {
    /// Constructs a new, empty [Arena]
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a value of type `T` to the [Arena], returning a [Handle]
    pub fn add(&mut self, value: T) -> Handle<T> {
        self.data.push(Some(value));
        Handle {
            arena_idx: self.data.len() - 1,
            phantom: PhantomData,
        }
    }

    /// Allocates space for a value of type `T` to the [Arena], returning a [Handle]
    pub fn alloc(&mut self) -> Handle<T> {
        self.data.push(None);
        Handle {
            arena_idx: self.data.len() - 1,
            phantom: PhantomData,
        }
    }

    /// Returns an immutable reference to the value referenced by a [Handle]
    pub fn get(&self, handle: Handle<T>) -> &T {
        self.data[handle.arena_idx]
            .as_ref()
            .expect("tried to use uninitialized value")
    }

    /// Returns a mutable reference to the value referenced by a [Handle]
    pub fn get_mut(&mut self, handle: Handle<T>) -> &mut T {
        self.data[handle.arena_idx]
            .as_mut()
            .expect("tried to use uninitialized value")
    }

    /// Assigns the value referenced by a [Handle]
    pub fn set(&mut self, handle: Handle<T>, value: T) {
        self.data[handle.arena_idx] = Some(value);
    }

    /// Returns an iterator over handles in this arena
    pub fn iter_handles(&self) -> impl Iterator<Item = Handle<T>> {
        (0..self.data.len()).map(|i| Handle {
            arena_idx: i,
            phantom: PhantomData,
        })
    }

    /// Returns the number of entries in this arena
    pub fn num_entries(&self) -> usize {
        self.data.len()
    }
}

impl<T: Debug + Clone> Arena<T> {
    /// Fills an arena with the given default value (use this carefully)
    pub fn new_filled(value: T, amount: usize) -> Self {
        Self {
            data: vec![Some(value); amount],
        }
    }
}

impl<T: Debug> Default for Arena<T> {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

/// A mapping from handles to `T` to values of type `U`
#[derive(Debug)]
pub struct ArenaMap<T, U> {
    data: Vec<Option<U>>,
    phantom: PhantomData<T>,
}

impl<T, U> ArenaMap<T, U> {
    /// Constructs a new, empty [ArenaMap]
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a value into the map
    pub fn insert(&mut self, handle: Handle<T>, value: U) -> Option<U> {
        let idx = handle.arena_idx;
        while self.data.len() <= idx {
            self.data.push(None);
        }
        let old = self.data[idx].take();
        self.data[idx] = Some(value);
        old
    }

    /// Returns the existing value in the map indexed by the given handle
    pub fn get(&self, handle: Handle<T>) -> Option<&U> {
        self.data.get(handle.arena_idx).and_then(|val| val.as_ref())
    }
}

impl<T, U> Default for ArenaMap<T, U> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            phantom: PhantomData,
        }
    }
}

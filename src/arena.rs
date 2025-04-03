use std::{fmt::Debug, hash::Hash, marker::PhantomData};

/// A handle to a value of type [T] stored inside an [Arena]
#[derive(Debug)]
pub struct Handle<T> {
    arena_idx: usize,
    phantom: PhantomData<T>,
}

impl<T> Handle<T> {
    /// Returns the arena index associated with this handle
    pub fn get_idx(&self) -> usize {
        self.arena_idx
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

/// An arena of items of type [T] referenced by index
#[derive(Debug)]
pub struct Arena<T: Debug> {
    data: Vec<T>,
}

impl<T: Debug> Arena<T> {
    /// Constructs a new, empty [Arena]
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a value of type [T] to the [Arena], returning a [Handle]
    pub fn add(&mut self, value: T) -> Handle<T> {
        self.data.push(value);
        Handle {
            arena_idx: self.data.len() - 1,
            phantom: PhantomData,
        }
    }

    /// Returns an immutable reference to the value referenced by a [Handle]
    pub fn get(&self, handle: Handle<T>) -> &T {
        &self.data[handle.arena_idx]
    }

    /// Returns a mutable reference to the value referenced by a [Handle]
    pub fn get_mut(&mut self, handle: Handle<T>) -> &mut T {
        &mut self.data[handle.arena_idx]
    }

    /// Returns an iterator over items in this arena
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.data.iter()
    }
}

impl<T: Debug> Default for Arena<T> {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

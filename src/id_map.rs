use std::collections::HashMap;
use std::marker::PhantomData;

use serde::Serialize;

pub struct IdMap<I>(HashMap<Id<I>, I>);

impl<I> IdMap<I> {
    pub fn new() -> IdMap<I> {
        IdMap(HashMap::new())
    }

    pub fn insert(&mut self, item: I) -> Id<I> {
        let id = Id(self.0.len(), PhantomData);
        let id_clone = id.clone();
        self.0.insert(id, item);

        id_clone
    }

    pub fn iter(&self) -> impl Iterator<Item = &I> {
        self.0.values()
    }

    pub fn get(&self, id: &Id<I>) -> &I {
        self.0.get(&id).expect("The location id was invalid.")
    }
}

#[derive(Serialize)]
pub struct Id<T>(usize, PhantomData<T>);

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Id<T>) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id(self.0.clone(), PhantomData)
    }
}

impl<T> std::hash::Hash for Id<T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.0.hash(state)
    }
}

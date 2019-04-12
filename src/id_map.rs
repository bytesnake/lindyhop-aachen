use std::collections::HashMap;
use std::marker::PhantomData;

use serde::Serialize;
use uuid::Uuid;

#[derive(Serialize)]
pub struct IdMap<I>(HashMap<Id<I>, I>);

impl<I> IdMap<I> {
    pub fn new() -> IdMap<I> {
        IdMap(HashMap::new())
    }

    pub fn insert(&mut self, item: I) -> Id<I> {
        let id = Id {
            raw: Uuid::new_v4(),
            phantom: PhantomData,
        };
        let id_clone = id.clone();
        self.0.insert(id, item);

        id_clone
    }

    pub fn values(&self) -> impl Iterator<Item = &I> {
        self.0.values()
    }

    pub fn get(&self, id: &Id<I>) -> &I {
        self.0.get(&id).expect("The location id was invalid.")
    }
}

#[derive(Serialize)]
#[serde(transparent)]
pub struct Id<T> {
    raw: Uuid,
    #[serde(skip)]
    phantom: PhantomData<T>,
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Id<T>) -> bool {
        self.raw == other.raw
    }
}

impl<T> Eq for Id<T> {}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id {
            raw: self.raw.clone(),
            phantom: PhantomData,
        }
    }
}

impl<T> std::hash::Hash for Id<T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.raw.hash(state)
    }
}

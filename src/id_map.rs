use std::collections::HashMap;
use std::marker::PhantomData;

use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::crud::{Crud};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct IdMap<I>(HashMap<Id<I>, I>);

impl<I> IdMap<I> {
    pub fn new() -> IdMap<I> {
        IdMap(HashMap::new())
    }

    pub fn from(raw: HashMap<UnsafeId, I>) -> IdMap<I> {
        IdMap(raw.into_iter().map(|(k, v)| (Id::from(k), v)).collect())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Id<I>, &I)> {
        self.0.iter()
    }

    pub fn values(&self) -> impl Iterator<Item = &I> {
        self.0.values()
    }

    pub fn validate(&self, unsafe_id: UnsafeId) -> Option<(Id<I>)> {
        if self.0.contains_key(&Id::from(unsafe_id)) {
            Some(Id::from(unsafe_id))
        } else {
            None
        }
    }
}

impl<I> Crud for IdMap<I> {
    type Id = Id<I>;
    type Item = I;


    fn create(&mut self, item: I) -> Id<I> {
        let uuid = Uuid::new_v4();
        self.0.insert(Id::from(uuid), item);

        Id::from(uuid)
    }

    fn read(&self, id: &Id<I>) -> &I {
        self.0.get(&id).expect("The id was invalid.")
    }

    fn update(&mut self, id: Id<I>, new_item: I) {
        self.0.insert(id, new_item);
    }

    fn delete(&mut self, id: Id<I>) -> I {
        self.0.remove(&id).expect("The id was invalid.")
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(transparent)]
pub struct Id<T> {
    raw: Uuid,
    #[serde(skip)]
    phantom: PhantomData<T>,
}

pub type UnsafeId = Uuid;

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

impl<T> Copy for Id<T> {}

impl<T> std::hash::Hash for Id<T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.raw.hash(state)
    }
}

impl<T> Id<T> {
    fn from(raw: Uuid) -> Id<T> {
        Id {
            raw,
            phantom: PhantomData,
        }
    }

    pub fn to_unsafe(&self) -> UnsafeId {
        self.raw.clone()
    }
}

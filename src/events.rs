use chrono::prelude::*;
use serde::Serialize;

use super::id_map::{Id, IdMap};

// Types

#[derive(Serialize, Clone)]
pub struct Event {
    pub name: String,
    pub teaser: String,
    pub description: String,
    pub occurrences: Vec<Occurrence>,
}

#[derive(Serialize, Clone)]
pub struct Occurrence {
    pub start: NaiveDateTime,
    pub duration: Duration,
    pub location_id: Id<Location>,
}

type Duration = u64;

#[derive(Serialize, Clone)]
pub struct Location {
    pub name: String,
    pub address: String,
}

pub struct Store {
    pub locations: IdMap<Location>,
    pub events: IdMap<Event>,
}

pub type Locations = IdMap<Location>;
pub type Events = IdMap<Event>;

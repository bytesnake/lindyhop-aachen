use std::collections::HashMap;

use chrono::prelude::*;
use serde::{Deserialize, Serialize};

use super::id_map::{Id, IdMap, UnsafeId};

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

#[derive(Serialize, Deserialize, Clone)]
pub struct Location {
    pub name: String,
    pub address: String,
}

pub type Locations = IdMap<Location>;

pub type Events = IdMap<Event>;

#[derive(Serialize, Clone)]
pub struct Store {
    pub locations: Locations,
    pub events: Events,
}

impl Store {
    fn resolve(locations: Locations, ref_events: HashMap<UnsafeId, RefEvent>) -> Option<Store> {
        let maybe_events: Option<HashMap<UnsafeId, Event>> = ref_events
            .into_iter()
            .map(|(key, ref_event)| ref_event.resolve(&locations).map(|event| (key, event)))
            .collect();

        maybe_events.map(|events| Store {
            locations: locations,
            events: IdMap::init(events),
        })
    }
}

#[derive(Deserialize)]
pub struct RefStore {
    pub locations: Locations,
    pub ref_events: HashMap<UnsafeId, RefEvent>,
}

#[derive(Deserialize)]
pub struct RefEvent {
    pub name: String,
    pub teaser: String,
    pub description: String,
    pub occurrences: Vec<RefOccurrence>,
}

impl RefEvent {
    pub fn resolve(self, locations: &Locations) -> Option<Event> {
        let name = self.name;
        let teaser = self.teaser;
        let description = self.description;

        let maybe_occurrences: Option<Vec<Occurrence>> = self
            .occurrences
            .into_iter()
            .map(|occurrence| occurrence.resolve(locations))
            .collect();

        maybe_occurrences.map(|occurrences| Event {
            name,
            teaser,
            description,
            occurrences,
        })
    }
}

#[derive(Deserialize)]
pub struct RefOccurrence {
    pub start: NaiveDateTime,
    pub duration: Duration,
    #[serde(rename = "location_id")]
    pub unsafe_location_id: UnsafeId,
}

impl RefOccurrence {
    pub fn resolve(self, locations: &Locations) -> Option<Occurrence> {
        locations
            .validate(self.unsafe_location_id)
            .map(|location_id| Occurrence {
                start: self.start,
                duration: self.duration,
                location_id,
            })
    }
}

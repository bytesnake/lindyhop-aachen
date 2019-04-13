#![feature(proc_macro_hygiene, decl_macro, map_get_key_value)]

mod events;
mod id_map;

use std::sync::RwLock;

#[macro_use]
extern crate rocket;
use chrono::prelude::*;
use maud::{html, Markup};
use rocket::State;
use rocket::response::status::NotFound;
use rocket_contrib::json::Json;
use rocket_contrib::serve::StaticFiles;
use rocket_contrib::uuid::Uuid;

use events::{Event, Events, Location, Locations, Occurrence, RefEvent};

#[get("/")]
fn index(store: State<Store>) -> Markup {
    let store = store.read().unwrap();
    
    html! {
        h1 { "Lindy Hop Aachen" }
        ol {
            @for event in store.events.values() {
                li { ( render_event(event, &store.locations) ) }
            }
        }
    }
}

fn render_event(event: &Event, locations: &Locations) -> Markup {
    html! {
        (event.name) " - " (event.teaser)
        ol {
            @for occurrence in &event.occurrences {
                li { (render_occurrence(&occurrence, locations)) }
            }
        }
    }
}

fn render_occurrence(occurrence: &Occurrence, locations: &Locations) -> Markup {
    html! {
        (occurrence.start.format("%d.%m.%Y %H:%M")) " - " (locations.get(&occurrence.location_id).name)
    }
}

#[get("/api/events")]
fn all_events(store: State<Store>) -> Json<events::Store> {
    let store = store.read().unwrap();
    Json(store.clone())
}

#[post("/api/events/<uuid>", data = "<new_event>")]
fn set_event(
    uuid: Uuid,
    new_event: Json<RefEvent>,
    store: State<Store>,
) -> Result<Json<Event>, NotFound<&'static str>> {
    let mut store = store.write().unwrap();

    store
        .events
        .validate(uuid.into_inner())
        .ok_or("The uuid does not belong to an event.")
        .and_then(|id| {
            new_event
                .into_inner()
                .resolve(&store.locations)
                .ok_or("An occurrence had an invalid location_id.")
                .map(|event| {
                    store.events.set(id, event);

                    Json(store.events.get(&id).clone())
                })
        }).map_err(|err| NotFound(err))
}

#[post("/api/locations/<uuid>", data = "<new_location>")]
fn set_location(
    uuid: Uuid,
    new_location: Json<Location>,
    store: State<Store>,
) -> Option<Json<Location>> {
    let mut store = store.write().unwrap();

    store.locations.validate(uuid.into_inner()).map(|id| {
        store.locations.set(id, new_location.into_inner());

        Json(store.locations.get(&id).clone())
    })
}

type Store = RwLock<events::Store>;

fn main() {
    let mut locations = Locations::new();
    let chico_id = locations.insert(Location {
        name: "Chico Mendès".to_string(),
        address: "Aachen".to_string(),
    });
    let sencillito_id = locations.insert(Location {
        name: "Sencillito".to_string(),
        address: "Aachen".to_string(),
    });

    let mut events = Events::new();
    events.insert(Event {
        name: "Social Dance".to_string(),
        teaser: "Einfach tanzen.".to_string(),
        description: "Lindy Hop tanzen in einer Bar.".to_string(),
        occurrences: vec![
            Occurrence {
                start: NaiveDate::from_ymd(2019, 4, 1).and_hms(20, 30, 00),
                duration: 90,
                location_id: chico_id.clone(),
            },
            Occurrence {
                start: NaiveDate::from_ymd(2019, 4, 8).and_hms(20, 30, 00),
                duration: 90,
                location_id: sencillito_id.clone(),
            },
        ],
    });
    events.insert(Event {
        name: "Anfängerkurs".to_string(),
        teaser: "Hereinschnuppern.".to_string(),
        description: "Ein Einführung für diejenigen, die noch nie Lindy Hop getanzt haben."
            .to_string(),
        occurrences: vec![
            Occurrence {
                start: NaiveDate::from_ymd(2019, 4, 1).and_hms(19, 45, 00),
                duration: 45,
                location_id: chico_id.clone(),
            },
            Occurrence {
                start: NaiveDate::from_ymd(2019, 4, 8).and_hms(20, 30, 00),
                duration: 90,
                location_id: sencillito_id.clone(),
            },
        ],
    });

    rocket::ignite()
        .manage(RwLock::new(events::Store { locations, events }))
        .mount("/admin", {
            let path = concat!(env!("CARGO_MANIFEST_DIR"), "/admin/dist");
            StaticFiles::from(path)
        })
        .mount("/", routes![index, all_events, set_event, set_location])
        .launch();
}

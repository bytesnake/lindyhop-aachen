#![feature(proc_macro_hygiene, decl_macro, map_get_key_value)]

mod events;
mod id_map;

use std::path::{Path, PathBuf};
use std::sync::RwLock;

#[macro_use]
extern crate rocket;
use chrono::prelude::*;
use maud::{html, Markup};
use rocket::response::status::NotFound;
use rocket::response::NamedFile;
use rocket::State;
use rocket_contrib::json::Json;
use rocket_contrib::uuid::Uuid;

use events::{Event, Events, Location, Locations, Occurrence};
use id_map::Id;

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
    let maybe_location = locations
        .validate(occurrence.location_id)
        .map(|id| locations.get(&id));

    html! {
        (occurrence.start.format("%d.%m.%Y %H:%M")) " - " @match maybe_location {
            Some(location) => (location.name),
            None => "Steht noch nicht fest."
        }
    }
}

#[get("/api/events")]
fn all_events(store: State<Store>) -> Json<events::Store> {
    let store = store.read().unwrap();
    Json(store.clone())
}

#[put("/api/events/<uuid>", data = "<new_event>")]
fn put_event(
    uuid: Uuid,
    new_event: Json<Event>,
    store: State<Store>,
) -> Result<Json<Event>, NotFound<&'static str>> {
    let mut store = store.write().unwrap();

    store
        .events
        .validate(uuid.into_inner())
        .ok_or("The uuid does not belong to an event.")
        .map(|id| {
            store.events.set(id, new_event.into_inner());

            Json(store.events.get(&id).clone())
        })
        .map_err(|err| NotFound(err))
}

#[post("/api/locations", data = "<new_location>")]
fn create_location(new_location: Json<Location>, store: State<Store>) -> Json<Id<Location>> {
    let mut store = store.write().unwrap();

    Json(store.locations.insert(new_location.into_inner()))
}

#[get("/api/locations/<uuid>")]
fn read_location(uuid: Uuid, store: State<Store>) -> Option<Json<Location>> {
    let store = store.read().unwrap();

    store
        .locations
        .validate(uuid.into_inner())
        .map(|id| Json(store.locations.get(&id).clone()))
}

#[put("/api/locations/<uuid>", data = "<new_location>")]
fn update_location(
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

#[derive(Responder, Debug)]
enum DeleteLocationError {
    #[response(status = 409)]
    DependentEvents(Json<Vec<Id<Event>>>),
    #[response(status = 404)]
    InvalidId(&'static str),
}

#[delete("/api/locations/<uuid>")]
fn delete_location(uuid: Uuid, store: State<Store>) -> Result<Json<Location>, DeleteLocationError> {
    let mut store = store.write().unwrap();

    use DeleteLocationError::*;
    store
        .locations
        .validate(uuid.into_inner())
        .ok_or(InvalidId("No event was found with the id."))
        .and_then(|id| {
            store
                .delete_location(&id)
                .map_err(|dependent_events| DependentEvents(Json(dependent_events)))
        })
        .map(|location| Json(location))
}

#[get("/admin")]
fn admin_route() -> Option<NamedFile> {
    admin()
}

#[get("/admin/<path..>")]
#[allow(unused_variables)]
fn admin_subroute(path: PathBuf) -> Option<NamedFile> {
    admin()
}

fn admin() -> Option<NamedFile> {
    NamedFile::open(Path::new("admin/dist/index.html")).ok()
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
                location_id: chico_id.to_unsafe(),
            },
            Occurrence {
                start: NaiveDate::from_ymd(2019, 4, 8).and_hms(20, 30, 00),
                duration: 90,
                location_id: sencillito_id.to_unsafe(),
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
                location_id: chico_id.to_unsafe(),
            },
            Occurrence {
                start: NaiveDate::from_ymd(2019, 4, 8).and_hms(20, 30, 00),
                duration: 90,
                location_id: sencillito_id.to_unsafe(),
            },
        ],
    });

    rocket::ignite()
        .manage(RwLock::new(events::Store::from(locations, events)))
        .mount(
            "/",
            routes![
                index,
                all_events,
                put_event,
                create_location,
                read_location,
                update_location,
                delete_location,
                admin_route,
                admin_subroute
            ],
        )
        .launch();
}

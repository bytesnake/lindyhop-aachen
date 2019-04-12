#![feature(proc_macro_hygiene, decl_macro)]

mod events;

#[macro_use]
extern crate rocket;
use rocket::State;

use rocket_contrib::serve::StaticFiles;

use events::{Event, Location, Occurrence};

use chrono::prelude::*;

use maud::{html, Markup};

#[get("/")]
fn index(store: State<events::Store>) -> Markup {
    html! {
        h1 { "Lindy Hop Aachen" }
        ol {
            @for event in store.events.iter() {
                li { ( render_event(event, &store.locations) ) }
            }
        }
    }
}

fn render_event(event: &Event, locations: &events::IdMap<Location>) -> Markup {
    html! {
        (event.name) " - " (event.teaser)
        ol {
            @for occurrence in &event.occurrences {
                li { (render_occurrence(&occurrence, locations)) }
            }
        }
    }
}

fn render_occurrence(occurrence: &Occurrence, locations: &events::IdMap<Location>) -> Markup {
    html! {
        (occurrence.start.format("%d.%m.%Y %H:%M")) " - " (locations.get(&occurrence.location_id).name)
    }
}

fn main() {
    let mut locations = events::IdMap::new();
    let chico_id = locations.insert(Location {
        name: "Chico Mendès".to_string(),
        address: "Aachen".to_string(),
    });
    let sencillito_id = locations.insert(Location {
        name: "Sencillito".to_string(),
        address: "Aachen".to_string(),
    });

    let mut events = events::IdMap::new();
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
        .manage(events::Store { locations, events })
        .mount("/admin", {
            let path = concat!(env!("CARGO_MANIFEST_DIR"), "/admin/dist");
            StaticFiles::from(path)
        })
        .mount("/", routes![index])
        .launch();
}

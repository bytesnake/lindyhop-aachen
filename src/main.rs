#![feature(proc_macro_hygiene, decl_macro)]

mod events;

#[macro_use]
extern crate rocket;
use rocket::State;

use events::{Event, Location, Occurrence};

use chrono::prelude::*;

use maud::{html, Markup};

#[get("/")]
fn index(events: State<Events>) -> Markup {
    html! {
        h1 { "Lindy Hop Aachen" }
        ol {
            @for event in &events.0 {
                li { (event.name) " - " (event.teaser) ": " (event.occurrences.get(0).unwrap().location.name) }
            }
        }
    }
}

struct Events<'a>(Vec<Event<'a>>);

const CHICO: Location = Location {
                name: "Chico Mendès",
                address: "Aachen",
            };

fn main() {
    rocket::ignite()
    .manage(Events(vec![Event {
        name: "Social Dance",
        teaser: "Einfach tanzen.",
        description: "Lindy Hop tanzen in einer Bar.",
        occurrences: vec![Occurrence {
            start: Local.ymd(2019, 4, 1).and_hms(20, 30, 00),
            duration: chrono::Duration::minutes(90),
            location: &chico,
        }],
    }, Event {
        name: "Anfängerkurs",
        teaser: "Hereinschnuppern.",
        description: "Ein Einführung für diejenigen, die noch nie Lindy Hop getanzt haben.",
        occurrences: vec![Occurrence {
            start: Local.ymd(2019, 4, 1).and_hms(19, 45, 00),
            duration: chrono::Duration::minutes(45),
            location: &chico,
        }]
    }])).mount("/", routes![index]).launch();
}

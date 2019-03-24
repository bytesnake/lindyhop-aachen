#![feature(proc_macro_hygiene, decl_macro)]

mod counter;

#[macro_use]
extern crate rocket;
use rocket::State;

use maud::{html, Markup};

use counter::Counter;

#[get("/")]
fn index(counter: State<Counter>) -> Markup {
    let count = counter.increment();
    html! {
        h1 { "Visitor Counter" }
        p { "You are visitor number " (count) "."}
    }
}

fn main() {
    rocket::ignite()
        .mount("/", routes![index])
        .manage(Counter::new())
        .launch();
}

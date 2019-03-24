#![feature(proc_macro_hygiene, decl_macro)]

mod counter;

#[macro_use]
extern crate rocket;
use counter::Counter;
use rocket::State;

#[get("/")]
fn index(counter: State<Counter>) -> String {
    let count = counter.get_count();
    counter.increment();
    format!("Visitor number: {}", count)
}

fn main() {
    rocket::ignite()
        .mount("/", routes![index])
        .manage(Counter::new())
        .launch();
}

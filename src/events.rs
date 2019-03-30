use chrono::prelude::*;

pub struct Event<'a> {
    pub name: &'a str,
    pub teaser: &'a str,
    pub description: &'a str,
    pub occurrences: Vec<Occurrence<'a>>,
}

pub struct Occurrence<'a> {
    pub start: DateTime<Local>,
    pub duration: chrono::Duration,
    pub location: &'a Location<'a>,
}

pub struct Location<'a> {
    pub name: &'a str,
    pub address: &'a str,
}

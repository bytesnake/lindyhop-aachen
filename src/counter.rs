use std::fs;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

const FILE_PATH: &str = "count.db";

pub struct Counter(AtomicUsize);

impl Counter {
    pub fn new() -> Counter {
        let count = fs::read_to_string(FILE_PATH)
            .map_err(|_| "File could not be read.")
            .and_then(|count_string| {
                count_string
                    .parse()
                    .map_err(|_| "File contained illegal count.")
            })
            .unwrap_or_else(|error| {
                eprintln!("{}", error);
                0
            });
        Counter(AtomicUsize::new(count))
    }

    pub fn get_count(&self) -> usize {
        self.0.load(Ordering::Relaxed)
    }

    pub fn increment(&self) -> usize {
        let new_count = fetch_plus(&self.0, 1, Ordering::Relaxed);
        fs::write(FILE_PATH, format!("{}", new_count)).expect("File was not writable.");
        new_count
    }
}

/// Like `AtomicUsize::fetch_add`, but returns the updated value.
fn fetch_plus(atom: &AtomicUsize, val: usize, order: Ordering) -> usize {
    atom.fetch_add(val, order) + val
}

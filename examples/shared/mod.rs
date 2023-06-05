use itsy::itsy_api;
#[allow(unused_imports)]
use std::{thread, time::{Instant, Duration}, io::{self, Write}};

/// Demo context data.
#[allow(dead_code)]
pub struct Context {
    pub seed: f64,
    pub last_frame: Instant,
}

#[allow(dead_code)]
impl Context {
    pub fn new() -> Self {
        Self {
            seed: 1.2345,
            last_frame: Instant::now(),
        }
    }
}

itsy_api! {
    /// Demo API providing some basic functionality
    pub MyAPI<Context> {
        /// Prints the given string to standard output.
        fn print(&mut context, value: String) {
            print!("{}", value);
        }
        /// Prints the given string followed by a newline to standard output.
        fn println(&mut context, value: String) {
            println!("{}", value);
        }
        /// Flushes standard output.
        fn flush(&mut context) {
            io::stdout().flush().unwrap();
        }
        /// Returns a random number between 0.0 and non-inclusive 1.0
        fn random(&mut context) -> f64 {
            context.seed += 1.0;
            let large = context.seed.sin() * 100000000.0;
            large - large.floor()
        }
        /// Pauses for the given number of milliseconds.
        fn sleep(&mut context, milliseconds: u64) {
            thread::sleep(Duration::from_millis(milliseconds));
        }
        /// Pauses for the given number of milliseconds minus the number of milliseconds since the last call.
        fn wait_frame(&mut context, milliseconds: u64) -> u64 {
            let already_expired = (Instant::now() - context.last_frame).as_millis() as u64;
            if already_expired < milliseconds {
                let remaining = milliseconds - already_expired;
                thread::sleep(Duration::from_millis(remaining));
            }
            context.last_frame = Instant::now();
            already_expired
        }
    }
}
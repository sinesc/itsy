use itsy::*;
use std::io::{self, Write};

/// Demo context data.
pub struct Context {
    pub seed: f64,
}

itsy_api!(
    /// Demo API providing some basic functionality
    pub MyAPI, Context, {
        /// Prints the given string to standard output.
        fn print(&mut context, value: String) {
            print!("{}", value);
            io::stdout().flush().unwrap();
        }
        /// Prints the given string followed by a newline to standard output.
        fn println(&mut context, value: String) {
            println!("{}", value);
        }
        /// Returns a random number between 0.0 and non-inclusive 1.0
        fn random(&mut context) -> f64 {
            context.seed += 1.0;
            let large = context.seed.sin() * 100000000.0;
            large - large.floor()
        }
    }
);
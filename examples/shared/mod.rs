use itsy::itsy_api;
#[allow(unused_imports)]
use std::{thread, time::{Instant, Duration}, io::{self, Write}};

/// Demo context data.
#[allow(dead_code)]
pub struct Context {
    pub seed: f64,
    pub last_frame: Instant,
    pub args: Vec<String>,
}

#[allow(dead_code)]
impl Context {
    pub fn new(args: &[ String ]) -> Self {
        Self {
            seed: 1.2345,
            last_frame: Instant::now(),
            args: args.to_vec(),
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
        /// Returns given argument or empty string. TODO: support array result
        fn get_arg(&mut context, arg: u64) -> String {
            context.args.get(arg as usize).cloned().unwrap_or_else(|| "".to_string())
        }
    }
}

/// Enable virtual terminal sequences on operating systems that,
/// don't enable this by default. Does nothing if enabling is not required.
pub fn enable_virtual_terminal_processing() {
    #[cfg(windows)]
    win_enable_virtual_terminal_processing();
}

#[cfg(windows)]
fn win_enable_virtual_terminal_processing() {
    use winapi_util::console::Console;

    if let Ok(mut term) = Console::stdout() {
        let _ = term.set_virtual_terminal_processing(true);
    }
    if let Ok(mut term) = Console::stderr() {
        let _ = term.set_virtual_terminal_processing(true);
    }
}
use itsy::{itsy_api, VMValue};
#[allow(unused_imports)]
use std::{thread, time::{Instant, Duration}, io::{self, Write, stdout}};
use std::collections::HashSet;

/// Key codes exposed to Itsy scripts.
#[derive(VMValue, Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum KeyCode {
    Backspace,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    PageUp,
    PageDown,
    Tab,
    Delete,
    Insert,
    Escape,
    F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
    KeyA, KeyB, KeyC, KeyD, KeyE, KeyF, KeyG, KeyH, KeyI, KeyJ,
    KeyK, KeyL, KeyM, KeyN, KeyO, KeyP, KeyQ, KeyR, KeyS, KeyT,
    KeyU, KeyV, KeyW, KeyX, KeyY, KeyZ,
    Digit0, Digit1, Digit2, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9,
    Space,
    Minus, Equal,
    LeftBracket, RightBracket,
    Backslash,
    Semicolon, Quote,
    Comma, Dot, Slash,
    GraveAccent,
}

/// Modifier keys exposed to Itsy scripts.
#[derive(VMValue, Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ModifierKey {
    Shift,
    Ctrl,
    Alt,
}

impl KeyCode {
    /// Maps a crossterm KeyCode to our KeyCode, or None if unmapped.
    fn from_crossterm(code: crossterm::event::KeyCode) -> Option<Self> {
        Some(match code {
            crossterm::event::KeyCode::Backspace => Self::Backspace,
            crossterm::event::KeyCode::Enter => Self::Enter,
            crossterm::event::KeyCode::Left => Self::Left,
            crossterm::event::KeyCode::Right => Self::Right,
            crossterm::event::KeyCode::Up => Self::Up,
            crossterm::event::KeyCode::Down => Self::Down,
            crossterm::event::KeyCode::Home => Self::Home,
            crossterm::event::KeyCode::End => Self::End,
            crossterm::event::KeyCode::PageUp => Self::PageUp,
            crossterm::event::KeyCode::PageDown => Self::PageDown,
            crossterm::event::KeyCode::Tab => Self::Tab,
            crossterm::event::KeyCode::Delete => Self::Delete,
            crossterm::event::KeyCode::Insert => Self::Insert,
            crossterm::event::KeyCode::Esc => Self::Escape,
            crossterm::event::KeyCode::F(n) => match n {
                1 => Self::F1, 2 => Self::F2, 3 => Self::F3, 4 => Self::F4,
                5 => Self::F5, 6 => Self::F6, 7 => Self::F7, 8 => Self::F8,
                9 => Self::F9, 10 => Self::F10, 11 => Self::F11, 12 => Self::F12,
                _ => return None,
            },
            crossterm::event::KeyCode::Char(c) => match c {
                'A' ..= 'Z' | 'a' ..= 'z' => {
                    let upper = c.to_ascii_uppercase();
                    match upper as u8 - b'A' {
                        0 => Self::KeyA, 1 => Self::KeyB, 2 => Self::KeyC,
                        3 => Self::KeyD, 4 => Self::KeyE, 5 => Self::KeyF,
                        6 => Self::KeyG, 7 => Self::KeyH, 8 => Self::KeyI,
                        9 => Self::KeyJ, 10 => Self::KeyK, 11 => Self::KeyL,
                        12 => Self::KeyM, 13 => Self::KeyN, 14 => Self::KeyO,
                        15 => Self::KeyP, 16 => Self::KeyQ, 17 => Self::KeyR,
                        18 => Self::KeyS, 19 => Self::KeyT, 20 => Self::KeyU,
                        21 => Self::KeyV, 22 => Self::KeyW, 23 => Self::KeyX,
                        24 => Self::KeyY, 25 => Self::KeyZ,
                        _ => unreachable!(),
                    }
                }
                '0' => Self::Digit0, '1' => Self::Digit1, '2' => Self::Digit2,
                '3' => Self::Digit3, '4' => Self::Digit4, '5' => Self::Digit5,
                '6' => Self::Digit6, '7' => Self::Digit7, '8' => Self::Digit8,
                '9' => Self::Digit9,
                ' ' => Self::Space,
                '-' => Self::Minus, '=' => Self::Equal,
                '[' => Self::LeftBracket, ']' => Self::RightBracket,
                '\\' => Self::Backslash,
                ';' => Self::Semicolon, '\'' => Self::Quote,
                ',' => Self::Comma, '.' => Self::Dot, '/' => Self::Slash,
                '`' => Self::GraveAccent,
                _ => return None,
            },
            _ => return None,
        })
    }
}

/// Demo context data.
#[allow(dead_code)]
pub struct Context {
    pub seed: f64,
    pub started: Instant,
    pub last_frame: Instant,
    pub args: Vec<String>,
    pub keys_down: HashSet<KeyCode>,
    pub modifiers_down: HashSet<ModifierKey>,
    pub keys_last_seen: std::collections::HashMap<KeyCode, u64>,
    pub frame_count: u64,
    pub raw_mode: bool,
    pub kitty: bool,
}

#[allow(dead_code)]
impl Context {
    pub fn new(args: &[ String ]) -> Self {
        Self {
            seed: 1.2345,
            started: Instant::now(),
            last_frame: Instant::now(),
            args: args.to_vec(),
            keys_down: HashSet::new(),
            modifiers_down: HashSet::new(),
            keys_last_seen: std::collections::HashMap::new(),
            frame_count: 0,
            raw_mode: false,
            kitty: false,
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        if self.raw_mode {
            let _ = crossterm::terminal::disable_raw_mode();
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
        /// Returns a (bad) pseudo-random number between 0.0 and non-inclusive 1.0
        fn random(&mut context) -> f64 {
            context.seed += 1.0;
            let large = context.seed.sin() * 100000000.0;
            large - large.floor()
        }
        /// Terminates the process.
        fn exit(&mut context, code: i32) {
            std::process::exit(code);
        }
        /// Milliseconds since context initialization.
        fn runtime(&mut context) -> u64 {
            (Instant::now() - context.started).as_millis() as u64
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
        /// Returns whether given argument is present.
        fn has_arg(&mut context, arg: String) -> bool {
            context.args.contains(&arg)
        }
        /// Compute mandelbrot value for given point.
        fn fastbrot(&mut context, depth: u32, x: f64, y: f64) -> u32 {
            let mut n = 0;
            let mut r = 0.0f64;
            let mut i = 0.0f64;
            while n < depth && 4.0 > r * r + i * i {
                let i_tmp = i * i;
                i = r * i * 2.0 + y;
                r = r * r - i_tmp + x;
                n += 1;
            }
            n
        }

        // Keyboard control for the demos.
        /// Puts the terminal into raw mode for keyboard input. Returns false if already enabled or on error.
        fn enable_raw_mode(&mut context) -> bool {
            if context.raw_mode {
                false
            } else if crossterm::terminal::enable_raw_mode().is_err() {
                false
            } else {
                use crossterm::{execute, event::{PushKeyboardEnhancementFlags, KeyboardEnhancementFlags}};
                let mut out = stdout();
                let _ = execute!(out, PushKeyboardEnhancementFlags(KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES), PushKeyboardEnhancementFlags(KeyboardEnhancementFlags::REPORT_EVENT_TYPES));
                context.raw_mode = true;
                true
            }
        }
        /// Restores the terminal to normal (canonical) mode.
        fn disable_raw_mode(&mut context) {
            if context.raw_mode {
                use crossterm::{execute, event::PopKeyboardEnhancementFlags};
                let mut out = stdout();
                let _ = execute!(out, PopKeyboardEnhancementFlags);
                let _ = crossterm::terminal::disable_raw_mode();
                context.raw_mode = false;
            }
        }
        /// Polls for pending keyboard events and updates internal key state. Call once per frame.
        /// Returns false if Ctrl+C was pressed (signal to quit).
        fn update_input(&mut context) -> bool {
            context.frame_count += 1;
            let mut ctrl_c = false;
            let mut keys_this_frame: HashSet<KeyCode> = HashSet::new();
            use crossterm::event::{poll, read, Event, KeyEventKind, KeyModifiers};
            while poll(Duration::ZERO).unwrap_or(false) {
                let event = match read() {
                    Ok(e) => e,
                    Err(_) => break,
                };
                if let Event::Key(key) = event {
                    // Ctrl+C => signal to quit
                    if matches!(key.code, crossterm::event::KeyCode::Char('c'))
                        && key.modifiers.contains(KeyModifiers::CONTROL)
                        && matches!(key.kind, KeyEventKind::Press | KeyEventKind::Repeat) {
                        ctrl_c = true;
                        break;
                    }
                    match key.kind {
                        KeyEventKind::Press | KeyEventKind::Repeat => {
                            if let Some(kc) = KeyCode::from_crossterm(key.code) {
                                context.keys_down.insert(kc);
                                keys_this_frame.insert(kc);
                                context.keys_last_seen.insert(kc, context.frame_count);
                            }
                            if key.modifiers.contains(KeyModifiers::SHIFT) {
                                context.modifiers_down.insert(ModifierKey::Shift);
                            }
                            if key.modifiers.contains(KeyModifiers::CONTROL) {
                                context.modifiers_down.insert(ModifierKey::Ctrl);
                            }
                            if key.modifiers.contains(KeyModifiers::ALT) {
                                context.modifiers_down.insert(ModifierKey::Alt);
                            }
                        }
                        KeyEventKind::Release => {
                            context.kitty = true; // we've seen a release event, disable auto release mechanism below
                            if let Some(kc) = KeyCode::from_crossterm(key.code) {
                                context.keys_down.remove(&kc);
                                context.keys_last_seen.remove(&kc);
                            }
                        }
                    }
                }
            }
            // remove keys not seen for too long (handles terminals without Release events)
            if !context.kitty {
                let stale = context.keys_last_seen
                    .iter()
                    .filter(|&(_, &last)| context.frame_count - last > 2)
                    .map(|(&kc, _)| kc)
                    .collect::<HashSet<KeyCode>>();
                for kc in &stale {
                    context.keys_down.remove(kc);
                    context.keys_last_seen.remove(kc);
                }
            }
            !ctrl_c
        }
        /// Returns true if the key is currently held down.
        fn is_key_down(&mut context, keycode: KeyCode) -> bool {
            context.keys_down.contains(&keycode)
        }
        /// Returns true if the key is not currently held down.
        fn is_key_up(&mut context, keycode: KeyCode) -> bool {
            !context.keys_down.contains(&keycode)
        }
        /// Returns true if the modifier key is currently held down.
        fn is_modifier_down(&mut context, modifier: ModifierKey) -> bool {
            context.modifiers_down.contains(&modifier)
        }
        /// Returns true if the modifier key is not currently held down.
        fn is_modifier_up(&mut context, modifier: ModifierKey) -> bool {
            !context.modifiers_down.contains(&modifier)
        }
        /// Returns the terminal width in columns. Falls back to 80 if the query fails.
        fn terminal_width(&mut context) -> u16 {
            crossterm::terminal::size().map(|(w, _)| w).unwrap_or(80)
        }
        /// Returns the terminal height in rows. Falls back to 24 if the query fails.
        fn terminal_height(&mut context) -> u16 {
            crossterm::terminal::size().map(|(_, h)| h).unwrap_or(24)
        }

        // Benchmark compatibility methods.
        /// Returns the given value unchanged.
        fn ret_u8(&mut context, value: u8) -> u8 { value }
        /// Returns the given value unchanged.
        fn ret_u16(&mut context, value: u16) -> u16 { value }
        /// Returns the given value unchanged.
        fn ret_u32(&mut context, value: u32) -> u32 { value }
        /// Returns the given value unchanged.
        fn ret_u64(&mut context, value: u64) -> u64 { value }
        /// Returns the given value unchanged.
        fn ret_i8(&mut context, value: i8) -> i8 { value }
        /// Returns the given value unchanged.
        fn ret_i16(&mut context, value: i16) -> i16 { value }
        /// Returns the given value unchanged.
        fn ret_i32(&mut context, value: i32) -> i32 { value }
        /// Returns the given value unchanged.
        fn ret_i64(&mut context, value: i64) -> i64 { value }
        /// Returns the given value unchanged.
        fn ret_f32(&mut context, value: f32) -> f32 { value }
        /// Returns the given value unchanged.
        fn ret_f64(&mut context, value: f64) -> f64 { value }
        /// Returns the given value unchanged.
        fn ret_bool(&mut context, value: bool) -> bool { value }
        /// Returns the given value unchanged.
        fn ret_string(&mut context, value: String) -> String { value }
        /// Returns the given value as a String.
        fn ret_str(&mut context, value: str) -> String { value.to_string() }
    }
}

/// Enable virtual terminal sequences on operating systems that,
/// don't enable this by default. Does nothing if enabling is not required.
#[allow(dead_code)]
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

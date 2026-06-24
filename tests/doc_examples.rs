//! Compiles the Itsy code examples embedded in the crate documentation to make sure they are valid.
//!
//! Itsy examples live in doc comments as ```` ``` ignore ```` fenced blocks (the `ignore` tag keeps
//! rustdoc from trying to compile them as Rust while still applying its `#`-hidden-line rendering).
//! This test extracts every such block from the documentation sources, strips `#`-hidden setup lines
//! the same way rustdoc does, assembles a compilable module and runs it through `build_str`.
//!
//! Conventions a doc example must follow to compile here (mirroring Rust's doctests):
//! - Lines starting with `#` are hidden from the rendered docs but still compiled. Use them for
//!   setup/teardown: `fn main` wrappers, helper-function stubs, type definitions reused from earlier
//!   blocks, etc. `##` at the start of a line renders a literal `#`.
//! - A block consisting solely of statements (no `fn`/`struct`/`enum`/`trait`/`impl`/`use` at the
//!   start of any line) is automatically wrapped in `fn main { ... }`. Any block that defines items
//!   must provide its own `fn main` (commonly a hidden `# fn main() { ... }`), since `build_str`
//!   requires an entry point.
//! - `print`/`println` (taking a `String`) are provided by this test's API and are auto-imported;
//!   no `use` needed. They mirror the host functions the reference example runner exposes.

use itsy::{build_str, itsy_api};

// Minimal host API for the examples, mirroring the reference runner's `print`/`println`. These are
// auto-imported into every example below. Examples are compiled, never run, so the bodies don't matter.
itsy_api! {
    pub Api<Vec<String>> {
        fn print(&mut context, value: String) {
            context.push(value);
        }
        fn println(&mut context, value: String) {
            context.push(value);
        }
    }
}

/// One extracted, ready-to-compile example.
struct Example {
    /// `file:line` of the opening fence, for diagnostics.
    label: String,
    /// First visible (non-hidden, non-blank) source line, for diagnostics.
    first_line: String,
    /// Assembled Itsy module passed to `build_str`.
    source: String,
}

/// Strips a leading `///` or `//!` doc-comment marker (and one optional following space). Returns
/// `None` for lines that are not doc comments.
fn strip_doc_prefix(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    let rest = trimmed.strip_prefix("///").or_else(|| trimmed.strip_prefix("//!"))?;
    Some(rest.strip_prefix(' ').unwrap_or(rest))
}

/// Whether a (already `trim_start`ed) example line is a rustdoc-style hidden line.
fn is_hidden(trimmed: &str) -> bool {
    trimmed.starts_with('#') && !trimmed.starts_with("##")
}

/// Turns one raw example line into the line that is actually compiled: hidden lines (`# ...`) lose
/// their marker, escaped lines (`## ...`) lose one `#`, everything else passes through unchanged.
fn uncomment(line: &str) -> String {
    let trimmed = line.trim_start();
    if trimmed.starts_with("##") {
        let indent = &line[..line.len() - trimmed.len()];
        format!("{}{}", indent, &trimmed[1..])
    } else if trimmed.starts_with('#') {
        let rest = &trimmed[1..];
        rest.strip_prefix(' ').unwrap_or(rest).to_string()
    } else {
        line.to_string()
    }
}

/// Assembles the lines of one fenced block into a compilable module.
fn assemble(file: &str, fence_line: usize, lines: &[String]) -> Example {
    let first_line = lines
        .iter()
        .find(|l| {
            let t = l.trim_start();
            !t.is_empty() && !is_hidden(t)
        })
        .cloned()
        .unwrap_or_default();

    let compiled: Vec<String> = lines.iter().map(|l| uncomment(l)).collect();

    const ITEM_KEYWORDS: [&str; 6] = ["fn ", "struct ", "enum ", "trait ", "impl ", "use "];
    let defines_items = compiled.iter().any(|l| {
        let t = l.trim_start();
        ITEM_KEYWORDS.iter().any(|kw| t.starts_with(kw))
    });

    let body = compiled.join("\n");
    let source = if defines_items {
        // Author is responsible for providing `fn main` (often a hidden one).
        format!("use Api::{{print, println}};\n{}\n", body)
    } else {
        // Pure statement block: wrap it so it has an entry point.
        format!("use Api::{{print, println}};\nfn main() {{\n{}\n}}\n", body)
    };

    Example { label: format!("{}:{}", file, fence_line), first_line, source }
}

/// Extracts every ```` ``` ignore ```` fenced block from a documentation source file.
fn extract(file: &str, src: &str) -> Vec<Example> {
    let mut examples = Vec::new();
    let mut in_fence = false;
    let mut collecting = false;
    let mut buffer: Vec<String> = Vec::new();
    let mut fence_line = 0;

    for (index, raw) in src.lines().enumerate() {
        let doc = match strip_doc_prefix(raw) {
            Some(doc) => doc,
            None => {
                // A non-doc line cannot appear inside a doc-comment fence; reset defensively.
                in_fence = false;
                collecting = false;
                buffer.clear();
                continue;
            }
        };
        let trimmed = doc.trim_start();
        if !in_fence {
            if trimmed.starts_with("```") {
                let info = trimmed.trim_start_matches('`');
                in_fence = true;
                collecting = info.contains("ignore");
                buffer.clear();
                fence_line = index + 1;
            }
        } else if trimmed.starts_with("```") {
            in_fence = false;
            if collecting {
                examples.push(assemble(file, fence_line, &buffer));
            }
            collecting = false;
            buffer.clear();
        } else if collecting {
            buffer.push(doc.to_string());
        }
    }

    examples
}

#[test]
fn doc_examples_compile() {
    let files = [
        ("src/documentation.rs", include_str!("../src/documentation.rs")),
        ("src/bytecode/builtins.rs", include_str!("../src/bytecode/builtins.rs")),
    ];

    let mut total = 0;
    let mut failures = Vec::new();

    for (name, src) in files {
        for example in extract(name, src) {
            total += 1;
            if let Err(err) = build_str::<Api>(&example.source) {
                let (line, column) = err.loc(&example.source);
                let numbered = example
                    .source
                    .lines()
                    .enumerate()
                    .map(|(i, l)| format!("  {:>3} | {}", i + 1, l))
                    .collect::<Vec<_>>()
                    .join("\n");
                failures.push(format!(
                    "\n{} (first line: `{}`)\n  {} at line {}, column {}\n{}",
                    example.label,
                    example.first_line.trim(),
                    err,
                    line,
                    column,
                    numbered,
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "{} of {} documentation example(s) failed to compile:\n{}",
        failures.len(),
        total,
        failures.join("\n"),
    );

    eprintln!("all {} documentation examples compiled", total);
}

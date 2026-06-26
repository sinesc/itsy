use itsy::*;
use itsy::runtime::VMState;
use std::{
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

/*
 * Itsy micro-benchmark harness
 *
 * A small, dependency-free benchmarking framework for Itsy code. It does not rely on the unstable
 * `test::Bencher` API, so it builds and runs on stable Rust via `cargo bench` (the bench target is
 * declared with `harness = false`).
 *
 * Each benchmark case is a standalone `.itsy` file under `itsy/bench/cases/<category>/<name>.itsy`.
 * The directory tree *is* the registry: drop a file into a (sub)folder and it becomes a case, with
 * the relative folder path as its category. No Rust changes are needed to add a case.
 *
 * A case file defines two free functions (and must NOT define `main` -- the harness generates it):
 *
 *   - `fn setup() -> State` (optional) -- builds the benchmark's state once on a fresh VM.
 *   - `fn bench(state: State)`          -- the workload, called repeatedly and timed. If the case has
 *     `fn bench()`                         no `setup`, `bench` takes no arguments.
 *
 * Sharing state without globals: the harness generates a `main` that calls `setup`, then `suspend`s
 * the VM so the Rust side can start the clock, then resumes to run `bench(state)` in a loop. Because
 * `state` is a live local in `main`'s frame, it (and its heap data) survives the suspend point -- so
 * `setup` and `bench` can exchange arbitrary custom datatypes that never cross the Rust boundary.
 *
 * The generated `main` looks like:
 *
 *   fn main() {
 *       let state = setup();
 *       while true {
 *           suspend;                     // hand control back so Rust can time the next batch
 *           let n = bench_iterations();  // how many reps Rust wants this batch (calibrated)
 *           for _ in 0..n { bench(state); }
 *       }
 *   }
 *
 * Each `vm.run()` resume executes exactly one timed batch of `n` reps and suspends again.
 *
 * Usage (from the project root):
 *
 *   cargo bench                 # run every case
 *   cargo bench -- fib          # run only cases whose "category/name" contains "fib"
 */

/// Host state threaded through API calls. `batch` is the number of `bench` reps the harness wants
/// the VM to run on the next resume; the script reads it via the `bench_iterations` API function.
struct BenchContext {
    batch: u64,
}

itsy_api! {
    BenchAPI<BenchContext> {
        fn print(&mut context, value: str) {
            print!("{}", value);
            io::stdout().flush().unwrap();
        }
        fn println(&mut context, value: str) {
            println!("{}", value);
        }
        /// Number of `bench` repetitions the harness wants for the upcoming batch.
        fn bench_iterations(&mut context) -> u64 {
            context.batch
        }
    }
}

/// Time spent warming up before measurement starts (per case).
const WARMUP: Duration = Duration::from_millis(150);
/// Number of measured samples collected per case.
const SAMPLES: usize = 50;
/// A single timed batch must run at least this long, otherwise clock resolution dominates. The
/// harness auto-calibrates the number of `bench()` calls per batch to reach this.
const MIN_BATCH_TIME: Duration = Duration::from_micros(500);
/// Safety cap on batch size so a no-op `bench()` can never spin forever during calibration.
const MAX_BATCH: u64 = 1 << 26;

/// Per-iteration timing summary for one case, in seconds.
struct Stats {
    label: String,
    mean: f64,
    median: f64,
    min: f64,
    stddev: f64,
}

fn main() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("itsy/bench/cases");

    // The directory tree is the registry: every *.itsy file is a case.
    let mut cases = Vec::new();
    collect_cases(&root, &root, &mut cases);
    cases.sort_by(|a, b| a.0.cmp(&b.0));

    if cases.is_empty() {
        eprintln!("no benchmark cases found under {}", root.display());
        return;
    }

    // Optional substring filter: first non-flag CLI argument (e.g. `cargo bench -- fib`).
    let filter = std::env::args().skip(1).find(|a| !a.starts_with('-'));

    println!("Itsy micro-benchmarks ({} samples each)\n", SAMPLES);

    let mut results = Vec::new();
    for (label, path) in &cases {
        if let Some(filter) = &filter {
            if !label.contains(filter) {
                continue;
            }
        }
        eprint!("  running {label} ... ");
        io::stderr().flush().ok();
        match run_case(path) {
            Ok(samples) => {
                eprintln!("done");
                results.push(summarize(label.clone(), &samples));
            }
            Err(err) => eprintln!("FAILED\n    {err}"),
        }
    }

    print_table(&results);
}

/// Recursively collects `(label, path)` pairs for every `.itsy` file under `dir`. The label is the
/// path relative to `root` with the extension stripped, e.g. `loops/while_count`.
fn collect_cases(root: &Path, dir: &Path, out: &mut Vec<(String, PathBuf)>) {
    let entries = match fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_cases(root, &path, out);
        } else if path.extension().map_or(false, |e| e == "itsy") {
            let label = path
                .strip_prefix(root)
                .unwrap_or(&path)
                .with_extension("")
                .to_string_lossy()
                .replace('\\', "/");
            out.push((label, path));
        }
    }
}

/// Wraps a case's source with the generated `main` that drives setup/suspend/bench. `has_setup`
/// selects the stateful (`bench(state)`) or stateless (`bench()`) shape.
fn assemble_source(case_source: &str, has_setup: bool) -> String {
    let (setup_line, bench_call) = if has_setup {
        ("    let state = setup();\n", "bench(state);")
    } else {
        ("", "bench();")
    };
    format!(
        "use BenchAPI::{{ bench_iterations }};\n\
         {case_source}\n\
         fn main() {{\n\
         {setup_line}\
         \x20   while true {{\n\
         \x20       suspend;\n\
         \x20       let n = bench_iterations();\n\
         \x20       for _ in 0..n {{\n\
         \x20           {bench_call}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n"
    )
}

/// Heuristically detects whether the case defines a top-level `fn setup`.
fn defines_setup(source: &str) -> bool {
    source.lines().any(|line| {
        let trimmed = line.trim_start();
        trimmed.strip_prefix("fn setup").map_or(false, |rest| {
            rest.chars().next().map_or(true, |c| c == '(' || c.is_whitespace())
        })
    })
}

/// Builds and runs a single benchmark case, returning one per-iteration duration (seconds) per sample.
fn run_case(path: &Path) -> Result<Vec<f64>, String> {
    let case_source = fs::read_to_string(path).map_err(|e| format!("read error: {e}"))?;
    let source = assemble_source(&case_source, defines_setup(&case_source));

    let program = build_str::<BenchAPI>(&source).map_err(|e| format!("build error: {e}"))?;
    let mut context = BenchContext { batch: 0 };
    let mut vm = runtime::VM::new(program);

    // First run: execute setup() and stop at the first `suspend` (this is the untimed setup phase).
    match vm.run(&mut context).map_err(|e| format!("setup (main) failed: {e}"))? {
        VMState::Suspended => {}
        other => return Err(format!("expected VM to suspend after setup, got {other:?}")),
    }

    // Calibrate: grow the batch size until one batch runs for at least MIN_BATCH_TIME.
    let mut batch = 1u64;
    loop {
        let elapsed = time_batch(&mut vm, &mut context, batch)?;
        if elapsed >= MIN_BATCH_TIME || batch >= MAX_BATCH {
            break;
        }
        let factor = (MIN_BATCH_TIME.as_secs_f64() / elapsed.as_secs_f64().max(1e-9)).ceil() as u64;
        batch = batch.saturating_mul(factor.max(2)).min(MAX_BATCH);
    }

    // Warm up for a fixed wall-clock budget.
    let warmup_start = Instant::now();
    while warmup_start.elapsed() < WARMUP {
        time_batch(&mut vm, &mut context, batch)?;
    }

    // Measure: collect SAMPLES batches, recording the per-iteration time of each.
    let mut per_iter = Vec::with_capacity(SAMPLES);
    for _ in 0..SAMPLES {
        let elapsed = time_batch(&mut vm, &mut context, batch)?;
        per_iter.push(elapsed.as_secs_f64() / batch as f64);
    }
    Ok(per_iter)
}

/// Resumes the suspended VM to run a batch of `n` `bench()` reps and returns the elapsed wall-clock
/// time. The VM suspends again at the top of its loop, ready for the next batch.
fn time_batch(vm: &mut runtime::VM<BenchAPI, BenchContext>, context: &mut BenchContext, n: u64) -> Result<Duration, String> {
    context.batch = n;
    let start = Instant::now();
    let state = vm.run(context).map_err(|e| format!("bench() failed: {e}"))?;
    let elapsed = start.elapsed();
    match state {
        VMState::Suspended => Ok(elapsed),
        other => Err(format!("VM stopped unexpectedly during bench ({other:?})")),
    }
}

/// Reduces a set of per-iteration samples to summary statistics.
fn summarize(label: String, samples: &[f64]) -> Stats {
    let n = samples.len();
    let mean = samples.iter().sum::<f64>() / n as f64;
    let variance = if n > 1 {
        samples.iter().map(|s| (s - mean).powi(2)).sum::<f64>() / (n - 1) as f64
    } else {
        0.0
    };
    let mut sorted = samples.to_vec();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let median = sorted[n / 2];
    let min = sorted[0];
    Stats { label, mean, median, min, stddev: variance.sqrt() }
}

/// Prints all collected stats as an aligned table.
fn print_table(results: &[Stats]) {
    if results.is_empty() {
        return;
    }
    let name_width = results.iter().map(|s| s.label.len()).max().unwrap_or(0).max("benchmark".len());
    println!(
        "\n{:<width$}  {:>11}  {:>11}  {:>11}  {:>11}  {:>13}",
        "benchmark", "mean", "median", "min", "stddev", "iters/s",
        width = name_width
    );
    for s in results {
        println!(
            "{:<width$}  {:>11}  {:>11}  {:>11}  {:>11}  {:>13}",
            s.label,
            fmt_secs(s.mean),
            fmt_secs(s.median),
            fmt_secs(s.min),
            fmt_secs(s.stddev),
            fmt_count(1.0 / s.mean),
            width = name_width
        );
    }
}

/// Formats a duration in seconds using an appropriate unit (ns/µs/ms/s).
fn fmt_secs(s: f64) -> String {
    if s < 1e-6 {
        format!("{:.2} ns", s * 1e9)
    } else if s < 1e-3 {
        format!("{:.2} µs", s * 1e6)
    } else if s < 1.0 {
        format!("{:.2} ms", s * 1e3)
    } else {
        format!("{:.3} s", s)
    }
}

/// Formats an iterations-per-second count with thousands separators.
fn fmt_count(v: f64) -> String {
    let n = v.round() as u64;
    let digits = n.to_string();
    let mut out = String::new();
    for (i, c) in digits.chars().enumerate() {
        if i > 0 && (digits.len() - i) % 3 == 0 {
            out.push(',');
        }
        out.push(c);
    }
    out
}

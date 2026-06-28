use itsy::{itsy_api, build_str, runtime};
use itsy::runtime::VMState;
use std::{
    collections::HashMap,
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

#[path = "marshalling/mod.rs"]
mod marshalling;
use marshalling::*;

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
 *   cargo bench                          # run every case
 *   cargo bench -- fib                   # run only cases whose "category/name" contains "fib"
 *
 *   cargo bench -- --save out.txt        # run, print, and save a report to out.txt
 *   cargo bench -- --save-commit         # run and save a report named after the HEAD commit,
 *                                        # into itsy/bench/reports/<date>_<hash>_<message>.txt
 *
 *   cargo bench -- --compare base.txt        # run, then print results relative (%) to base.txt
 *   cargo bench -- --compare base.txt new.txt # compare two saved reports (no run); new vs base
 *
 * A report is a plain-text file: a `#`-prefixed metadata header (date / commit / message /
 * samples) followed by one tab-separated row per case holding the raw per-iteration seconds
 * (mean, median, min, stddev). The format round-trips through `write_report` / `read_report`.
 *
 * Flags can be combined with the substring filter and with each other, e.g.
 *   cargo bench -- fib --compare base.txt --save new.txt
 * runs only "fib" cases, saves them to new.txt, and prints them relative to base.txt. When using
 * --compare, put the filter *before* the flag so the filenames parse unambiguously.
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

        // --- Primitive marshalling ---
        fn echo_i32(&mut context, v: i32) -> i32 { v }
        fn echo_u64(&mut context, v: u64) -> u64 { v }
        fn echo_f64(&mut context, v: f64) -> f64 { v }
        fn echo_bool(&mut context, v: bool) -> bool { v }
        fn echo_string(&mut context, v: String) -> String { v }

        // --- Struct marshalling ---
        fn make_point(&mut context, x: i32, y: i32) -> Point {
            Point { x, y }
        }
        fn point_sum(&mut context, p: Point) -> i32 {
            p.x + p.y
        }
        fn make_record(&mut context, id: u32, name: String, value: f64) -> Record {
            Record { id, name, value }
        }
        fn record_value(&mut context, r: Record) -> f64 {
            r.value
        }
        fn make_nested(&mut context, x: i32, y: i32, label: String, score: u32) -> Nested {
            Nested { pos: Point { x, y }, label, score }
        }
        fn nested_score(&mut context, n: Nested) -> u32 {
            n.score
        }

        // --- Enum marshalling ---
        fn opposite_status(&mut context, s: Status) -> Status {
            match s {
                Status::Ok => Status::Error,
                Status::Error => Status::Ok,
                Status::Pending => Status::Pending,
            }
        }
        fn shape_area(&mut context, s: Shape) -> f64 {
            match s {
                Shape::Circle(r) => 3.14159 * r * r,
                Shape::Rect(w, h) => w * h,
                Shape::Named(_) => -1.0,
                Shape::Empty => 0.0,
            }
        }
        fn make_circle(&mut context, r: f64) -> Shape {
            Shape::Circle(r)
        }

        // --- Array marshalling ---
        fn make_i32_array(&mut context, n: u32) -> [ i32 ] {
            (0..n as i32).collect()
        }
        fn sum_i32(&mut context, values: [ i32 ]) -> i32 {
            values.iter().sum()
        }
        fn make_string_array(&mut context, n: u32) -> [ String ] {
            (0..n).map(|i| format!("item_{}", i)).collect()
        }
        fn join_strings(&mut context, parts: [ String ]) -> String {
            parts.join("-")
        }
        fn make_points(&mut context, n: u32) -> [ Point ] {
            (0..n as i32).map(|i| Point { x: i, y: i * 2 }).collect()
        }
        fn points_sum(&mut context, ps: [ Point ]) -> i32 {
            ps.iter().map(|p| p.x + p.y).sum()
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

/// Parsed command line. See the header comment for the flag semantics.
struct Args {
    /// Substring filter on the case label (positional argument).
    filter: Option<String>,
    /// `--save <file>`: write the run's report here.
    save: Option<PathBuf>,
    /// `--save-commit`: write the run's report into the reports dir, named after HEAD.
    save_commit: bool,
    /// `--compare a [b]`: one name compares the run against `a`; two names compare `b` against
    /// `a` without running anything.
    compare: Vec<PathBuf>,
}

fn main() {
    let args = match parse_args(std::env::args().skip(1)) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("{err}\n");
            print_usage();
            std::process::exit(2);
        }
    };

    // Two report names: pure comparison, no benchmarking.
    if args.compare.len() == 2 {
        match (read_report(&args.compare[0]), read_report(&args.compare[1])) {
            (Ok((_, base)), Ok((_, subj))) => print_comparison(
                &base, &subj,
                &args.compare[0].display().to_string(),
                &args.compare[1].display().to_string(),
            ),
            (Err(err), _) | (_, Err(err)) => {
                eprintln!("{err}");
                std::process::exit(1);
            }
        }
        return;
    }

    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("itsy/bench/cases");

    // The directory tree is the registry: every *.itsy file is a case.
    let mut cases = Vec::new();
    collect_cases(&root, &root, &mut cases);
    cases.sort_by(|a, b| a.0.cmp(&b.0));

    if cases.is_empty() {
        eprintln!("no benchmark cases found under {}", root.display());
        return;
    }

    println!("Itsy micro-benchmarks ({} samples each)\n", SAMPLES);
    let results = run_all(&cases, args.filter.as_deref());

    // One report name: print the run relative to that baseline.
    if args.compare.len() == 1 {
        match read_report(&args.compare[0]) {
            Ok((_, base)) => print_comparison(&base, &results, &args.compare[0].display().to_string(), "this run"),
            Err(err) => eprintln!("\n{err}"),
        }
    } else {
        print_table(&results);
    }

    // Persist the report if requested.
    let meta = ReportMeta::current();
    if let Some(path) = &args.save {
        save_report(path, &results, &meta);
    }
    if args.save_commit {
        match fname_for_commit(&meta) {
            Ok(path) => save_report(&path, &results, &meta),
            Err(err) => eprintln!("--save-commit: {err}"),
        }
    }
}

/// Parses the CLI arguments (everything after `cargo bench --`).
fn parse_args(raw: impl Iterator<Item = String>) -> Result<Args, String> {
    let mut args = Args { filter: None, save: None, save_commit: false, compare: Vec::new() };
    let mut it = raw.peekable();
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "-h" | "--help" => {
                print_usage();
                std::process::exit(0);
            }
            "-o" | "--save" => {
                let file = it.next().ok_or_else(|| format!("{arg} requires a filename"))?;
                args.save = Some(PathBuf::from(file));
            }
            "--save-commit" => args.save_commit = true,
            // Cargo appends these to signal bench mode to a libtest harness; we have none, so ignore.
            "--bench" | "--test" => {}
            "-c" | "--compare" => {
                // Greedily consume up to two following non-flag tokens as report names.
                while args.compare.len() < 2 {
                    match it.peek() {
                        Some(next) if !next.starts_with('-') => args.compare.push(PathBuf::from(it.next().unwrap())),
                        _ => break,
                    }
                }
                if args.compare.is_empty() {
                    return Err(format!("{arg} requires one or two report filenames"));
                }
            }
            other if other.starts_with('-') => return Err(format!("unknown option: {other}")),
            other => {
                if args.filter.is_some() {
                    return Err(format!("unexpected argument: {other}"));
                }
                args.filter = Some(other.to_string());
            }
        }
    }
    Ok(args)
}

fn print_usage() {
    eprintln!(
        "Usage: cargo bench -- [FILTER] [OPTIONS]\n\
         \n\
         \x20 FILTER                 only run cases whose label contains this substring\n\
         \n\
         \x20 -o, --save <file>      save this run's report to <file>\n\
         \x20     --save-commit      save this run's report into itsy/bench/reports/, named after HEAD\n\
         \x20 -c, --compare <a> [b]  one name: run, then print results as % vs report <a>\n\
         \x20                        two names: compare report <b> against <a>, without running\n\
         \x20 -h, --help             show this help"
    );
}

/// Runs every case (optionally filtered) and returns one `Stats` per successful case.
fn run_all(cases: &[(String, PathBuf)], filter: Option<&str>) -> Vec<Stats> {
    let mut results = Vec::new();
    for (label, path) in cases {
        if let Some(filter) = filter {
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
    results
}

/// Writes a report, reporting success/failure to stderr.
fn save_report(path: &Path, results: &[Stats], meta: &ReportMeta) {
    match write_report(path, results, meta) {
        Ok(()) => eprintln!("\nreport saved to {}", path.display()),
        Err(err) => eprintln!("\nfailed to save report to {}: {err}", path.display()),
    }
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
/// selects the stateful (`bench(state)`) or stateless (`bench()`) shape. The `use BenchAPI::` line
/// imports `bench_iterations`; cases import any additional API functions/types they need.
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

/// Metadata stored in a report header.
struct ReportMeta {
    date: String,
    /// Short commit hash of HEAD, empty if git is unavailable.
    commit: String,
    /// Commit subject line, empty if git is unavailable.
    message: String,
    samples: usize,
}

impl ReportMeta {
    /// Gathers metadata for a report produced right now. Date and commit come from `git` when
    /// available; otherwise the date falls back to the local clock and commit/message are empty.
    fn current() -> ReportMeta {
        match git_head_info() {
            Some((commit, date, message)) => ReportMeta { date, commit, message, samples: SAMPLES },
            None => ReportMeta { date: today(), commit: String::new(), message: String::new(), samples: SAMPLES },
        }
    }
}

/// Returns `(short_hash, date, subject)` for HEAD, or `None` if `git` cannot be queried (e.g. not
/// a repository or no commits yet). Date is `YYYY-MM-DD` from the committer date.
fn git_head_info() -> Option<(String, String, String)> {
    let output = Command::new("git")
        .args(["log", "-1", "--format=%h%n%cI%n%s"])
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8_lossy(&output.stdout);
    let mut lines = text.lines();
    let hash = lines.next()?.trim().to_string();
    let date = lines.next()?.trim().chars().take(10).collect::<String>(); // first 10 of ISO 8601
    let subject = lines.next().unwrap_or("").trim().to_string();
    if hash.is_empty() { None } else { Some((hash, date, subject)) }
}

/// Builds the `itsy/bench/reports/<date>_<hash>_<message40>.txt` path for `--save-commit`.
fn fname_for_commit(meta: &ReportMeta) -> Result<PathBuf, String> {
    if meta.commit.is_empty() {
        return Err("could not read commit info from git (not a repository, or no commits yet)".into());
    }
    let slug = sanitize(&meta.message);
    let name = format!("{}_{}_{}.txt", meta.date, meta.commit, slug);
    Ok(Path::new(env!("CARGO_MANIFEST_DIR")).join("itsy/bench/reports").join(name))
}

/// Lowercases the first 40 characters of `s` into a filesystem-safe `[a-z0-9-]` slug.
fn sanitize(s: &str) -> String {
    let mut out = String::new();
    let mut prev_dash = false;
    for c in s.chars().take(40) {
        if c.is_ascii_alphanumeric() {
            out.push(c.to_ascii_lowercase());
            prev_dash = false;
        } else if !prev_dash {
            out.push('-');
            prev_dash = true;
        }
    }
    out.trim_matches('-').to_string()
}

/// Serializes a report: `#`-prefixed metadata header, then one tab-separated row per case holding
/// the raw per-iteration seconds. Creates parent directories as needed.
fn write_report(path: &Path, results: &[Stats], meta: &ReportMeta) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(|e| format!("create dir: {e}"))?;
        }
    }
    let mut out = String::new();
    out.push_str("# itsy-bench-report v1\n");
    out.push_str(&format!("# date: {}\n", meta.date));
    out.push_str(&format!("# commit: {}\n", meta.commit));
    out.push_str(&format!("# message: {}\n", meta.message));
    out.push_str(&format!("# samples: {}\n", meta.samples));
    out.push_str("# label\tmean\tmedian\tmin\tstddev\n");
    for s in results {
        out.push_str(&format!("{}\t{:e}\t{:e}\t{:e}\t{:e}\n", s.label, s.mean, s.median, s.min, s.stddev));
    }
    fs::write(path, out).map_err(|e| format!("write: {e}"))
}

/// Parses a report file back into its metadata and per-case stats.
fn read_report(path: &Path) -> Result<(ReportMeta, Vec<Stats>), String> {
    let text = fs::read_to_string(path).map_err(|e| format!("cannot read report {}: {e}", path.display()))?;
    let mut meta = ReportMeta { date: String::new(), commit: String::new(), message: String::new(), samples: 0 };
    let mut rows = Vec::new();
    for line in text.lines() {
        if let Some(rest) = line.strip_prefix('#') {
            let rest = rest.trim();
            if let Some(v) = rest.strip_prefix("date:") { meta.date = v.trim().to_string(); }
            else if let Some(v) = rest.strip_prefix("commit:") { meta.commit = v.trim().to_string(); }
            else if let Some(v) = rest.strip_prefix("message:") { meta.message = v.trim().to_string(); }
            else if let Some(v) = rest.strip_prefix("samples:") { meta.samples = v.trim().parse().unwrap_or(0); }
            continue;
        }
        if line.trim().is_empty() {
            continue;
        }
        let mut cols = line.split('\t');
        let label = cols.next().ok_or_else(|| format!("malformed row in {}: {line:?}", path.display()))?;
        let parse = |c: Option<&str>| -> Result<f64, String> {
            c.ok_or_else(|| format!("malformed row in {}: {line:?}", path.display()))?
                .trim().parse::<f64>().map_err(|e| format!("bad number in {}: {e}", path.display()))
        };
        let mean = parse(cols.next())?;
        let median = parse(cols.next())?;
        let min = parse(cols.next())?;
        let stddev = parse(cols.next())?;
        rows.push(Stats { label: label.to_string(), mean, median, min, stddev });
    }
    Ok((meta, rows))
}

/// Prints `subj` relative to `base`, matching by label. The `change` column is the signed percent
/// change in mean (`(subj/base - 1) * 100`); positive means `subj` is slower. Labels present on
/// only one side render `n/a`.
fn print_comparison(base: &[Stats], subj: &[Stats], base_name: &str, subj_name: &str) {
    let base_by_label: HashMap<&str, &Stats> = base.iter().map(|s| (s.label.as_str(), s)).collect();
    let subj_by_label: HashMap<&str, &Stats> = subj.iter().map(|s| (s.label.as_str(), s)).collect();

    // Union of labels, preserving subj order first, then base-only labels.
    let mut labels: Vec<&str> = subj.iter().map(|s| s.label.as_str()).collect();
    for s in base {
        if !subj_by_label.contains_key(s.label.as_str()) {
            labels.push(s.label.as_str());
        }
    }

    let name_width = labels.iter().map(|l| l.len()).max().unwrap_or(0).max("benchmark".len());
    println!("\nbaseline: {base_name}\ncurrent:  {subj_name}\n");
    println!(
        "{:<width$}  {:>11}  {:>11}  {:>9}",
        "benchmark", "current", "baseline", "change",
        width = name_width
    );
    for label in labels {
        let cur = subj_by_label.get(label);
        let bas = base_by_label.get(label);
        let cur_s = cur.map_or("n/a".to_string(), |s| fmt_secs(s.mean));
        let bas_s = bas.map_or("n/a".to_string(), |s| fmt_secs(s.mean));
        let change = match (cur, bas) {
            (Some(c), Some(b)) if b.mean > 0.0 => fmt_percent((c.mean / b.mean - 1.0) * 100.0),
            _ => "n/a".to_string(),
        };
        println!("{:<width$}  {:>11}  {:>11}  {:>9}", label, cur_s, bas_s, change, width = name_width);
    }
}

/// Formats a signed percentage, e.g. `+5.0%` (slower) or `-8.0%` (faster).
fn fmt_percent(p: f64) -> String {
    format!("{:+.1}%", p)
}

/// Current local date as `YYYY-MM-DD`, derived from the system clock without external crates.
fn today() -> String {
    let secs = SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs()).unwrap_or(0);
    let days = (secs / 86_400) as i64;
    let (y, m, d) = civil_from_days(days);
    format!("{:04}-{:02}-{:02}", y, m, d)
}

/// Converts a count of days since the Unix epoch to a `(year, month, day)` civil date. This is
/// Howard Hinnant's well-known `civil_from_days` algorithm (valid for the proleptic Gregorian
/// calendar over the range we care about).
fn civil_from_days(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097; // [0, 146096]
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365; // [0, 399]
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // [0, 365]
    let mp = (5 * doy + 2) / 153; // [0, 11]
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32; // [1, 31]
    let m = (if mp < 10 { mp + 3 } else { mp - 9 }) as u32; // [1, 12]
    (if m <= 2 { y + 1 } else { y }, m, d)
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

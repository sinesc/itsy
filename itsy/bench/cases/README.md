# Itsy micro-benchmark cases

Each `.itsy` file in this tree is a benchmark case run by the `bench` harness
(`benches/bench.rs`, executed via `cargo bench`). The directory layout is the registry:

- The folder structure categorizes cases. A file at `loops/while_count.itsy` has the
  label `loops/while_count`.
- To add a case, just drop a new `.itsy` file into an existing or new (sub)folder. No
  Rust code needs to change.

## Shape of a case file

A case defines `bench` (required) and optionally `setup`. It must **not** define `main` —
the harness generates `main` for you.

Stateless case:

```ignore
fn bench() {
    // workload to measure
}
```

Stateful case (shares a custom datatype between setup and the timed workload):

```ignore
struct State { /* ... */ }

fn setup() -> State {
    // built once, before timing
}

fn bench(state: State) {
    // workload to measure; receives the value setup() produced
}
```

## How state sharing works (no globals needed)

Itsy has no global variables, but `setup` can still hand arbitrary data — arrays, strings,
structs, any custom type — to `bench`. The harness generates a `main` like:

```ignore
fn main() {
    let state = setup();
    while true {
        suspend;                     // hand control back so the harness can time the batch
        let n = bench_iterations();
        for _ in 0..n { bench(state); }
    }
}
```

`setup` runs once; then the VM `suspend`s so the harness can start the clock and resume it.
Because `state` is a live local in `main`'s frame, it (and its heap data) survives across the
suspend point and is passed to every `bench` call. The state never crosses the Rust boundary,
so it is not limited to the primitive/string types that host calls support.

## Important: keep `bench` independent of past iterations

The same `state` value is reused for every repetition (and during warm-up). If `bench`
**mutates** `state`, iterations are no longer independent — e.g. an array that `bench` keeps
pushing to will grow without bound and skew timings. Treat `state` as read-mostly, or have
`bench` undo whatever it changes. The `structs/sum_points` case is an example of read-only use.

## Running

```bash
cargo bench                 # run every case
cargo bench -- fib          # run only cases whose label contains "fib"
```

## Saving and comparing reports

Runs can be saved and compared (see `../reports/README.md` for the file format):

```bash
cargo bench -- --save out.txt          # run, print, and save a report
cargo bench -- --save-commit           # save a report named after the current HEAD commit
cargo bench -- --compare base.txt      # run, then print results as % change vs base.txt
cargo bench -- --compare a.txt b.txt   # compare two saved reports (no run); b vs a
```

`bash scripts/install-bench-hook.sh` installs an opt-in git hook that runs `--save-commit` after
each commit.

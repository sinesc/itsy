# Itsy benchmark reports

Saved output of the `bench` harness (`benches/bench.rs`, run via `cargo bench`). Each report is a
snapshot of one benchmark run, kept in version control so performance can be tracked over time and
compared between commits.

## How reports get here

```bash
cargo bench -- --save <file>     # save a run to an explicit path
cargo bench -- --save-commit     # save a run named after the current HEAD commit, into this dir
```

`--save-commit` writes `<date>_<shorthash>_<message>.txt`, where the date, short hash and message
come from the HEAD commit (the message is the subject line, lowercased and truncated to a
filesystem-safe 40-character slug). To capture one automatically after every commit, install the
opt-in git hook:

```bash
bash scripts/install-bench-hook.sh
```

(The hook runs a full benchmark, which takes a while — it is opt-in for that reason.)

## File format

Plain text. A `#`-prefixed metadata header, then one tab-separated row per case holding the raw
per-iteration timings in **seconds** (mean, median, min, stddev):

```
# itsy-bench-report v1
# date: 2026-06-26
# commit: 3a82806c
# message: remove old ballpark bench, collapse one folder level
# samples: 50
# label	mean	median	min	stddev
arrays/push	3.41e-7	3.33e-7	3.20e-7	1.02e-8
recursion/fib	1.20e-3	1.19e-3	1.18e-3	8.0e-6
```

## Comparing

```bash
cargo bench -- --compare <base>            # run now, print results as % change vs <base>
cargo bench -- --compare <base> <other>    # compare two saved reports (no run); <other> vs <base>
```

Positive percentages mean slower than the baseline, negative means faster.

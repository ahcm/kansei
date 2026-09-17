# Performance baselines

Run on an idle machine with a fixed power profile:

```sh
python3 scripts/benchmark.py --output /tmp/kansei-before.json
# Make the change, then rerun on the same machine.
python3 scripts/benchmark.py --baseline /tmp/kansei-before.json --output /tmp/kansei-after.json
```

The runner uses the library test harness (`cargo test --lib`) with the pinned
release toolchain and no optional libraries, warms
up each case, calibrates batches to at least 20 ms, and records nine samples.
It reports medians in nanoseconds per operation, raw samples, build flags, CPU,
compiler, revision, dirty-worktree status, and lockfile/harness hashes. Compilation
and process startup are outside the timings. Compare multiple runs before drawing
conclusions from small differences. The checked-in sample is a reference from one
machine, not a performance requirement for other hardware.

Cases cover parsing 100 functions; cloning and resolving their AST; formatting
that source; arithmetic and array lookup in all three execution modes; and cold
versus warmed arithmetic and index caches. Execution cases reuse the same interpreter and
function with prebuilt arguments. They assert results before timing and require arithmetic to use compiled code
in the enabled modes. Indexing currently falls back to AST evaluation; those
cases explicitly identify that fallback. Cache cases verify that the warmed path records hits
and only one miss. Formatting metadata remains transient and separate from the
runtime AST and caches. Resolution timing includes AST cloning to prevent reuse
of an already-resolved tree.

`--max-regression 15` makes comparison fail if any median slows by more than 15%.
This is opt-in because shared CI workers have variable load. Comparisons reject
different hardware, compiler, features, flags, or benchmark workloads. Review the
lockfile hashes when dependencies change. Ordinary tests skip benchmarks, and
production builds contain no benchmark instrumentation.

The sample `baseline-linux-x86_64.json` was recorded at commit `8adde40` on an
AMD Ryzen 9 7945HX3D. Repeating unchanged code produced parsing medians of roughly
169–217 microseconds per 100 functions, while the execution/cache medians in the
last two runs differed by less than 2%. A 15% threshold correctly flagged the
parsing variation. These observations are measurement variability, not evidence
of a code regression. On Linux, pinning the run to one allowed logical CPU with
`taskset -c <cpu> python3 scripts/benchmark.py ...` can help control migration;
use the same affinity and power settings for both runs.

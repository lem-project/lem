# T10: perceived-speed gate

The benchmark suite measures editor-side work in a fake frontend, using a
10,000-line buffer: command dispatch (next/previous line), insertion through full
redisplay, a real `le-themes-ergonomic` install/uninstall cycle, and empty transaction
commit. The theme benchmark includes applying the ergonomic theme and restoring
the previous theme and palette source. It refuses to uninstall a session-owned
active/suspended component and compensates failed activation. Only the expected
palette override warning is muffled during measurement, so terminal log writes do
not dominate the theme timing; unexpected warnings remain visible.

Each workload warms up 200 iterations and runs three measured rounds. The report
contains median p50/p95/p99 across rounds, in microseconds. The clock has microsecond
resolution; individual empty commits may measure zero. Results describe editor-side
cost, excluding physical terminal I/O and frontend/compositor latency.

The comparator rejects empty, incomplete, malformed, negative, unordered or
incompatible results, including mismatched iteration counts. It fails when p95 is
more than 20% above the baseline **and** the absolute increase is more than 2 us
(the existing calibrated noise floor). Exactly 20% is allowed. A zero baseline
produces an absolute-delta report, never division by zero. Existing absolute budgets
also apply: keystroke-to-frame 33 ms, dispatch 150 ms, empty commit 10 us.

```sh
sbcl --script scripts/run-yeditor-bench.lisp head.json [base.json]
# Record baseline data without applying head's pass/fail policy:
sbcl --script scripts/run-yeditor-bench.lisp --measure-only base.json
bash scripts/bench-gate.sh BASE_REF bench-out
```

The Docker gate measures both revisions on the same runner. It exports the base
through `git archive`, without creating a worktree, then copies only the **head
measurement harness** (benchmark Lisp, ASDF definition and CLI runner) into the
base archive. Production code and dependency locks remain from the base revision.
Both versions therefore measure the same real theme, including the first PR that
introduces that workload. A base predating the suite explicitly uses budgets-only
mode; a broken baseline that has the suite is a failure, never silently skipped.

Artifacts from previous invocations are removed first. Compiler/runtime failures
and missing/invalid baseline data produce a failing exit status. `report.txt`,
`base.log`, `head.log`, `metadata.txt` and the successful JSON measurements are
published by the existing `benchmark-report` upload step with `if: always()`.
A failure in baseline preparation/build/measurement leaves a stage-specific FAIL
report. Shell arguments remain separate, including output directories with spaces.

Validation combines:

- Rove comparison tests for the 20% boundary, absolute budgets, zero baseline,
  noise floor, incomplete results, invalid timings and mismatched sample counts.
- 100 real theme cycles asserting activation, theme/colors/palette restoration and
  inactive registry state; failed activation and borrowed session-state checks.
- Four shell contract tests injecting a broken baseline, a regression, an old base
  and a successful run, with stale artifacts and paths containing spaces.
- Actual benchmark measurements and the full Yeditor suite. `yeditor-bench-tests`
  is included in `scripts/run-yeditor-all-tests.lisp`; shell tests run before image
  building in the CI workflow.

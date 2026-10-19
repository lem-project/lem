# T4: reversible buffer, mode and hook effects

Issue #4 is implemented in the production `yeditor-effects` system. The T4
acceptance suite uses real Lem buffers and a fake frontend, without a graphical
window or user interaction.

## Ownership and activation

- `install-buffer` resolves ownership at commit and on every resume. An existing
  buffer is borrowed and survives rollback. A buffer created by the effect is
  deleted on rollback, including when it is the only registered buffer. Cleanup
  uses Lem's public deletion API rather than the interactive kill command's
  last-buffer guard.
- `install-mode` accepts a buffer object or a buffer name resolved at activation.
  This permits one component to queue buffer creation, mode activation and hook
  registration in order. Major-mode undo restores the mode present immediately
  before activation. Minor modes are enabled explicitly and preserve an existing
  activation. The previous function-valued major-mode callback API remains.
- `install-hook` preserves pre-existing callbacks and weights, and removes only
  its own registration. The callback is stored under `:callback`; `:fn` remains
  reserved for the transaction installer so suspension/resumption reinstalls the
  hook rather than invoking its callback.

These are editor-context operations. They do not provide synchronization for
concurrent mutations or reference-counted ownership across independent owners.

## Tests

From the repository root, with SBCL and Qlot dependencies available:

```sh
sbcl --script scripts/run-yeditor-effects-tests.lisp
sbcl --script scripts/benchmark-t4.lisp
```

The effects runner executes T4, T5 and T6 and now propagates Rove failures as a
nonzero process exit. T4's ASDF test operation also signals failure when Rove
returns NIL. A controlled failing assertion in a separate temporary Lisp process
was used to verify the negative path; it exited with status 1 as expected.

T4 includes the original smoke test and six regression cases: borrowed buffers,
sole owned buffers, pre-existing hooks, commit-time major-mode baselines,
minor-mode restoration and a component combining all three effect types. The
combined component runs 300 full suspend/resume pairs. Each suspension directly
checks the component registry, buffer list, hook list and current buffer; each
resume checks active resources and stable handle identity. Final uninstall
leaves no component-owned resources.

## Buffer installation benchmark

`benchmark-t4.lisp` performs 200 warmups and then measures 10,000 fresh
installations. Each sample includes effect registration, transaction commit and
actual buffer creation. Rollback runs outside the timed interval and verifies
cleanup after every sample. No frontend or redisplay is started. Percentiles are
nearest-rank p50/p95/p99 from microsecond wall-clock samples; a backwards clock
invalidates the run, and p95 >= 50 microseconds exits nonzero.

On the current cloud machine with SBCL 2.5.2: **p50 1 µs, p95 2 µs, p99 5 µs**.
This meets T4's p95 < 50 µs target on this machine. Shared-runner timing can vary;
the CI test container also runs this benchmark and enforces the same budget.

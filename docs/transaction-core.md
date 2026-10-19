# Transactional core (T1–T2, issues #1–#2)

`yeditor-tx` is an independent ASDF system. It queues reversible effects,
commits them in registration order, and rolls them back in strict LIFO order.
`yeditor-context` supplies the L2 entry point `effect`; it depends only on L1.
Neither system requires Lem, a frontend, or Quicklisp at runtime.

```lisp
(let ((log (yeditor-tx:make-transaction-log))
      (value 0))
  (yeditor-tx:with-transaction (log)
    (yeditor-context:effect :example :variable
                           (lambda () (incf value))
                           :undo-thunk (lambda () (decf value))))
  (yeditor-tx:commit log)   ; value = 1
  (yeditor-tx:rollback log) ; value = 0
  (yeditor-tx:log-records log))
```

`install-effect` and `effect` return opaque handles. They require an open
transaction and a function-valued `undo-thunk`, captured before installation.
Missing inverses are rejected before any effect runs. Automatic inverses for
standard editor operations belong to the later effect-adapter tasks.

Commit records successful effects as alists with `:kind`, `:owner`, `:payload`
(the installer's primary return value), `:inverse` and `:ts`. Timestamps use the
process monotonic clock in `internal-time-units-per-second`. Records remain for
diagnostics after rollback. `log-records` copies list structure, retaining
payload/function identity. T1's low-level `record-effect` remains audit-only;
it does not register work or compensation with the transaction lifecycle.

## Lifecycle and failures

The normal lifecycle is `:open` → `:committed` → `:rolled-back`. Repeating a
successful commit or rollback does nothing. Rolled-back logs cannot be reused.
A transaction scope selects a log; leaving the scope does not auto-commit.
`undo-effect` cancels a pending handle or uninstalls an active handle once.
Individual uninstalls are suitable for independent effects; dependent effects
must be undone as a group in reverse order.

A commit error or nonlocal exit compensates every attempted installer, including
one that failed after partially changing state. Inverses must therefore tolerate
partial installation. If one inverse signals an error, rollback still attempts
the others and signals `rollback-failure` with `rollback-errors` and the original
`rollback-cause`, when present. The log becomes `:rollback-failed`; an explicit
rollback retry executes only failed inverses. An inverse that partially mutates
before failing must itself be safely retryable. Inverses should return normally
or signal errors, rather than transfer control with `throw`/`return-from`.

Installers and inverses cannot recursively commit, roll back or change membership
in their own log. Logs are owned by one execution context, not thread-safe. These
transactions compensate in-process callbacks; they cannot undo process crashes,
external irreversible operations, or arbitrary callbacks lacking correct inverses.
Durability and editor-specific inverse adapters are separate tasks.

## Workspace side-transactions

Inside an ordinary transaction, `with-workspace-transaction (owner)` collects a
kind-2 child and registers it as one parent effect. Child effects commit in order
and roll back as a block in LIFO order. Failure inside the child compensates the
child and then the previously installed parent effects.

The grouping layer never calls or clears Lem's native buffer undo. Buffer
callbacks must follow the editor's undo protocol themselves. The integration
test appends reversible edits using `lem:with-inhibit-undo` and checks that both
pre-existing native undo and redo still work after rolling back the group, over
100 cycles. This is not a general snapshot adapter for arbitrary buffer edits,
text properties, hooks, modified flags, or concurrent human changes.

## Validation

From the repository root (source `/workspace/.cloud-tools/activate.sh` first
when using the onboarding toolchain):

```sh
sbcl --noinform --script scripts/test-tx.lisp
sbcl --noinform --script scripts/benchmark-tx.lisp
```

The dependency-free ASDF tests retain the T1 smoke tests and cover commit order,
LIFO rollback, pending cancellation, partial failure, nonlocal exit, failed-inverse
retry, idempotence, nested workspace failure and reentrancy. The property check
runs 20 deterministic seeds, each with 100 install/cancel operations followed by
commit, individual uninstalls and rollback to a nonzero baseline.

With Qlot dependencies installed, run the Rove integration tests:

```sh
qlot exec sbcl --noinform --non-interactive --load .qlot/setup.lisp \
  --eval '(ql:quickload "yeditor-tx/lem-tests")' \
  --eval '(asdf:test-system "yeditor-tx/lem-tests")'
```

The benchmark reports three runs of amortized CPU time per empty transaction
scope, not durable commit latency or an individual wall-clock tail percentile.

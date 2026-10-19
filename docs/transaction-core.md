# Transactional core (T1, issue #1)

`yeditor-tx` is an independent ASDF system with no editor or Quicklisp
prerequisites. It records effects in memory; real commit, rollback, persistence,
editor adapters and thread coordination belong to later tasks.

```lisp
(let ((log (yeditor-tx:make-transaction-log)))
  (yeditor-tx:with-transaction (log)
    (yeditor-tx:record-effect :noop :example '("payload") (lambda () nil)))
  (yeditor-tx:log-records log))
```

Each record is an alist with `:kind`, `:owner`, `:payload`, `:inverse` and `:ts`.
Timestamps use the process monotonic clock in `internal-time-units-per-second`.
Effects are logged immediately, including before an error. Nothing invokes an
inverse yet. Nested transaction scopes restore the enclosing destination, and
body return values propagate. Use a log from one execution context only.
Snapshots copy list structure; payloads and functions keep their identity.

From the repository root (source `/workspace/.cloud-tools/activate.sh` first
when using the onboarding installation):

```sh
sbcl --noinform --non-interactive --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "yeditor-tx.asd"))' \
  --eval '(asdf:test-system "yeditor-tx")'
sbcl --noinform --script scripts/benchmark-tx.lisp
```

The dependency-free ASDF tests use Common Lisp assertions so the isolated L1
system remains testable before Qlot/editor setup. They check 100-effect
retention and order, metadata, independent logs, nested scope restoration,
return values and error behavior. An assertion failure exits SBCL nonzero.
The benchmark reports three runs of amortized CPU time per empty transaction
scope in batches, not a durable commit or individual wall-clock tail latency.
The issue's 10 microsecond target applies only to this provisional empty scope.

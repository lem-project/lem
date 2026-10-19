# T9: effects journal and crash recovery

The terminal launcher (`scripts/run-yeditor-ncurses.lisp`) starts durability in
Lem's after-init hook, after definitions and the initial frame are available and
before its normal component installs. It surfaces recovery errors and ignored
record counts through Lem messages. Loading an ASDF library alone does not read
a user's journal or install components. Other session initializers can call:

```lisp
(yeditor-context/durability:start-durable-session)
```

`start-durable-session` enables the observer, calls `recover`, returns its report
and leaves journaling enabled. Each API also accepts `:path` to select a journal; otherwise the path is
`$XDG_DATA_HOME/yeditor/effects.log` (using UIOP's XDG fallback). Keep durability
enabled during subsequent session changes. `disable-durability` restores the
previous lifecycle observer and preserves an observer replaced by another owner.
Enabling repeatedly does not duplicate records. The library itself installs no global Lem boot hook; the terminal launcher owns
that wiring. Custom initializers should call after loading definitions and the
session's configuration/context.

Each JSONL record contains a universal-time `ts`, an `event`, and a package-qualified
component id. Events are `install-begin`, `install-commit`, `install-abort` (failed
installation and rollback), `install-cancelled` (interrupted installation),
`uninstall`, `suspend`, and `resume`. Runtime closures and inverse functions are
never serialized or evaluated from the journal.

Writes are serialized in this process and flushed before the lifecycle operation
returns. Encoding happens before opening the output file. An incomplete final
record is preserved; the next append inserts a separator so it cannot swallow the
next event. Invalid JSON, invalid record shapes and unknown events cannot replace
valid state, and are counted in the third return value of `replay-log` and in
`:ignored-records` from `recover`. UTF-8 tails split inside a codepoint are tolerated.
A journal belongs to one process; this is process-crash recovery, not a claim of
power-loss persistence, cross-process locking or undoing arbitrary external writes.

`replay-log` returns live components in their last activation order, preserving
newer override layers, and interrupted installs separately. Recovery records a
cancellation for each interrupted install and never reruns it. It reinstalls only
registered, inactive `:persist t` components, constructing fresh effects and
inverses against the new session baseline. `install-effect` rejects a missing or
non-callable inverse before queuing a mutation. Existing active or suspended
components, including zero-effect components, belong to the current session and
are preserved. Unknown/nonpersistent components are skipped. Session configuration
is reconstructed separately, not restored from stale executable data in the log.

Recovery journals its own lifecycle transitions to its explicit path even when
normal durability is disabled or points elsewhere, without duplicating events or
changing the enabled observer. A failed reinstall records `install-abort`; its id
appears in `:skipped` and its condition in `:errors`, so it cannot remain falsely
committed. The result also contains `:reinstalled` and `:cancelled` id lists.
Callers should surface recovery errors and ignored-record counts to their operator.

## Validation

`durability-tests` exercises:

- A real child SBCL killed externally after applying an effect with its inverse
  captured, before the commit record. A second SBCL process asserts cancellation,
  no reinstallation of the interrupted component, restoration of a committed
  persistent component with a fresh inverse, and clean state after uninstall. The
  parent checks the exact log events.
- Truncated JSON and UTF-8 writes followed by recovery and a second replay.
- Unknown/malformed records, last activation order and actual overlapping effects
  restored/uninstalled in LIFO order to their original baseline.
- Observer ownership/idempotence, explicit recovery path isolation, active and
  suspended empty components, failed reinstall audit and rejection without inverse.
- 100 install-from-journal/uninstall cycles, checking the exact baseline each time.
- 10,000 records (5,000 committed component ids) through replay **and reconciliation**,
  checking all 5,000 are skipped when unavailable and median elapsed time is <100 ms.
  This budget excludes SBCL/dependency startup and running component installers.

The normal `scripts/run-yeditor-all-tests.lisp` runner includes this suite.
`asdf:test-system :durability-tests` also runs assertions and signals on failure.

# T8: configuration and hot reload

Component definitions must already be loaded. Configuration forms are read as
standard s-expressions, never evaluated, using a fresh standard readtable and
`*read-eval*` disabled:

```lisp
(use-component my-package:my-component :config (:delay 2))
(use-component "le-project")
```

A string resolves to one registered component name; ambiguous names require a
package-qualified symbol. Explicit `:config` is normalized to the same option
plist as the legacy `(use-component name :delay 2)` syntax. Plists must have
unique keyword keys, and every supplied option must appear in the component's
`:config-spec` (currently a list of allowed keyword names). Validation checks
keys and shape; this v1 does not provide a type-schema language or evaluate values.
The installer receives options as `(getf context :config)`.

`apply-config-file` streams forms from a file and returns per-component error
strings from reconciliation. Syntax/read failures signal before any delta is
applied. Each replacement is validated before the previous configuration is
removed. If installation of a replacement fails, the previous handles and options
are restored. The delta remains independent per component; a valid change to
another component can succeed while one replacement is rejected.

Components already active or suspended outside this loader are borrowed, so
removing their declaration preserves them. A loader cannot silently change their
options. Owned conditional components respect their coeffects, retain options
across conditional reinstalls, and remain disabled after removal from the config.
Newer retained effect layers are temporarily suspended/resumed in LIFO order when
an older layer changes; their component installer is not called again and their
handles are retained. Unchanged components remain installed.

```lisp
(yeditor-context/loader:apply-config-file #p"/path/to/.yeditor.lisp")
(yeditor-context/loader:start-config-watcher #p"/path/to/.yeditor.lisp")
(yeditor-context/loader:stop-config-watcher)
```

The watcher defaults to a two-second interval. A dedicated worker reads snapshots
of mtime and content, confirms stability briefly (at most 20 ms), and enqueues the
snapshot for parsing/reconciliation on the editor thread. This detects same-size
rewrites within one timestamp tick and avoids applying brief partial overwrites.
Atomic file replacement remains preferable for writers; a valid snapshot held
long enough to be observed is a legitimate desired configuration. Deleted files
are treated as empty configurations. Stopping/restarting invalidates older queued
callbacks. Optional `:on-error` receives explicit messages on the editor thread;
the default displays them with `lem:message`.

## Validation

`loader-tests` includes real polling and editor-queue integration, not only direct
calls to the reconciliation function:

- 50 runtime file rewrites, asserting the complete component delta after every
  step and verifying that an unchanged component is installed exactly once.
- Invalid option reporting through the UI callback while retaining the last
  valid value; deleting the file removes owned components.
- Option delivery, malformed declarations, duplicate options, standard reader
  isolation, reader evaluation rejection, and both config syntaxes.
- Invalid and failing replacements restore previous state; borrowed components
  survive removal; conditional components retain options and stay removed.
- Layered overrides retain handles and restore the original resource baseline.
- Same-tick content changes are detected; stopping cancels queued snapshots.
- 100 configured install/remove cycles restore the effect baseline and empty the
  applied-component map after every cycle.
- Updating an inactive conditional component does not replay unrelated effects.

Run from the configured repository root:

```sh
sbcl --script scripts/run-yeditor-all-tests.lisp
```

The focused ASDF test target is `loader-tests`; it now signals an error when Rove
reports failure. The complete Yeditor suite and agent smoke runner provide
regressions for the T7 context bridge, transactions, effects, LSP and MCP/HUD flows.
The fake frontend exercises the real core and local resources; graphical builds
are validated separately by GitHub CI.

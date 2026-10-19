# T5: reversible keymap, variable, command, overlay and timer effects

Keybindings, variables and commands capture their baseline when the transaction
commits, and again when a suspended component resumes. Variable overrides warn
when replacing a bound value and must unwind in LIFO order. Keybindings restore
prefix commands and remove newly created prefix paths, preserving unrelated
bindings. Overlay and timer inverses release their owned references; timer undo
also invalidates callbacks already queued by that activation. Timer shutdown uses
Lem's asynchronous stop operation and does not wait on the UI thread.

The regression suite exercises all five effects in one component through 200
suspend/resume pairs and 200 fresh install/uninstall cycles. Assertions cover
actual command execution, real timer notification and execution, suppression of
callbacks after undo and after resume, exact overlay count, variable value and
keybinding restoration, and repeated uninstall. Additional cases cover baseline
changes between registration and commit, nested variable overrides with warnings,
and 200 key-sequence cycles for both absent and command-bearing prefixes.

Run from the repository root after activating the cloud toolchain:

```sh
. /workspace/.cloud-tools/activate.sh
sbcl --script scripts/run-yeditor-effects-tests.lisp
```

Validated locally with SBCL 2.5.2: T4, T5 and T6 effect suites, plus component
regressions for feature toggles and agent HUD mount/unmount. Before the fixes,
the new tests failed on stale snapshots, missing variable override warnings,
incorrect inner-variable restoration, queued timer callbacks, and prefix residue.
The T5 ASDF test operation now signals failure when Rove returns false.

The local checks use a headless editor and a test timer manager that receives real
worker notifications. They do not validate a graphical frontend or replace the
full GitHub build matrix.

# Reversible components (T3, issue #3)

`yeditor-prototype-sdk` is an independent ASDF system above `yeditor-prototype-context` and
`yeditor-prototype-tx`. It provides registration, installation, suspension, resumption
and uninstall without depending on a Lem frontend.

```lisp
(defvar *example-enabled* nil)

(yeditor-prototype-sdk:define-component example
    (:configuration '(:enabled t)
     :documentation "An example reversible component.")
  (:install (component context)
    (declare (ignore context))
    (let ((previous nil))
      (yeditor-prototype-sdk:effect
       component :variable
       (lambda ()
         (setf previous *example-enabled*
               *example-enabled* t))
       :undo-thunk (lambda () (setf *example-enabled* previous))))))

(yeditor-prototype-sdk:install-component 'example :context '(:workspace :main))
(yeditor-prototype-sdk:toggle-component 'example) ; suspend: restore the old value
(yeditor-prototype-sdk:toggle-component 'example) ; resume: same handles, effects reapplied
(yeditor-prototype-sdk:uninstall-component 'example)
```

`define-component` accepts exactly one `(:install (component context) ...)`
clause. Its options are evaluated at registration, and the install body runs
once per fresh installation to queue reversible effects. All mutations belong
inside deferred effect callbacks, with inverses that tolerate partial installs.
The SDK cannot compensate arbitrary state mutations performed directly by the
install body. Installers/inverses must not perform blocking I/O on the UI thread.

## Registry and lifecycle

`*component-registry*` holds the default registry. Bind it to `(make-registry)`
for an isolated workspace or test; lifecycle functions also accept `:registry`.
Names are symbols, preserving package identity. Duplicate registrations are
rejected rather than overwrite a component that may own live resources.
Hot redefinition/reconciliation belongs to the config-loader task (T8).

`register-component` takes a name and `(lambda (component context) ...)`,
with optional `:configuration`, `:documentation` and `:registry`.
`find-component` returns the definition/instance, or NIL. Metadata readers
are `component-name`, `component-state` and `component-configuration`.

- `install-component`: create a private transaction, queue the install body's
  effects, commit in order and mark the component `:active`. Repeating an active
  install is idempotent. A failed definition/commit compensates effects before
  reporting the error, returning to `:registered` when cleanup succeeds.
- `suspend-component`: undo effects in LIFO order and mark `:suspended`.
  Definition, configuration, context, installer closures and handles remain.
- `resume-component`: replay the saved installers with the same handle objects.
  It does not rerun the definition body or allocate replacement handles.
- `toggle-component`: suspend an active component, resume a suspended one, or
  install a registered/uninstalled one. Its `:context` is used only on a fresh
  install; toggling never replaces an existing context.
- `uninstall-component`: perform final rollback, mark `:uninstalled` and release
  the runtime transaction/handles/context. The definition/configuration remain
  available for a fresh install. A fresh install creates new handles.

For an active/suspended component, `install-component` rejects a different
explicit context; uninstall first to replace it. Component transitions in one
registry cannot be nested: an install body or inverse cannot recursively toggle,
register, install or otherwise change registry membership/lifecycle. Registries/logs require one execution context;
these guards are not cross-thread synchronization.

## Active log and stable identity

`component-handles` returns a fresh list containing the current installation's
handle objects in registration order. Its list can be mutated without changing
the component's membership.

`registry-log` returns an active snapshot across the registry in definition order,
then per-component effect order. Each entry is an alist containing `:component`,
`:handle`, `:state` and `:record` (the T1/T2 effect metadata). A failed inverse
remains visible as `:undo-failed`; fully suspended/uninstalled components have
no active entries. Lists/record spines are copied, while payloads and handles
retain their identity. This is an active view derived from the owned
transactions, not an append-only audit journal or global rollback order.

Transaction `suspend`/`resume` opt into replay separately from final `rollback`.
They retain handle identity, replay only effects active at suspension, and replace
previous activation records, keeping the per-transaction log bounded. Effects
cancelled or individually uninstalled before suspension are not resurrected.
Nested workspace groups follow the same replay rules. A final rollback is still
terminal, as specified by T2. Capture/recheck inverse state inside the installer
callback on each activation when the external state can change while suspended.

## Failures and recovery

A compensated resume failure leaves the component `:suspended` and retryable.
A failed inverse leaves it `:failed` and keeps residual handles visible in the
active log. No automatic cleanup retry conceals an inverse failure.

After repairing the underlying prerequisite, retry `suspend-component` for a
failed suspension/replay compensation, or `uninstall-component` for final cleanup.
A resume is rejected until suspension cleanup succeeds. Inverses that partially
mutate before failing must be safely retryable, as in T2. Error and nonlocal-exit
paths release the registry transition guard.

## Validation

From the repository root, source `/workspace/.cloud-tools/activate.sh` when using
the cloud toolchain:

```sh
sbcl --noinform --script contrib/transaction-core-prototype/scripts/test-tx.lisp
sbcl --noinform --script contrib/transaction-core-prototype/scripts/test-sdk.lisp
qlot exec sbcl --noinform --non-interactive --load .qlot/setup.lisp \
  --eval '(ql:quickload "yeditor-prototype-sdk/rove-tests")' \
  --eval '(asdf:test-system "yeditor-prototype-sdk/rove-tests")'
```

The dependency-free SDK acceptance suite runs 1,000 full suspend/resume pairs
for a ten-effect component, checking clean state/zero active entries at every
suspend and identical handles/ten entries at every resume. It also checks
configuration/context retention, failures, retries, registry isolation and
nested workspace replay. Rove runs the same behavioral suite in the existing
Qlot test environment. Both the SBCL CI job and Docker test command include T3.
The existing Lem buffer integration suite remains a separate regression check.

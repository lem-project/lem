# Independent transaction-core prototype (MIT)

This directory preserves the T1–T3 implementation developed in the cloud session
based on main at 77d936df. When integrating, develop already contained a different
production implementation of T1–T19 under yeditor/core/, yeditor/effects/,
yeditor/components/ and yeditor/enterprise/.

The production implementation and its licensing remain unchanged. To avoid
redefining its ASDF systems and packages, this prototype uses the distinct names
`yeditor-prototype-tx`, `yeditor-prototype-context` and `yeditor-prototype-sdk`.
It is not loaded by the production `yeditor` system. Its original MIT SPDX headers
are retained. All original source commits remain in the merge history.

Run from the repository root:

```sh
sbcl --noinform --script contrib/transaction-core-prototype/scripts/test-tx.lisp
sbcl --noinform --script contrib/transaction-core-prototype/scripts/test-sdk.lisp
```

The Qlot/Rove systems are `yeditor-prototype-tx/lem-tests` and
`yeditor-prototype-sdk/rove-tests`. See docs/ for their APIs and validation scope.
The production test runner remains scripts/run-yeditor-all-tests.lisp.

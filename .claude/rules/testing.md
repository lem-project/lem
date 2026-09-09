# Testing in Lem

## Test Framework

Lem uses Rove for testing.

## Running Tests

```bash
# All tests
make test

# Specific system
.qlot/bin/rove lem-tests.asd

# In REPL
(asdf:test-system "lem-tests")
```

## Writing Tests

```lisp
(defpackage :lem-my-mode/tests
  (:use :cl :rove :lem))
(in-package :lem-my-mode/tests)

(deftest test-feature
  (ok (= 1 1))
  (ng (= 1 2))
  (ok (equal "a" "a")))
```

## Test System Definition

```lisp
(defsystem "lem-my-mode/tests"
  :depends-on ("lem-my-mode" "rove")
  :components ((:file "tests/main"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
```

## CI

Tests run via GitHub Actions on every PR. See `.github/workflows/test.yml`.

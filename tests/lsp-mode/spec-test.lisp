(defpackage :lem-tests/lsp-mode/spec-test
  (:use :cl :rove)
  (:import-from :lem-lsp-mode/spec
                :get-language-spec
                :register-language-spec
                :spec)
  (:import-from :lem-lsp-mode
                :find-workspace))
(in-package :lem-tests/lsp-mode/spec-test)

;;;; Regression tests for the no-spec / nil-language-id path that the
;;;; lsp-mode hardening change made tolerant.  Both functions are called
;;;; transitively by lsp-mode hooks attached to every buffer, so any
;;;; regression here crashes the editor command loop on the next keystroke
;;;; in a buffer whose major mode has no language spec (e.g. REPL
;;;; transcripts).

(deftest get-language-spec-returns-nil-for-unregistered-mode
  (testing "unknown mode -> nil (callers use when-let against this)"
    (let ((mode (gensym "UNREGISTERED-MODE-")))
      (ok (null (get-language-spec mode))
          "Unregistered modes must return nil, not signal an assertion")))

  (testing "registered mode -> the spec instance"
    (let ((mode (gensym "REGISTERED-MODE-"))
          (spec (make-instance 'spec
                               :mode 'dummy-mode
                               :language-id "dummy"
                               :connection-mode :stdio)))
      (register-language-spec mode spec)
      (ok (eq spec (get-language-spec mode))
          "A registered spec must round-trip through get-language-spec"))))

(deftest find-workspace-tolerates-nil-language-id
  ;; Before the fix, find-workspace defaulted to :errorp t and signaled
  ;; "The NIL workspace is not found." on the first keystroke in a
  ;; buffer whose major mode had no spec.  The :errorp nil path was
  ;; already nil-safe; only the default-errorp case regressed.
  (testing "nil language-id with :errorp t -> nil (was an error)"
    (ok (null (find-workspace nil :errorp t))
        "A nil language-id means the buffer is not an LSP buffer, not a failure")))

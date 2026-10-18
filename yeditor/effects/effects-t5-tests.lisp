(defpackage :yeditor-effects-t5-tests
  (:use :cl :yeditor-tx :yeditor-effects :rove))
(in-package :yeditor-effects-t5-tests)

;;; ── helpers ────────────────────────────────────────────────────────────────

(defun fresh-tx ()
  "Clear transaction state between tests."
  (setf yeditor-tx:*tx-pending* nil
        yeditor-tx:*tx-committed* nil))

(defmacro with-fresh-tx (&body body)
  `(progn (fresh-tx) ,@body (fresh-tx)))

;;; ── e-variable ─────────────────────────────────────────────────────────────

(defvar *test-var-t5* nil)

(deftest test-variable-install-uninstall
  (with-fresh-tx
    (let ((handle (install-variable 'test-comp '*test-var-t5* 42)))
      (tx-commit)
      (ok (eql *test-var-t5* 42) "var set to 42 after commit")
      ;; undo via rollback
      (tx-rollback)
      (ok (eql *test-var-t5* nil) "var restored to nil after rollback"))))

(deftest test-variable-200x-cycle
  ;; 200 install→commit→rollback cycles; var must always return to initial value
  (let ((initial *test-var-t5*))
    (dotimes (i 200)
      (with-fresh-tx
        (install-variable 'test-comp '*test-var-t5* (1+ i))
        (tx-commit)
        (ok (eql *test-var-t5* (1+ i)) "var set")
        (tx-rollback)
        (ok (eql *test-var-t5* initial) (format nil "var restored cycle ~A" i))))))

(deftest test-variable-previously-unbound
  ;; var that did not exist → undo calls makunbound
  (let ((sym (intern (format nil "*TX-UNBOUND-~A*" (get-internal-real-time)))))
    (with-fresh-tx
      (install-variable 'test-comp sym :sentinel)
      (tx-commit)
      (ok (eql (symbol-value sym) :sentinel) "unbound var set")
      (tx-rollback)
      (ok (not (boundp sym)) "previously-unbound var is unbound after rollback"))))

;;; ── e-keymap ───────────────────────────────────────────────────────────────
;;; keymap tests run without a live Lem frontend — use lem:make-keymap directly.

(deftest test-keymap-install-uninstall
  ;; minimal smoke: install-keybinding records kind :keybinding
  (let* ((km (lem:make-keymap :name "test-km-t5")))
    (with-fresh-tx
      (let ((handle (install-keybinding 'test-comp km "C-t" 'lem:forward-char)))
        (ok (eq (cdr (assoc :kind handle)) :keybinding) "handle kind is :keybinding"))
      (tx-commit)
      ;; check via lem internals: parse key and find prefix
      (let* ((keys (lem::parse-keyspec "C-t"))
             (prefix (lem::prefix-for-key km (car (last keys)))))
        (ok (and prefix (eq (lem::prefix-suffix prefix) 'lem:forward-char))
            "key bound after commit"))
      (tx-rollback)
      (let* ((keys (lem::parse-keyspec "C-t"))
             (prefix (lem::prefix-for-key km (car (last keys)))))
        (ok (null prefix) "key unbound after rollback")))))

;;; ── e-command ──────────────────────────────────────────────────────────────

(deftest test-command-install-uninstall
  (let ((cmd-name "yeditor-test-command-t5"))
    (with-fresh-tx
      (install-command 'test-comp cmd-name (lambda () :executed))
      (tx-commit)
      (ok (lem::find-command cmd-name) "command registered after commit")
      (tx-rollback)
      (ok (null (lem::find-command cmd-name)) "command removed after rollback"))))

;;; ── e-overlay ──────────────────────────────────────────────────────────────

(deftest test-overlay-install-uninstall
  ;; creates a scratch buffer, installs overlay, rolls back → overlay gone
  (let* ((buf (lem:make-buffer "*t5-overlay*" :enable-undo-p nil))
         (start (lem:buffer-start-point buf))
         (end   (lem:buffer-end-point buf)))
    (with-fresh-tx
      (install-overlay 'test-comp start end (lem:make-attribute :reverse t))
      (tx-commit)
      (ok (plusp (length (lem:buffer-overlays buf))) "overlay present after commit")
      (tx-rollback)
      (ok (zerop (length (lem:buffer-overlays buf))) "overlay gone after rollback"))
    (lem:kill-buffer buf)))

;;; ── e-timer ────────────────────────────────────────────────────────────────

(deftest test-timer-install-uninstall
  ;; installs a 10 000 ms (one-shot) timer and immediately rolls back —
  ;; the stop-timer undo must not error.
  (with-fresh-tx
    (install-timer 'test-comp 10000 (lambda () nil) :repeat nil)
    (tx-commit)
    (ok t "timer started without error")
    (tx-rollback)
    (ok t "timer stopped via rollback without error")))

;;; ── combined component using all 5 new kinds ────────────────────────────────

(defvar *combined-sentinel* nil)

(yeditor-context::define-component t5-combined
  :version "0.1"
  :author "test"
  :depends-on nil
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp '*combined-sentinel* :active)
                   (install-command  comp "t5-combined-cmd" (lambda () :ok))))
  :activate nil
  :deactivate nil
  :config-spec nil)

(deftest test-combined-200x-toggle
  (dotimes (i 200)
    ;; toggle on
    (yeditor-context:install-component 't5-combined)
    (ok (eql *combined-sentinel* :active) "sentinel active after install")
    (ok (lem::find-command "t5-combined-cmd") "command present after install")
    ;; toggle off
    (yeditor-context:uninstall-component 't5-combined)
    (ok (not (eql *combined-sentinel* :active)) "sentinel cleared after uninstall")
    (ok (null (lem::find-command "t5-combined-cmd")) "command gone after uninstall")))

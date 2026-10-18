;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/lang-python
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-context/reactivity #:define-coeffects)
  (:import-from :yeditor-components/context-bridge #:buffer-context)
  (:export :le-lang-python
           :*eval-listeners*
           :*test-listeners*
           :enable-python-support
           :*python-coeffects*
           :find-venv
           :*active-venv*
           :*lsp-candidates*
           :*formatter-candidates*
           :*format-on-save*
           :format-python-buffer
           :*pytest-command*
           :run-python-tests
           :*python-test-results*
           :test-overlays
           :python-repl-process
           :python-eval
           :*python-evals*
           :yeditor-python-repl
           :yeditor-python-send-region
           :yeditor-python-send-buffer
           :yeditor-python-test
           :yeditor-python-format))
(in-package :yeditor-components/lang-python)

;;;; le-lang-python (plan T28): activated by coeffect on Python files or Python
;;;; project markers. While active: the project's virtualenv is on PATH, the LSP
;;;; server is the best one installed, files are formatted on save as a reversible
;;;; side-transaction, and a REPL and pytest runner are available. Uninstalling
;;;; undoes all of it.
;;;; Eval and test records are kept in this package until the workspace journal
;;;; (T20, yeditor-enterprise) exists.

(defparameter *python-coeffects*
  (list :any (list (list :file-type "py")
                   (list :project-marker (list "pyproject.toml" "requirements.txt" "uv.lock"))))
  "Plan T28: a .py file, or a project with a Python marker.")

(defun enable-python-support ()
  "Let le-lang-python activate and deactivate on its coeffects."
  (define-coeffects (quote le-lang-python) *python-coeffects*))

(defun python-buffer-p (buffer)
  "A buffer in python-mode, or visiting a .py file."
  (or (eq (buffer-major-mode buffer) (quote lem-python-mode:python-mode))
      (let ((file (buffer-filename buffer)))
        (and file (equal (pathname-type file) "py")))))

;;; Virtualenv

(defun venv-python (venv)
  (merge-pathnames "bin/python" venv))

(defun find-venv (root)
  "The virtualenv of the project at ROOT: .venv (uv), venv or env with a bin/python;
   else the active conda environment (CONDA_PREFIX); else nil."
  (or (loop :for name :in (list ".venv/" "venv/" "env/")
            :for dir := (merge-pathnames name root)
            :when (probe-file (venv-python dir)) :return dir)
      (let ((conda (uiop:getenv "CONDA_PREFIX")))
        (and conda (probe-file (venv-python (uiop:ensure-directory-pathname conda)))
             (uiop:ensure-directory-pathname conda)))))

(defvar *active-venv* nil "Plist (:root :path :handles) of the activated virtualenv, or nil.")

(defun deactivate-venv ()
  (when *active-venv*
    (dolist (handle (reverse (getf *active-venv* :handles)))
      (funcall (cdr (assoc :inverse handle))))
    (setf *active-venv* nil)))

(defun activate-venv-for (root)
  "Make the virtualenv of ROOT the active one (a no-op if it already is)."
  (unless (and *active-venv* (equal (getf *active-venv* :root) root))
    (deactivate-venv)
    (let ((venv (and root (find-venv root))))
      (when venv
        (let ((yeditor-tx:*tx-pending* nil)
              (yeditor-tx:*tx-committed* nil))
          (let ((handles (list (install-environment-variable (quote le-lang-python) "VIRTUAL_ENV"
                                                              (namestring venv))
                               (install-environment-variable
                                (quote le-lang-python) "PATH"
                                (format nil "~Abin:~A" (namestring venv) (or (uiop:getenv "PATH") ""))))))
            (yeditor-tx:tx-commit)
            (setf *active-venv* (list :root root :path venv :handles handles))))))
    (choose-lsp-command)))

(defun follow-project (buffer)
  "On file open or buffer switch, follow the Python project of BUFFER."
  (when (python-buffer-p buffer)
    (activate-venv-for (getf (buffer-context buffer) :project-root))))

(defun python-program ()
  (if *active-venv* (namestring (venv-python (getf *active-venv* :path))) "python3"))

;;; Tools lookup

(defun executable-on-path-p (program)
  (loop :for dir :in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
        :thereis (and (plusp (length dir))
                      (probe-file (merge-pathnames program (uiop:ensure-directory-pathname dir))))))

(defun first-available (candidates)
  (find-if (lambda (command) (executable-on-path-p (first command))) candidates))

;;; LSP server choice

(defparameter *lsp-candidates*
  (list (list "basedpyright-langserver" "--stdio") (list "pyright-langserver" "--stdio") (list "pylsp"))
  "Python language servers in order of preference.")

(defvar *lsp-choice-active* nil
  "True while le-lang-python manages the Python LSP command (between install and undo).")

(defun python-spec ()
  (lem-lsp-mode/spec:get-language-spec (quote lem-python-mode:python-mode)))

(defun choose-lsp-command ()
  "Point the Python language spec at the best server found on the current PATH."
  (let ((spec (python-spec))
        (choice (first-available *lsp-candidates*)))
    (when (and spec choice *lsp-choice-active*)
      (setf (lem-lsp-mode/spec:spec-command spec) choice))))

(defun install-lsp-choice (comp)
  "Manage the Python LSP command: chosen now and again whenever the virtualenv (and
   so PATH) changes; undo restores the command found at commit."
  (let ((previous nil))
    (yeditor-tx:install-effect comp :lsp-command
                               (lambda ()
                                 (alexandria:when-let ((spec (python-spec)))
                                   (setf previous (lem-lsp-mode/spec:spec-command spec)))
                                 (setf *lsp-choice-active* t)
                                 (choose-lsp-command))
                               :undo-thunk
                               (lambda ()
                                 (setf *lsp-choice-active* nil)
                                 (alexandria:when-let ((spec (python-spec)))
                                   (when previous
                                     (setf (lem-lsp-mode/spec:spec-command spec) previous)))))))

;;; Formatting

(defparameter *formatter-candidates*
  (list (list "ruff" "format" "-") (list "black" "-q" "-"))
  "Formatters reading the source on stdin and writing it on stdout, in order of preference.")

(defvar *format-on-save* t)

(defun format-python-buffer (buffer)
  "Format BUFFER with the first available formatter. The change is registered as a
   side-transaction (kind 2) named format:<buffer> whose undo restores the previous
   text. Returns :formatted, :unchanged, :no-formatter or :failed (buffer untouched)."
  (let ((command (first-available *formatter-candidates*)))
    (if (null command)
        :no-formatter
        (let ((text (points-to-string (buffer-start-point buffer) (buffer-end-point buffer))))
          (multiple-value-bind (output error code)
              (with-input-from-string (in text)
                (uiop:run-program command :input in :output :string :error-output :string
                                          :ignore-error-status t))
            (cond ((not (zerop code))
                   (message "Formatter failed: ~A" (string-trim '(#\Newline) error))
                   :failed)
                  ((string= output text) :unchanged)
                  (t
                   (flet ((replace-text (new)
                            (let ((line (line-number-at-point (buffer-point buffer))))
                              (erase-buffer buffer)
                              (insert-string (buffer-start-point buffer) new)
                              (move-to-line (buffer-point buffer) line))))
                     (yeditor-tx-side:side-transaction
                      (format nil "format:~A" (buffer-name buffer))
                      (lambda () (replace-text output))
                      (lambda () (replace-text text))))
                   :formatted)))))))

(defun format-on-save (buffer)
  (when (and *format-on-save* (python-buffer-p buffer))
    (format-python-buffer buffer)))

(defun install-save-hook (comp)
  (yeditor-tx:install-effect comp :hook
                             (lambda () (add-hook (variable-value (quote before-save-hook) :global)
                                                  (quote format-on-save)))
                             :undo-thunk
                             (lambda () (remove-hook (variable-value (quote before-save-hook) :global)
                                                     (quote format-on-save)))))

;;; REPL

(defvar *repl* nil "The REPL process, or nil.")
(defvar *python-evals* nil "Eval records, newest first: plists :code :output :time.")

(defun python-repl-process () *repl*)

(defun repl-buffer ()
  (or (get-buffer "*yeditor-python*")
      (make-buffer "*yeditor-python*")))

(defun repl-output (string)
  (insert-string (buffer-end-point (repl-buffer)) string)
  (when *python-evals*
    (setf (getf (first *python-evals*) :output)
          (concatenate (quote string) (getf (first *python-evals*) :output) string))))

(defun ensure-repl ()
  (unless (and *repl* (lem-process:process-alive-p *repl*))
    (setf *repl* (lem-process:run-process (list (python-program) "-i" "-u")
                                          :name "yeditor-python"
                                          :output-callback (quote repl-output))))
  *repl*)

(defun stop-repl ()
  (when *repl*
    (lem-process:delete-process *repl*)
    (setf *repl* nil))
  (alexandria:when-let ((buffer (get-buffer "*yeditor-python*")))
    (kill-buffer buffer)))

(defvar *eval-listeners* nil
  "Functions called with each eval record (plist :code :language :time), e.g. the journal.")
(defvar *test-listeners* nil
  "Functions called with each test run record (plist :target :exit :failures :output).")

(defun notify-listeners (listeners record)
  (dolist (listener listeners)
    (ignore-errors (funcall listener record))))

(defun python-eval (code)
  "Send CODE to the REPL (via a temporary file, so multi-line code works) and record it."
  (let ((file (uiop:tmpize-pathname (merge-pathnames "yeditor-eval.py" (uiop:default-temporary-directory)))))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-string code out))
    (push (list :code code :output "" :time (get-universal-time)) *python-evals*)
    (notify-listeners *eval-listeners* (list :code code :language "python" :time (get-universal-time)))
    (lem-process:process-send-input (ensure-repl)
                                    (format nil "exec(open(~S).read())~%" (namestring file)))))

;;; pytest

(define-attribute python-test-failure-attribute
  (t :underline t :foreground "#ea6962"))

(defvar *test-overlays* nil)
(defvar *python-test-results* nil "Test run records, newest first: plists :target :exit :failures :output.")

(defun test-overlays () *test-overlays*)

(defparameter *pytest-command*
  (lambda (target) (list (python-program) "-m" "pytest" "-q" target))
  "Function of the target (file or file::test) returning the command to run.")

(defun clear-test-overlays ()
  (mapc (function delete-overlay) *test-overlays*)
  (setf *test-overlays* nil))

(defun parse-failures (output)
  "(file . line) for each 'path.py:LINE: ' line in pytest OUTPUT."
  (let ((failures nil))
    (ppcre:do-register-groups (file line) ("(?m)^([^\\s:]+\\.py):(\\d+): " output)
      (pushnew (cons file (parse-integer line)) failures :test (function equal)))
    (nreverse failures)))

(defun mark-failure (root file line)
  (let ((buffer (find (probe-file (merge-pathnames file root)) (buffer-list)
                      :key (lambda (b) (and (buffer-filename b) (probe-file (buffer-filename b))))
                      :test (function equal))))
    (when buffer
      (with-point ((s (buffer-start-point buffer)) (e (buffer-start-point buffer)))
        (move-to-line s line)
        (move-to-line e line)
        (line-end e)
        (push (make-overlay s e (quote python-test-failure-attribute)) *test-overlays*)))))

(defun run-python-tests (target root)
  "Run pytest on TARGET in ROOT, mark failing lines in open buffers, record the run.
   Returns the run record."
  (clear-test-overlays)
  (multiple-value-bind (output error code)
      (uiop:run-program (funcall *pytest-command* target) :directory root :output :string
                                                            :error-output :string :ignore-error-status t)
    (let* ((all (concatenate (quote string) output error))
           (failures (parse-failures all))
           (record (list :target target :exit code :failures failures :output all)))
      (loop :for (file . line) :in failures :do (mark-failure root file line))
      (push record *python-test-results*)
      (notify-listeners *test-listeners* record)
      record)))

(defun test-at-point (point)
  "file::test_name of the test function enclosing POINT, or nil."
  (with-point ((p point))
    (loop
      (let ((name (nth-value 1 (ppcre:scan-to-strings "^\\s*(?:async\\s+)?def\\s+(test_\\w+)" (line-string p)))))
        (when name (return (aref name 0))))
      (unless (line-offset p -1) (return nil)))))

;;; Commands

(define-command yeditor-python-repl () ()
  "Start (or show) the Python REPL of the active virtualenv."
  (ensure-repl)
  (pop-to-buffer (repl-buffer)))

(define-command yeditor-python-send-region (start end) (:region)
  "Evaluate the region in the Python REPL."
  (python-eval (points-to-string start end)))

(define-command yeditor-python-send-buffer () ()
  "Evaluate the whole buffer in the Python REPL."
  (python-eval (points-to-string (buffer-start-point (current-buffer)) (buffer-end-point (current-buffer)))))

(define-command yeditor-python-test (&optional whole-file) (:universal-nil)
  "Run the test under the cursor (or the whole file with a prefix argument) with pytest."
  (let* ((buffer (current-buffer))
         (file (or (buffer-filename buffer) (editor-error "Buffer has no file")))
         (root (getf (buffer-context buffer) :project-root))
         (test (and (not whole-file) (test-at-point (current-point))))
         (relative (enough-namestring file root))
         (record (run-python-tests (if test (format nil "~A::~A" relative test) relative) root)))
    (message "pytest ~A: exit ~D, ~D failing line~:P marked"
             (getf record :target) (getf record :exit) (length (getf record :failures))
             (length (getf record :failures)))))

(define-command yeditor-python-format () ()
  "Format the current buffer (reversible as a side-transaction)."
  (message "Format: ~(~A~)" (format-python-buffer (current-buffer))))

;;; Component

(define-component le-lang-python
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (let ((keymap (mode-keymap (quote lem-python-mode:python-mode))))
               (list
                (yeditor-tx:install-effect comp :environment
                                           (lambda () (follow-project (current-buffer)))
                                           :undo-thunk (function deactivate-venv))
                (install-lsp-choice comp)
                (install-save-hook comp)
                (install-hook comp (quote *find-file-hook*) (quote follow-project))
                (install-hook comp (quote *switch-to-buffer-hook*) (quote follow-project))
                (install-keybinding comp keymap "C-c C-z" (quote yeditor-python-repl))
                (install-keybinding comp keymap "C-c C-r" (quote yeditor-python-send-region))
                (install-keybinding comp keymap "C-c C-b" (quote yeditor-python-send-buffer))
                (install-keybinding comp keymap "C-c C-t" (quote yeditor-python-test))
                (install-keybinding comp keymap "C-c C-f" (quote yeditor-python-format))
                (yeditor-tx:install-effect comp :process (lambda () nil)
                                           :undo-thunk (lambda () (stop-repl) (clear-test-overlays)))))))

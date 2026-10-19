;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise/forge
  (:use :cl :lem :yeditor-context)
  (:import-from :yeditor-components/models #:provider-generate)
  (:export :forge-component
           :check-component-source
           :forge-rejected
           :forge-rejected-reason
           :*forge-directory*
           :*max-component-bytes*
           :*reversibility-rounds*
           :*allowed-operators*
           :state-fingerprint
           :yeditor-forge
           :le-forge))
(in-package :yeditor-enterprise/forge)

;;;; le-forge (plan T18): natural language -> a tested, reversible editor component.
;;;; 1. ask the model for one define-component form, given the SDK and allowed effects;
;;;; 2. reject it statically unless it is small, a single define-component, calls only
;;;;    whitelisted operators (effects that carry their inverse, plus a harmless subset
;;;;    of Common Lisp), and gives any raw install-effect an :undo-thunk;
;;;; 3. register it and install/uninstall it several times, checking that the editor's
;;;;    observable state comes back identical every time;
;;;; 4. promote it (saved under ~/.yeditor/components/) or discard it with no residue.

(defparameter *forge-directory* (merge-pathnames ".yeditor/components/" (user-homedir-pathname)))
(defparameter *max-component-bytes* 16384)
(defparameter *reversibility-rounds* 5)

(define-condition forge-rejected (error)
  ((reason :initarg :reason :reader forge-rejected-reason))
  (:report (lambda (c s) (format s "Component rejected: ~A" (forge-rejected-reason c)))))

(defun reject (format-control &rest args)
  (error (quote forge-rejected) :reason (apply (function format) nil format-control args)))

;;; The sandbox package generated code is read into

(defpackage :yeditor-forge-sandbox
  (:use :cl :lem :yeditor-context :yeditor-effects))

(defparameter *allowed-operators*
  (append
   (list (quote lambda) (quote declare) (quote ignore) (quote ignorable) (quote function) (quote quote)
         (quote list) (quote list*) (quote cons) (quote append) (quote let) (quote let*) (quote progn)
         (quote if) (quote when) (quote unless) (quote cond) (quote and) (quote or) (quote not)
         (quote equal) (quote eq) (quote eql) (quote string=) (quote format) (quote concatenate)
         (quote first) (quote rest) (quote length)
         (quote define-component) (quote yeditor-tx:install-effect))
   ;; effects that change editor state only and carry their inverse; deliberately not
   ;; process, server, fdefinition, environment, LSP or grammar effects
   (list (quote yeditor-effects:install-keybinding) (quote yeditor-effects:install-key-removal)
         (quote yeditor-effects:install-variable) (quote yeditor-effects:install-editor-variable)
         (quote yeditor-effects:install-hook) (quote yeditor-effects:install-hook-removal)
         (quote yeditor-effects:install-theme) (quote yeditor-effects:install-timer)
         (quote yeditor-effects:install-buffer)))
  "Operators a generated component may call.")

(defun literal-value-p (form)
  "A self-evaluating datum that cannot smuggle code: string, number, character,
   keyword, T or NIL."
  (or (stringp form) (numberp form) (characterp form) (keywordp form) (member form (list t nil))))

(defun check-function-argument (form operator)
  "Hook and timer functions must be a lambda (checked like any code) or #' of an allowed
   operator; a quoted symbol would call an arbitrary function."
  (cond ((and (consp form) (eq (first form) (quote lambda))) (check-calls form))
        ((and (consp form) (eq (first form) (quote function))) (check-calls form))
        (t (reject "~S needs a lambda or #' of an allowed function, not ~S" operator form))))

(defun check-effect-arguments (form)
  (let ((operator (first form)))
    (cond ((eq operator (quote yeditor-effects:install-hook))
           (check-function-argument (fourth form) operator))
          ((eq operator (quote yeditor-effects:install-timer))
           (check-function-argument (fourth form) operator))
          ((member operator (list (quote yeditor-effects:install-variable)
                                  (quote yeditor-effects:install-editor-variable)))
           (unless (literal-value-p (fourth form))
             (reject "~S values must be literals (string, number, keyword, t, nil), not ~S"
                     operator (fourth form)))))))

(defun operator-allowed-p (symbol)
  (member symbol *allowed-operators*))

(defun check-calls (form)
  "Walk FORM; reject any call whose operator is not allowed, and any raw install-effect
   without an :undo-thunk."
  (when (consp form)
    (let ((head (first form)))
      (cond ((eq head (quote quote)) nil)
            ((eq head (quote function))
             (let ((target (second form)))
               (if (consp target)
                   (check-calls target)
                   (unless (operator-allowed-p target)
                     (reject "calls ~S, which is not allowed" target)))))
            ((eq head (quote lambda))
             (mapc (function check-calls) (cddr form)))
            ((member head (list (quote let) (quote let*)))
             (dolist (binding (second form))
               (when (consp binding) (check-calls (second binding))))
             (mapc (function check-calls) (cddr form)))
            ((eq head (quote declare)) nil)
            ((symbolp head)
             (unless (operator-allowed-p head)
               (reject "calls ~S, which is not allowed" head))
             (when (and (eq head (quote yeditor-tx:install-effect))
                        (not (getf (cddddr form) :undo-thunk)))
               (reject "an effect without an inverse (install-effect with no :undo-thunk)"))
             (check-effect-arguments form)
             (mapc (function check-calls) (rest form)))
            (t (reject "unexpected form ~S" head))))))

(defun read-component-source (source)
  "Read SOURCE into the sandbox package with *read-eval* off; returns the single form."
  (when (> (length source) *max-component-bytes*)
    (reject "~D characters exceeds the ~D limit" (length source) *max-component-bytes*))
  (let ((forms (handler-case
                   (let ((*read-eval* nil)
                         (*package* (find-package :yeditor-forge-sandbox)))
                     (with-input-from-string (in source)
                       (loop :for form := (read in nil in) :until (eq form in) :collect form)))
                 (error (e) (reject "unreadable source: ~A" e)))))
    (unless (= (length forms) 1)
      (reject "expected exactly one form, got ~D" (length forms)))
    (let ((form (first forms)))
      (unless (and (consp form) (eq (first form) (quote define-component)) (symbolp (second form)))
        (reject "the form must be (define-component NAME ...)"))
      form)))

(defun check-component-source (source)
  "Static checks; returns the define-component form or signals forge-rejected."
  (let ((form (read-component-source source)))
    (loop :for (key value) :on (cddr form) :by (function cddr)
          :do (unless (member key (list :version :author :install :config-spec))
                (reject "unsupported option ~S" key))
              (check-calls value))
    (unless (getf (cddr form) :install)
      (reject "no :install function"))
    form))

;;; Reversibility test

(defun state-fingerprint ()
  "Observable editor state that installing then uninstalling a component must restore."
  (let ((bindings nil))
    (traverse-keymap *global-keymap*
                     (lambda (keys command) (push (cons (format nil "~{~A~^ ~}" keys) command) bindings)))
    (list (sort (mapcar (function buffer-name) (buffer-list)) (function string<))
          (sort bindings (function string<) :key (function car))
          (copy-tree *find-file-hook*)
          (copy-tree *switch-to-buffer-hook*)
          (copy-tree *after-init-hook*)
          (current-theme)
          (variable-value (quote modeline-format) :global))))

(defun reversible-p (name)
  "Install and uninstall NAME *reversibility-rounds* times; true if the state comes back
   identical each time. Signals forge-rejected if installing fails."
  (let ((before (state-fingerprint)))
    (loop :repeat *reversibility-rounds*
          :do (handler-case (install-component name)
                (error (e) (reject "installing failed: ~A" e)))
              (uninstall-component name)
          :always (equal (state-fingerprint) before))))

;;; The forge loop

(defparameter *forge-prompt*
  "Write one Yeditor component in Common Lisp as a single form:
  (define-component NAME :version \"0.1.0\" :install (lambda (comp ctx) (declare (ignore ctx)) (list EFFECT...)))
Each EFFECT must be one of these calls (each registers its own inverse):
  (install-keybinding comp *global-keymap* \"C-c x\" 'some-existing-command)
  (install-variable comp 'symbol value)
  (install-editor-variable comp 'editor-variable value)
  (install-hook comp '*hook-variable* (lambda (&rest args) (declare (ignore args)) ...))
  (install-theme comp \"theme-name\")
Variable values must be literals (string, number, keyword, t or nil).
Use only those calls plus list, quote and lambda. Reply with the form only, no prose and no code fences.

Request: ~A")

(defun strip-fences (text)
  (let ((trimmed (string-trim '(#\Newline #\Space) text)))
    (if (uiop:string-prefix-p "```" trimmed)
        (let ((start (or (position #\Newline trimmed) 3))
              (end (or (search "```" trimmed :from-end t) (length trimmed))))
          (subseq trimmed start (max start end)))
        trimmed)))

(defun forge-component (request provider &key (promote t))
  "Generate, check, test and promote a component for REQUEST using PROVIDER.
   Returns a plist :status (:promoted or :discarded) :name :path :reason :source."
  (let* ((source (strip-fences (provider-generate provider (format nil *forge-prompt* request))))
         (name nil))
    (handler-case
        (let ((form (check-component-source source)))
          (setf name (second form))
          (when (gethash name *component-registry*)
            (reject "a component named ~A already exists" name))
          (eval form)
          (unless (reversible-p name)
            (reject "the editor state was not restored after uninstalling"))
          (let ((path (merge-pathnames (format nil "~(~A~).lisp" (symbol-name name)) *forge-directory*)))
            (when promote
              (ensure-directories-exist path)
              (with-open-file (out path :direction :output :if-exists :supersede)
                (write-string source out)
                (terpri out)))
            (list :status :promoted :name name :path (and promote path) :source source)))
      (forge-rejected (e)
        (when (and name (gethash name *component-registry*))
          (ignore-errors (uninstall-component name))
          (unregister-component name))
        (list :status :discarded :name name :reason (forge-rejected-reason e) :source source)))))

(define-command yeditor-forge (request) ((:string "Forge a component: "))
  "Generate a reversible editor component from a request, test it and promote it."
  (let ((provider (or yeditor-components/ai:*default-provider*
                      (editor-error "No AI provider configured"))))
    (let ((result (forge-component request provider)))
      (if (eq (getf result :status) :promoted)
          (message "Forged ~(~A~) (saved to ~A); M-x toggle it via the component list"
                   (getf result :name) (getf result :path))
          (message "Forge discarded the component: ~A" (getf result :reason))))))

(define-component le-forge
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (yeditor-effects:install-keybinding comp *global-keymap* "C-c a f" (quote yeditor-forge)))))

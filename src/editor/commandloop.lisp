(in-package :lem)

(export '(pre-command-hook
          post-command-hook
          exit-editor-hook
          interactive-p
          add-continue-flag
          continue-flag))

(defvar *interactive-p* nil)
(defun interactive-p () *interactive-p*)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *continue-command-flags*
  (list :next-line :kill :undo :yank))

(defun add-continue-flag (keyword)
  (pushnew keyword *continue-command-flags*))

(defun %make-flags ()
  (mapcar #'(lambda (sym)
              (cons sym nil))
          *continue-command-flags*))

(defun continue-flag (flag)
  (prog1 (cdr (assoc flag *last-flags*))
    (push (cons flag t) *last-flags*)
    (push (cons flag t) *curr-flags*)))

(defun cmd-call (cmd arg)
  (run-hooks 'pre-command-hook)
  (prog1 (funcall cmd arg)
    (buffer-undo-boundary)
    (run-hooks 'post-command-hook)))

(defun do-commandloop-function (function)
  (do ((*curr-flags* (%make-flags) (%make-flags))
       (*last-flags* (%make-flags) *curr-flags*))
      (nil)
    (let ((*interactive-p* t))
      (funcall function))))

(defvar +exit-tag+ (gensym "EXIT"))

(defmacro do-commandloop ((&key toplevel) &body body)
  (if toplevel
      `(catch +exit-tag+
         (do-commandloop-function (lambda () ,@body)))
      `(do-commandloop-function (lambda () ,@body))))

(defun exit-editor (&optional report)
  (run-hooks 'exit-editor-hook)
  (throw +exit-tag+ report))

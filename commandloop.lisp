(in-package :lem)

(export '(add-continue-flag
          continue-flag))

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *continue-command-flags*
  (list :next-line :kill :undo :yank :completion))

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

(defmacro do-commandloop (() &body body)
  `(do ((*curr-flags* (%make-flags) (%make-flags))
        (*last-flags* (%make-flags) *curr-flags*))
       (nil)
     ,@body))

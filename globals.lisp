(in-package :lem)

(defvar *program-name* "Lem")

(defvar *window-list*)
(defvar *current-window*)
(defvar *prev-buffer*)
(defvar *buffer-list* nil)

(defvar *tab-size* 8)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *universal-argument* nil)

(defstruct flags
  kill
  undo
  abbrev)

(defmacro when-interrupted-flag (flag-name &body body)
  (let ((name (intern (string-upcase (format nil "flags-~a" flag-name)))))
    `(progn
       (unless (,name *last-flags*)
         ,@body)
       (setf (,name *last-flags*) t)
       (setf (,name *curr-flags*) t))))

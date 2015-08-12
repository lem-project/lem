(in-package :lem)

(export '(*window-list*
          *current-window*
          *buffer-list*
          *tab-size*
          save-excursion))

(defvar *program-name* "Lem")

(defvar *window-list*)
(defvar *current-window*)
(defvar *buffer-list* nil)

(defvar *tab-size* 8)

(defvar *refresh-threshold-time* 10)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *universal-argument* nil)

(defvar *getch-wait-flag* nil)

(defvar *continue-command-flags* (list :kill :undo :abbrev))

(defun make-flags ()
  (mapcar (lambda (sym)
            (cons sym nil))
          *continue-command-flags*))

(defmacro define-class (name () default-arg-expr &body slots)
  (let ((garg (gensym "ARG"))
        (gval (gensym "VAL")))
    `(progn
       (defclass ,name ()
         ,(mapcar (lambda (slot)
                    `(,slot :initarg ,(intern (symbol-name slot) :keyword)
                            :initform nil))
                  slots))
       ,@(mapcan (lambda (slot)
                   (let ((name (intern (format nil "~a-~a" name slot))))
                     `((defun ,name (&optional (,garg ,default-arg-expr))
                         (slot-value ,garg ',slot))
                       (defun (setf ,name)
                         (,gval &optional (,garg ,default-arg-expr))
                         (setf (slot-value ,garg ',slot) ,gval)
                         ,gval))))
                 slots))))

(defmacro when-interrupted-flag (flag &body body)
  (let ((gflag (gensym "FLAG")))
    `(let ((,gflag ,flag))
       (unless (cdr (assoc ,flag *last-flags*)) ,@body)
       (push (cons ,gflag t) *last-flags*)
       (push (cons ,gflag t) *curr-flags*))))

(defmacro when-continue-flag (flag &body body)
  (let ((gflag (gensym "FLAG")))
    `(let ((,gflag ,flag))
       (when (cdr (assoc ,flag *last-flags*)) ,@body)
       (push (cons ,gflag t) *last-flags*)
       (push (cons ,gflag t) *curr-flags*))))

(defmacro save-excursion (&body body)
  (let ((gpoint (gensym "POINT")))
    `(let ((,gpoint (point)))
       (unwind-protect (progn ,@body)
         (point-set ,gpoint)))))

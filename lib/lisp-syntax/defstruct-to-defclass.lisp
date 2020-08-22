(defpackage :lem-lisp-syntax.defstruct-to-defclass
  (:use :cl :lem-base))
(in-package :lem-lisp-syntax.defstruct-to-defclass)

(defvar *save-points* '())

(defun save-point (point)
  (let ((new-point (copy-point point :left-inserting)))
    (push new-point *save-points*)
    new-point))

(defun cleanup-save-points ()
  (dolist (point *save-points*)
    (delete-point point))
  (setf *save-points* '()))

(defun call-with-temporary-points (function)
  (let ((*save-points* '()))
    (unwind-protect (funcall function)
      (cleanup-save-points))))

(defmacro with-temporary-points (() &body body)
  `(call-with-temporary-points (lambda () ,@body)))

(define-condition scan-failed (editor-error)
  ())

(defun exact (result)
  (unless result
    (error 'scan-failed))
  result)

(defun safe-read-from-string (string)
  (handler-case
      (let ((*read-eval* nil))
        (read-from-string string))
    (reader-error ()
      (error 'scan-failed))))

(defvar *struct-info*)

(defun forward-token (point &key (case-sensitive-p t))
  (skip-space-and-comment-forward point)
  (cond ((eql (character-at point) #\()
         :list-start)
        ((eql (character-at point) #\))
         :list-end)
        (t
         (let ((string (symbol-string-at-point point)))
           (cond ((null string)
                  nil)
                 (case-sensitive-p
                  string)
                 (t
                  (string-downcase string)))))))

(defun forward-form (point)
  (exact (form-offset point 1))
  (with-point ((start point))
    (exact (form-offset start -1))
    (let ((*read-eval* nil))
      (safe-read-from-string (points-to-string start point)))))

(defun enter-list (point)
  (scan-lists point 1 -1))

(defun exit-list (point)
  (scan-lists point 1 1))

(defstruct (slot-description-info (:conc-name slot-description-))
  name
  point
  initial-value-start-point
  initial-value-end-point
  read-only-p
  type-start-point
  type-end-point)

(defstruct (struct-info (:constructor make-struct-info ())
                        (:conc-name struct-))
  start-point
  end-point
  name
  name-and-options-point
  slot-descriptions)

(defun scan-defstruct-name-and-options (point)
  (let ((token (forward-token point)))
    (when (or (stringp token) (eq token :list-start)) ; (defstruct |structure-name ..., (defstruct |(structure-name ...
      (cond ((eq token :list-start)
             (editor-error "unimplemented (name-and-options...) parser"))
            (t
             (setf (struct-name *struct-info*)
                   token)
             (setf (struct-name-and-options-point *struct-info*)
                   (save-point point))
             (form-offset point 1))))))

(defun scan-slot-description-option (point slot-info)
  (trivia:match (forward-token point :case-sensitive-p nil)
    (":type"
     (form-offset point 1)
     ;(slot-name ... :type| type)
     (skip-space-and-comment-forward point)
     ;(slot-name ... :type |type)
     (setf (slot-description-type-start-point slot-info) (save-point point))
     (exact (form-offset point 1))
     ;(slot-name ... :type type|)
     (setf (slot-description-type-end-point slot-info) (save-point point))
     t)
    (":read-only"
     (form-offset point 1)
     ;(slot-name ... :read-only| boolean)
     (when (forward-form point)
       (setf (slot-description-read-only-p slot-info) t))
     ;(slot-name ... :read-only boolean|)
     t)
    ((eq :list-end)
     nil)
    (otherwise
     nil)))

(defun scan-complex-slot-description (point)
  (flet ((scan-slot-name ()
           (let ((slot-name (forward-token point)))
             (exact (stringp slot-name))
             (form-offset point 1)
             slot-name)))
    (enter-list point)
    (let ((slot-info
            (make-slot-description-info :name (scan-slot-name)
                                        :point (save-point point))))
      (skip-space-and-comment-forward point)
      ;(defstruct structure-name (slot-name |init-form :type integer))
      (setf (slot-description-initial-value-start-point slot-info) (save-point point))
      (exact (form-offset point 1))
      ;(defstruct structure-name (slot-name init-form| :type integer))
      (setf (slot-description-initial-value-end-point slot-info) (save-point point))
      (loop :repeat 2 :while (scan-slot-description-option point slot-info))
      (prog1 slot-info
        (exit-list point)))))

(defun scan-forward-slot-description (point)
  (trivia:ematch (forward-token point)
    ((trivia:guard token (stringp token))
     (prog1 (make-slot-description-info :name token :point (save-point point))
       (form-offset point 1)))
    ((eq :list-start)
     (scan-complex-slot-description point))
    ((eq :list-end)
     nil)
    ((eq nil)
     nil)))

(defun scan-defstruct (point)
  (when (eql (character-at point) #\() ; |(defstruct
    (enter-list point)
    (let ((token (forward-token point)))
      (when (and (stringp token) (string-equal token "defstruct")) ; (|defstruct
        (setf (struct-start-point *struct-info*)
              (save-point point))
        ; (|defstruct ...
        (exact (form-offset point 1))
        ; (defstruct| ...
        (exact (scan-defstruct-name-and-options point))
        ; (defstruct structure-name| slot-name ...)
        (loop :for slot-description := (scan-forward-slot-description point)
              :while slot-description
              :do (alexandria:nconcf (struct-slot-descriptions *struct-info*)
                                     (list slot-description)))
        (skip-space-and-comment-forward point)
        (exact (char= (character-at point) #\)))
        (setf (struct-end-point *struct-info*)
              (save-point point))))))

(defun analyze-defstruct (point *struct-info*)
  (with-point ((point point))
    (scan-defstruct point))
  *struct-info*)

(defun replace-at-point (point old new)
  (assert (string= old (symbol-string-at-point point)))
  (delete-character point (length old))
  (insert-string point new))

(defun translate-to-defclass-with-info (point info)
  (flet ((translate-slot (slot firstp)
           (move-point point (slot-description-point slot))
           (unless firstp
             (insert-character point #\space))
           (insert-character point #\()
           (line-end point)
           (when (point<= (struct-end-point info) point)
             (move-point point (struct-end-point info)))
           (insert-character point #\newline)
           (insert-string point
                          (format nil ":initarg :~A"
                                  (slot-description-name slot)))
           (insert-character point #\newline)
           (insert-string point
                          ":initform nil")
           (insert-character point #\newline)
           (insert-string point
                          (format nil ":accessor ~A-~A" (struct-name info) (slot-description-name slot)))
           (insert-character point #\))))
    (move-point point (struct-start-point info))
    (replace-at-point point "defstruct" "defclass")
    (form-offset point 1)
    (insert-character point #\space)
    (insert-string point "()")
    (line-offset point 1)
    (back-to-indentation point)
    (insert-character point #\()
    (loop :for slot :in (struct-slot-descriptions info)
          :for firstp := t :then nil
          :do (translate-slot slot firstp))
    (insert-character point #\))
    (indent-region (struct-start-point info)
                   (struct-end-point info))))

(defun defstruct-to-defclass (point)
  (handler-case
      (with-temporary-points ()
        (let ((info (analyze-defstruct point (make-struct-info))))
          (translate-to-defclass-with-info point info)))
    (editor-error (c)
      (error c))))

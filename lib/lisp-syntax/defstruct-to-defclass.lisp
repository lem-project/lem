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

(defvar *struct-form-info*)

(defun forward-token (point)
  (skip-space-and-comment-forward point)
  (cond ((eql (character-at point) #\()
         :list-start)
        ((eql (character-at point) #\))
         :list-end)
        (t
         (symbol-string-at-point point))))

(define-condition scan-failed (editor-error)
  ())

(defun exact (result)
  (unless result
    (error 'scan-failed))
  result)

(defun enter-list (point)
  (scan-lists point 1 -1))

(defun exit-list (point)
  (scan-lists point 1 1))

(defstruct slot-description-info
  name
  point)

(defstruct (struct-form-info (:conc-name struct-)
                             (:constructor %make-struct-form-info))
  start-point
  end-point
  name
  name-and-options-point
  slot-descriptions)

(defun make-struct-form-info ()
  (%make-struct-form-info))

(defun scan-defstruct-name-and-options (point)
  (let ((token (forward-token point)))
    (when (or (stringp token) (eq token :list-start)) ; (defstruct |structure-name ..., (defstruct |(structure-name ...
      (cond ((eq token :list-start)
             (editor-error "unimplemented (name-and-options...) parser"))
            (t
             (setf (struct-name *struct-form-info*)
                   token)
             (setf (struct-name-and-options-point *struct-form-info*)
                   (save-point point))
             (form-offset point 1))))))

(defun scan-forward-slot-description (point)
  (trivia:ematch (forward-token point)
    ((trivia:guard token (stringp token))
     (prog1 (make-slot-description-info :name token :point (save-point point))
       (form-offset point 1)))
    ((eq :list-start)
     (enter-list point)
     (let ((token (forward-token point)))
       (exact (stringp token))
       (prog1 (make-slot-description-info :name token :point (save-point point))
         (exit-list point))))
    ((eq :list-end)
     nil)
    ((eq nil)
     nil)))

(defun scan-defstruct (point)
  (when (eql (character-at point) #\() ; |(defstruct
    (enter-list point)
    (let ((token (forward-token point)))
      (when (and (stringp token) (string-equal token "defstruct")) ; (|defstruct
        (setf (struct-start-point *struct-form-info*)
              (save-point point))
        ; (|defstruct ...
        (exact (form-offset point 1))
        ; (defstruct| ...
        (exact (scan-defstruct-name-and-options point))
        ; (defstruct structure-name| slot-name ...)
        (loop :for slot-description := (scan-forward-slot-description point)
              :while slot-description
              :do (alexandria:nconcf (struct-slot-descriptions *struct-form-info*)
                                     (list slot-description)))
        (skip-space-and-comment-forward point)
        (exact (char= (character-at point) #\)))
        (setf (struct-end-point *struct-form-info*)
              (save-point point))))))

(defun analyze-defstruct (point *struct-form-info*)
  (with-point ((point point))
    (scan-defstruct point))
  *struct-form-info*)

(defun replace-at-point (point old new)
  (assert (string= old (symbol-string-at-point point)))
  (delete-character point (length old))
  (insert-string point new))

(defun translate-to-defclass-with-info (point info)
  (labels ((newline-and-indent (n)
             (insert-character point #\newline)
             (loop :repeat n :do (insert-character point #\space)))
           (translate-slot (slot firstp)
             (move-point point (slot-description-info-point slot))
             (if firstp
                 (insert-character point #\()
                 (insert-character point #\space))
             (insert-character point #\()
             (let ((indent (point-charpos point)))
               (line-end point)
               (when (point<= (struct-end-point info) point)
                 (move-point point (struct-end-point info)))
               (newline-and-indent indent)
               (insert-string point
                              (format nil ":initarg :~A"
                                      (slot-description-info-name slot)))
               (newline-and-indent indent)
               (insert-string point
                              ":initform nil")
               (newline-and-indent indent)
               (insert-string point
                              (format nil ":accessor ~A-~A" (struct-name info) (slot-description-info-name slot)))
               (insert-character point #\)))))
    (move-point point (struct-start-point info))
    (replace-at-point point "defstruct" "defclass")
    (form-offset point 1)
    (insert-character point #\space)
    (insert-string point "()")
    (loop :for slot :in (struct-slot-descriptions info)
          :for firstp := t :then nil
          :do (translate-slot slot firstp))
    (insert-character point #\))))

(defun defstruct-to-defclass (point)
  (handler-case
      (with-temporary-points ()
        (let ((info (analyze-defstruct (make-struct-form-info) point)))
          (translate-to-defclass-with-info point info)))
    (editor-error (c)
      (error c))))

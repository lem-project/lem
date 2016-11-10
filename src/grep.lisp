(in-package :cl-user)
(defpackage :lem.grep
  (:use :cl :lem)
  (:export
   :make-grep
   :grep-append
   :grep-update
   :grep-with-string
   :grep-with-string
   :grep
   :grep-next
   :grep-prev))
(in-package :lem.grep)

(defvar *current-grep* nil)

(defclass grep ()
  ((buffer-name
    :initarg :buffer-name
    :reader grep-buffer-name)
   (contents
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :reader grep-contents)
   (index
    :initform -1
    :accessor grep-index)))

(defun grep-content-item (content) (car content))
(defun grep-content-jump-function (content) (cdr content))

(defun make-grep (buffer-name)
  (make-instance 'grep :buffer-name buffer-name))

(defun grep-append (grep item jump-function)
  (vector-push-extend (cons item jump-function)
                      (grep-contents grep))
  (values))

(defun grep-update (grep)
  (setf (grep-index grep) -1)
  (info-popup (get-buffer-create (grep-buffer-name grep))
              (lambda (out)
                (loop :for content :across (grep-contents grep) :do
                  (princ (grep-content-item content) out)
                  (terpri out)))
              nil)
  (setf *current-grep* grep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grep-parse-line (line)
  (ignore-errors
   (let* ((i (position #\: line))
          (j (position #\: line :start (1+ i)))
          (filename (subseq line 0 i))
          (linum (parse-integer (subseq line (1+ i) j))))
     (when (and (stringp filename) (integerp linum))
       (list line filename linum)))))

(defun grep-parse-lines (lines)
  (remove nil
          (mapcar #'grep-parse-line
                  (remove "" lines :test #'string=))))

(defun grep-parse (string)
  (grep-parse-lines (uiop:split-string string :separator '(#\newline))))

(defun grep-with-string (buffer-name string)
  (let ((grep (make-grep buffer-name))
        (current-directory (buffer-directory)))
    (dolist (elt (grep-parse string))
      (destructuring-bind (item filename linum) elt
        (setf filename (expand-file-name filename current-directory))
        (grep-append grep
                     item
                     (lambda ()
                       (find-file filename)
                       (goto-line linum)))))
    (grep-update grep)))

(define-command grep (string) ((list (minibuf-read-string ": " "grep -nH ")))
  (let ((directory (buffer-directory)))
    (grep-with-string "*grep*"
                      (with-output-to-string (s)
                        (uiop:run-program (format nil "cd ~A; ~A" directory string)
                                          :output s
                                          :error-output s
                                          :ignore-error-status t)))))

(defun grep-jump-current-content ()
  (let ((content (aref (grep-contents *current-grep*)
                       (grep-index *current-grep*))))
    (funcall (grep-content-jump-function content))))

(define-key *global-keymap* (kbd "M-n") 'grep-next)
(define-command grep-next () ()
  (when *current-grep*
    (when (< (1+ (grep-index *current-grep*))
             (length (grep-contents *current-grep*)))
      (incf (grep-index *current-grep*))
      (grep-jump-current-content))))

(define-key *global-keymap* (kbd "M-p") 'grep-prev)
(define-command grep-prev () ()
  (when *current-grep*
    (when (<= 0 (1- (grep-index *current-grep*)))
      (decf (grep-index *current-grep*))
      (grep-jump-current-content))))

(in-package :cl-user)
(defpackage :lem.grep
  (:use :cl :lem)
  (:export
   :make-grep
   :put-entry-property
   :call-with-writer
   :update
   :grep-with-string
   :grep
   :grep-next
   :grep-prev))
(in-package :lem.grep)

(defvar *attribute-1* (make-attribute "blue" nil))
(defvar *attribute-2* (make-attribute "red" nil))

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
    :accessor grep-index)
   (firstp
    :initform t
    :accessor grep-firstp)))

(defun grep-content (content) (first content))

(defun make-grep (buffer-name)
  (make-instance 'grep :buffer-name buffer-name))

(defun put-entry-property (grep start end jump-function)
  (vector-push-extend (list jump-function) (grep-contents grep))
  (put-property start end 'entry jump-function))

(defun call-with-writer (grep function)
  (let ((buffer (get-buffer-create (grep-buffer-name grep))))
    (when (grep-firstp grep)
      (setf (grep-firstp grep) nil)
      (buffer-erase buffer))
    (save-excursion
      (setf (current-buffer) buffer)
      (point-set (point-max))
      (funcall function))))

(defun update (grep)
  (setf (grep-index grep) -1)
  (display-buffer (set-buffer-mode (get-buffer-create (grep-buffer-name grep))
                                   (lambda () (grep-mode t))))
  (setf *current-grep* grep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grep-parse-line (line)
  (ignore-errors
   (let* ((i (position #\: line))
          (j (position #\: line :start (1+ i)))
          (filename (subseq line 0 i))
          (linum (parse-integer (subseq line (1+ i) j))))
     (when (and (stringp filename) (integerp linum))
       (list filename linum (subseq line j))))))

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
      (destructuring-bind (filename linum thing) elt
        (let* ((filename (expand-file-name filename current-directory))
               (jump-fun (lambda ()
                           (find-file filename)
                           (goto-line linum))))
          (call-with-writer grep
                            (lambda ()
                              (insert-string-with-attribute filename *attribute-1*)
                              (insert-string ":")
                              (insert-string-with-attribute (princ-to-string linum) *attribute-2*)
                              (insert-string thing)
                              (put-entry-property grep
                                                  (beginning-of-line-point)
                                                  (end-of-line-point)
                                                  jump-fun)
                              (insert-newline 1))))))
    (update grep)))

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
    (funcall (grep-content content))))

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

(define-minor-mode grep-mode
    (:name "grep"
     :keymap *grep-mode-keymap*))

(define-key *grep-mode-keymap* "C-m" 'grep-jump)
(define-key *grep-mode-keymap* "q" 'quit-window)

(define-command grep-jump () ()
  (let ((jump-function (get-property (current-point) 'entry)))
    (when jump-function
      (funcall jump-function))))

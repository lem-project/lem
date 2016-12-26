(in-package :cl-user)
(defpackage :lem.grep
  (:use :cl :lem)
  (:export :grep))
(in-package :lem.grep)

(defvar *attribute-1* (make-attribute "blue" nil))
(defvar *attribute-2* (make-attribute "red" nil))

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
  (lem.sourcelist:with-sourcelist (sourcelist buffer-name)
    (let ((current-directory (buffer-directory)))
      (dolist (elt (grep-parse string))
        (destructuring-bind (filename linum thing) elt
          (let* ((filename (expand-file-name filename current-directory))
                 (jump-fun (lambda ()
                             (find-file filename)
                             (goto-line linum))))
            (lem.sourcelist:append-sourcelist
             sourcelist
             (lambda (cur-point)
               (insert-string cur-point
			      filename
			      :attribute *attribute-1*)
               (insert-string cur-point ":")
               (insert-string cur-point
			      (princ-to-string linum)
			      :attribute *attribute-2*)
               (insert-string cur-point thing))
             jump-fun)))))))

(define-command grep (string) ((list (prompt-for-string ": " "grep -nH ")))
  (let ((directory (buffer-directory)))
    (grep-with-string "*grep*"
                      (with-output-to-string (s)
                        (uiop:run-program (format nil "cd ~A; ~A" directory string)
                                          :output s
                                          :error-output s
                                          :ignore-error-status t)))))

(cl:defpackage :lem-sblint
  (:use :cl :lem)
  (:export
   :sblint-load))
(in-package :lem-sblint)

(defun sblint-internal (fn arg find-file-fn)
  (let ((text
         (with-output-to-string (stream)
           (funcall fn arg stream)))
        (grep
         (lem.grep:make-grep (format nil "*sblint ~A*" arg))))
    (dolist (line (uiop:split-string text :separator '(#\newline)))
      (ppcre:do-register-groups (file linum charpos rest-text) ("^(\\S+):(\\d+):(\\d+):\\s*(.*)" line)
        (setf linum (parse-integer linum)
              charpos (parse-integer charpos))
        (let ((jump-fn
               (lambda ()
                 (funcall find-file-fn arg file)
                 (goto-line linum)
                 (shift-position charpos))))
          (lem.grep:call-with-writer grep
                                     (lambda ()
                                       (insert-string-with-attribute file
                                                                     lem.grep::*attribute-1*)
                                       (insert-string ":")
                                       (insert-string-with-attribute (princ-to-string linum)
                                                                     lem.grep::*attribute-2*)
                                       (insert-string ":")
                                       (insert-string-with-attribute (princ-to-string charpos)
                                                                     lem.grep::*attribute-2*)
                                       (insert-string ":")
                                       (insert-string rest-text)
                                       (lem.grep:put-entry-property grep
                                                                    (beginning-of-line-point)
                                                                    (end-of-line-point)
                                                                    jump-fn)
                                       (insert-newline 1))))))
    (lem.grep:update grep)))

(define-key lem.lisp-mode:*lisp-mode-keymap* "C-c C-l" 'sblint-load-file)
(define-command sblint-load-file (filename) ("fsblnt file: ")
  (sblint-internal #'sblint:run-lint-file filename
                   (lambda (filename file)
                     (declare (ignore file))
                     (find-file filename))))

(define-command sblint-load-directory (dirname)
  ((list
    (minibuf-read-file "sblint directory: "
                       (buffer-directory)
                       (buffer-directory))))
  (sblint-internal #'sblint:run-lint-directory dirname
                   (lambda (dirname file)
                     (declare (ignore dirname))
                     (find-file file))))

(cl:defpackage :lem-sblint
  (:use :cl :lem :lem.grep)
  (:export
   :sblint-load))
(in-package :lem-sblint)

(defun sblint-internal (fn arg find-file-fn)
  (let ((text
         (with-output-to-string (stream)
           (funcall fn arg stream)))
        (grep
         (make-grep (format nil "*sblint ~A*" arg))))
    (dolist (line (uiop:split-string text :separator '(#\newline)))
      (ppcre:do-register-groups (file linum charpos) ("^(\\S+):(\\d+):(\\d+):\\s*(.*)" line)
        (setf linum (parse-integer linum)
              charpos (parse-integer charpos))
        (grep-append grep
                     line
                     (lambda ()
                       (funcall find-file-fn arg file)
                       (goto-line linum)
                       (shift-position charpos)))))
    (grep-update grep)))

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

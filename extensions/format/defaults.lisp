(defpackage #:lem-format/defaults
  (:use :cl :alexandria-2 :lem :lem-format))
(in-package :lem-format/defaults)

(defun walk-to-root (path)
  "Walk up the file tree collecting directories."
  (let ((dir (uiop:pathname-directory-pathname path)))
    (if (equal #P"/" dir)
        (list dir)
        (cons dir (walk-to-root (uiop:pathname-parent-directory-pathname dir))))))

(defun file-in-path (file path)
  "Test if a file exists in a directory path."
  (uiop:file-exists-p (merge-pathnames path file)))

(defun clang-spec (path)
  "Walk up file tree looking for .clang-format spec."
  (line-up-last
   (walk-to-root path)
   (mapcar (curry #'file-in-path ".clang-format"))
   (find-if #'identity)))

(defun clang-format (buf)
  "Format a C buffer with clang-format."
  (let ((file (buffer-filename buf)))
    (uiop:run-program
     (format nil "clang-format --style=file:~a -i ~a" (clang-spec file) file)
     :ignore-error-status t))
  (revert-buffer t))

(defun gofmt (buf)
  "Format a Go buffer with gofmt."
  (let ((file (buffer-filename buf)))
    (uiop:run-program 
     (format nil "gofmt -w ~a" file)
     :ignore-error-status t))
  (revert-buffer t))

;; register default formatters, these may be overwritten.
(register-formatters
  ('lem-go-mode:go-mode #'gofmt)
  ('lem-c-mode:c-mode   #'clang-format)
  ('lem-lisp-mode:lisp-mode #'indent-buffer))

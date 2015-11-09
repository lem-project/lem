;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(filter-buffer pipe-command))

(define-key *global-keymap* (kbd "C-x #") 'filter-buffer)
(define-command filter-buffer (str) ("sFilter buffer: ")
  (let ((outstr (make-array '(0)
                            :element-type 'character
                            :fill-pointer t)))
    (with-output-to-string (output outstr)
      (let ((temp-file-name (temp-file-name)))
        (write-to-file (window-buffer) temp-file-name)
        (shell-command (format nil "cat ~a | ~a" temp-file-name str)
                       :output output)
        (delete-file temp-file-name)))
    (let ((*kill-disable-p* nil))
      (kill-region (point-min) (point-max)))
    (insert-string outstr)
    (beginning-of-buffer)))

(define-key *global-keymap* (kbd "C-x @") 'pipe-command)
(define-command pipe-command (str) ("sPipe command: ")
  (info-popup (get-buffer-create "*Command*")
              #'(lambda (out)
                  (shell-command str :output out))))

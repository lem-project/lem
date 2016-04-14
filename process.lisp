;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(filter-buffer pipe-command))

(define-key *global-keymap* (kbd "C-x #") 'filter-buffer)
(define-command filter-buffer (str) ("sFilter buffer: ")
  (let (begin end)
    (cond ((buffer-mark-p)
           (setq begin (region-beginning))
           (setq end (region-end)))
          (t
           (setq begin (point-min))
           (setq end (point-max))))
    (let ((input-string
           (region-string begin end))
          (outstr (make-array '(0)
                              :element-type 'character
                              :fill-pointer t))
          output-value
          error-output-value
          status)
      (with-output-to-string (output outstr)
        (with-input-from-string (input input-string)
          (multiple-value-setq (output-value error-output-value status)
                               (uiop:run-program str
                                                 :input input
                                                 :output output
                                                 :ignore-error-status t))))
      (delete-region begin end)
      (insert-string outstr)
      (point-set begin)
      (minibuf-print (format nil "~D ~A" (write-to-string status) error-output-value))
      (zerop status))))

(define-key *global-keymap* (kbd "C-x @") 'pipe-command)
(define-command pipe-command (str) ("sPipe command: ")
  (info-popup (get-buffer-create "*Command*")
              #'(lambda (out)
                  (uiop:run-program str
                                    :output out
                                    :ignore-error-status t))))

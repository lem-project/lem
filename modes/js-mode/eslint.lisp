(defpackage :lem-js-mode.eslint
  (:use :cl :lem)
  (:export :eslint))
(in-package :lem-js-mode.eslint)

(defun run-eslint (buffer)
  (with-output-to-string (output)
    (uiop:run-program (format nil "eslint '~A'" (buffer-filename buffer))
                      :ignore-error-status t
                      :directory (buffer-directory buffer)
                      :output output
                      :error-output output)))

(defun parse-eslint-output (text)
  (with-input-from-string (in text)
    (loop :for line := (read-line in nil)
          :while line
          :for result := (ppcre:register-groups-bind (line-number column description)
                             ("^\\s*(\\d+):(\\d+)\\s*(.*)" line)
                           (setq line-number (parse-integer line-number)
                                 column (parse-integer column))
                           (list line-number column description))
          :when result
          :collect it)))

(define-command eslint () ()
  (let* ((buffer (current-buffer))
         (notes (parse-eslint-output (run-eslint buffer))))
    (lem/sourcelist:with-sourcelist (sourcelist "*eslint*")
      (dolist (note notes)
        (destructuring-bind (line-number column description) note
          (lem/sourcelist:append-sourcelist
           sourcelist
           (lambda (point)
             (insert-string point (princ-to-string line-number)
                            :attribute 'lem/sourcelist:position-attribute)
             (insert-character point #\:)
             (insert-string point (princ-to-string column)
                            :attribute 'lem/sourcelist:position-attribute)
             (insert-character point #\:)
             (insert-string point description))
           (lambda (set-buffer-fn)
             (let ((point (buffer-point buffer)))
               (move-to-line point line-number)
               (move-to-column point (1- column)))
             (funcall set-buffer-fn buffer))))))))

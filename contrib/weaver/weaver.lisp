(defpackage :lem.weaver
  (:use :cl :lem))
(in-package :lem.weaver)

(setf lem-lisp-mode::*record-history-of-repl* t)

(defun omit-string-if-too-long (string &optional (limit 30))
  (if (< limit (string-width string))
      (format nil "~A..." (subseq string 0 (wide-index string (- limit 3))))
      string))

(defun replace-newline-with-space (string)
  (ppcre:regex-replace-all "\\n" string " "))

(defun repl-history ()
  (reverse lem-lisp-mode::*repl-history*))

(define-command display-lisp-repl-history () ()
  (lem.menu-mode:display-menu
   (make-instance 'lem.menu-mode:menu
                  :columns '("Input" "Output")
                  :items (repl-history)
                  :column-function (lambda (item)
                                     (let ((input (car item))
                                           (output (cdr item)))
                                       (list (omit-string-if-too-long
                                              (replace-newline-with-space input))
                                             (replace-newline-with-space
                                              (format nil "~{~S~^,~}" output)))))
                  :update-items-function 'repl-history)
   :name "Repl History"))

(define-command make-unit-test-from-history () ()
  (alexandria:when-let ((buffer (get-buffer "*Repl History*")))
    (save-excursion
      (setf (current-buffer) buffer)
      (let ((output-buffer (make-buffer "*Unit Test*")))
        (erase-buffer output-buffer)
        (change-buffer-mode output-buffer 'lem-lisp-mode:lisp-mode)
        (with-open-stream (stream (make-buffer-output-stream (buffer-point output-buffer)))
          (loop :for (input . values) :in (lem.menu-mode::menu-current-items :marked t)
                :do (let ((input-form (read-from-string input)))
                      (pprint `(assert (equal (multiple-value-list ,input-form)
                                              ',values))
                              stream))))
        (pop-to-buffer output-buffer)))))

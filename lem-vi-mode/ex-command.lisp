(defpackage :lem-vi-mode.ex-command
  (:use :cl :lem-vi-mode.ex-util)
  (:export :*point*
           :search-forward
           :search-backward
           :goto-line
           :current-line
           :last-line
           :marker
           :offset-line
           :goto-current-point
           :range
           :all-lines
           :call-ex-command
           :define-ex-command))
(in-package :lem-vi-mode.ex-command)

(defvar *point*)
(defvar *command-table* '())

(defun search-forward (pattern)
  (lem:search-forward-regexp *point* pattern))

(defun search-backward (pattern)
  (lem:search-backward-regexp *point* pattern))

(defun goto-line (line-number)
  (lem:move-to-line *point* line-number))

(defun current-line ()
  *point*)

(defun last-line ()
  (lem:buffer-end *point*))

(defun marker (char)
  (declare (ignore char)))

(defun offset-line (offset)
  (declare (ignore offset)))

(defun goto-current-point (range)
  (declare (ignore range)))

(defun range (&rest range)
  range)

(defun all-lines ()
  (let ((buffer (lem:point-buffer *point*)))
    (list (lem:copy-point (lem:buffer-start-point buffer) :temporary)
          (lem:copy-point (lem:buffer-end-point buffer) :temporary))))

(defun call-ex-command (range command argument)
  (let ((function (find-ex-command command)))
    (unless function
      (lem:editor-error "unknown command: ~A" command))
    (funcall function range argument)))

(defun find-ex-command (command)
  (loop :for (names function) :in *command-table*
        :do (when (member command names :test #'string=)
              (return function))))

(defmacro define-ex-command (names (range argument) &body body)
  `(push (list (list . ,(alexandria:ensure-list names)) (lambda (,range ,argument) ,@body))
         *command-table*))

(defun ex-write (range filename)
  (when (string= filename "")
    (setf filename (lem:buffer-filename (lem:current-buffer))))
  (case (length range)
    (0 (lem:write-file filename))
    (2 (lem:write-region-file (first range) (second range) filename))
    (otherwise (syntax-error))))

(defun ex-write-quit (range filename force)
  (ex-write range filename)
  (lem-vi-mode.commands:vi-quit force))

(define-ex-command ("w" "write") (range filename)
  (ex-write range filename))

(define-ex-command ("wq") (range filename)
  (ex-write-quit range filename nil))

(define-ex-command ("wq!") (range filename)
  (ex-write-quit range filename t))

(define-ex-command ("q") (range argument)
  (declare (ignore range argument))
  (lem-vi-mode.commands:vi-quit t))

(define-ex-command ("q!") (range argument)
  (declare (ignore range argument))
  (lem-vi-mode.commands:vi-quit nil))

(define-ex-command ("sp") (range filename)
  (declare (ignore range))
  (lem:split-active-window-vertically)
  (unless (string= filename "")
    (lem:find-file (lem:expand-file-name filename))))

(define-ex-command ("vs") (range filename)
  (declare (ignore range))
  (lem:split-active-window-horizontally)
  (unless (string= filename "")
    (lem:find-file (lem:expand-file-name filename))))

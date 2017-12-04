(defpackage :lem-vi-mode.ex-command
  (:use :cl)
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
           :call-ex-command))
(in-package :lem-vi-mode.ex-command)

(defvar *point*)

(defun search-forward (pattern)
  (lem:search-forward-regexp *point* pattern))

(defun search-backward (pattern)
  (lem:search-backward-regexp *point* pattern))

(defun goto-line (line-number)
  (lem:move-to-line *point* line-number))

(defun current-line ()
  *point*)

(defun last-line ()
  (lem:line-end *point*))

(defun marker (char)
  (declare (ignore char)))

(defun offset-line (offset)
  (declare (ignore offset)))

(defun goto-current-point (range)
  (declare (ignore range)))

(defun range (&rest range)
  range)

(defun all-lines ()
  (let ((buffer (point-buffer *point*)))
    (list (copy-point (lem:buffer-start-point buffer) :temporary)
          (copy-point (lem:buffer-end-point buffer) :temporary))))

(defun call-ex-command (range command argument)
  (declare (ignore range command argument)))

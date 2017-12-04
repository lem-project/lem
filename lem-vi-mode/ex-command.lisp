(defpackage :lem-vi-mode.ex-command
  (:use :cl)
  (:export :search-forward
           :search-backward
           :goto-line
           :current-line
           :last-line
           :all-lines
           :marker
           :offset-line
           :goto-current-point
           :range
           :call-ex-command))
(in-package :lem-vi-mode.ex-command)

(defun search-forward (pattern)
  (declare (ignore pattern)))

(defun search-backward (pattern)
  (declare (ignore pattern)))

(defun goto-line (line-number)
  (declare (ignore line-number)))

(defun current-line ()
  )

(defun last-line ()
  )

(defun all-lines ()
  )

(defun marker (char)
  (declare (ignore char)))

(defun offset-line (offset)
  (declare (ignore offset)))

(defun goto-current-point (range)
  (declare (ignore range)))

(defun range (range)
  (declare (ignore range)))

(defun call-ex-command (range command argument)
  (declare (ignore range command argument)))

(defpackage :lem-display-time-mode
  (:use :cl :lem)
  (:export :enable
           :disable))
(in-package :lem-display-time-mode)

(defun display-time (window)
  (declare (ignore window))
  (multiple-value-bind (second minute hour)
      (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (values (format nil "Clock: ~a:~a" minute hour))))

(defun enable ()
  (modeline-add-status-list 'display-time))

(defun disable ()
   (modeline-remove-status-list 'display-time))

(lem:define-minor-mode display-time-mode
    (:name "Display Time Mode"
     :description "Displays the time in the modeline."
     :global t
     :enable-hook 'enable
     :disable-hook 'disable))
(defpackage :lem-display-time-mode
  (:use :cl :lem)
  (:export :enable
           :disable))
(in-package :lem-display-time-mode)

(define-attribute modeline-time-attribute
  (t :foreground "white" :background "#A0A0A0"))

(defun display-time (window)
  (declare (ignore window))
  (multiple-value-bind (second minute hour-24)
      (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (let* ((hour (mod hour-24 12))
           (am/pm (if (= hour hour-24) "AM" "PM")))
    (values (format nil " ~a:~a~a " hour minute am/pm)
            'modeline-time-attribute))))

(defun enable ()
  (setf (variable-value 'lem:modeline-format) 
        (cons 'display-time (variable-value 'lem:modeline-format))))

(defun disable ()
  (setf (variable-value 'lem:modeline-format)
        (remove 'display-time (variable-value 'lem:modeline-format))))

(lem:define-minor-mode display-time-mode
    (:name "Display Time Mode"
     :description "Displays the time in the modeline."
     :global t
     :enable-hook 'enable
     :disable-hook 'disable))
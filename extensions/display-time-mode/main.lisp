(defpackage :lem-display-time-mode
  (:use :cl :lem)
  (:export :enable
           :disable
           :display-time
           :display-time-mode))
(in-package :lem-display-time-mode)

(defun display-time (window)
  (declare (ignore window))
  (multiple-value-bind (second minute hour-24)
      (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (let* ((hour (if (> hour-24 12) (- hour-24 12) hour-24))
           (am/pm (if (= hour hour-24) "AM" "PM")))
    (values (format nil " ~d:~2,'0d~a " hour minute am/pm)
            'lem-core:modeline))))

(defun enable ()
  "Adds the display time function to the modeline format"
  (symbol-macrolet ((fmt (variable-value 'lem:modeline-format :global)))
    (let* ((new-fmt (append fmt (list '(display-time nil :right)))))
      (setf fmt new-fmt))))

(defun disable ()
  "Removes the display time function from the modeline format"
  (setf (variable-value 'lem:modeline-format :global)
        (remove 'display-time
                (variable-value 'lem:modeline-format :global)
                :key (lambda (obj)
                       (if (listp obj)
                           (first obj)
                           obj)))))

(lem:define-minor-mode display-time-mode
    (:name "Display Time Mode"
     :description "Displays the time in the modeline."
     :global t
     :enable-hook 'enable
     :disable-hook 'disable
     :hide-from-modeline t))
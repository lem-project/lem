(in-package :lem-capi)

(defparameter *log* nil)

(defun log-format (fmt &rest args)
  (when *log*
    (with-open-file (out "~/DEBUG" :direction :output :if-does-not-exist :create :if-exists :append)
      (fresh-line out)
      (apply #'format out fmt args)
      (terpri out))))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error (lambda (c)
                               (capi:display-message "error: ~A" c)
                               (lem:pdebug (format nil "~%******ERROR******:~%~A~%" c))
                               (lem:pdebug (with-output-to-string (out)
                                             (uiop:print-backtrace :stream out :condition c))))))
         (progn ,@body))
     (error ())))

(defmacro with-apply-in-pane-process-wait-single ((pane) &body body)
  `(capi:apply-in-pane-process-wait-single ,pane nil (lambda () ,@body)))

(defun convert-color (color &optional default-color)
  (if-let (rgb (lem:parse-color color))
    (let ((n (/ 1.0 255)))
      (destructuring-bind (r g b) rgb
        (color:make-rgb (* r n) (* g n) (* b n))))
    default-color))

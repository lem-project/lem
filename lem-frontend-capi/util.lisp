(in-package :lem-capi)

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error (lambda (c)
                               (lem:pdebug (format nil "~%******ERROR******:~%~A~%" c))
                               (lem:pdebug (with-output-to-string (out)
                                             (uiop:print-backtrace :stream out :condition c))))))
         (progn ,@body))
     (error ())))

(defun capi.apply-in-pane-process-wait-single (pane timeout function &rest args)
  (let ((mb (mp:make-mailbox :name "Apply-In-Pane-Process-Wait-Single")))
    (apply (lambda (&rest args)
             (declare (dynamic-extent args))
             (mp:mailbox-send
              mb 
              (apply #'capi:apply-in-pane-process-if-alive
                     pane
                     function
                     args)))
           args)
    (multiple-value-bind (result status)
                         (mp:mailbox-read mb 
                                          "Waiting for apply-in-pane-process"
                                          timeout)
      (values result
              (or status :timeout)))))

(declaim (inline apply-in-pane-process-wait-single))
(defun apply-in-pane-process-wait-single 
       (pane timeout function &rest args)
  (apply #+lispworks7.1 #'capi:apply-in-pane-process-wait-single
         #+lispworks7.0 #'capi.apply-in-pane-process-wait-single
         pane 
         timeout
         function 
         args))

(defmacro with-apply-in-pane-process-wait-single ((pane &optional timeout) &body body)
  `(apply-in-pane-process-wait-single ,pane ,timeout (lambda () ,@body)))

(defun convert-color (color &optional default-color)
  (if-let (rgb (lem:parse-color color))
    (let ((n (/ 1.0 255)))
      (destructuring-bind (r g b) rgb
        (color:make-rgb (* r n) (* g n) (* b n))))
    default-color))

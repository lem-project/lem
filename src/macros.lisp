(in-package :lem)

(export '(with-current-window
          with-pop-up-typeout-window
          handler-case-bind))

(defmacro with-current-window (window &body body)
  (let ((gprev-window (gensym "PREV-WINDOW"))
        (gwindow (gensym "WINDOW")))
    `(let ((,gprev-window (current-window))
           (,gwindow ,window))
       (setf (current-window) ,gwindow)
       (unwind-protect (progn ,@body)
         (unless (deleted-window-p ,gprev-window)
           (setf (current-window) ,gprev-window))))))

(defmacro with-pop-up-typeout-window ((stream-var buffer &key focus erase) &body body)
  `(pop-up-typeout-window ,buffer
                          (lambda (,stream-var) ,@body)
                          :focus ,focus
                          :erase ,erase))

#+sbcl
(defmacro with-profile (&body body)
  `(progn
     (sb-profile:profile "LEM")
     ,@body
     (with-open-file (out "PROFILE"
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
       (let ((*terminal-io* out)
             (*standard-output* out)
             (*standard-input* out)
             (*error-output* out)
             (*query-io* out)
             (*debug-io* out)
             (*trace-output* out))
         (sb-profile:report)))))

(defmacro handler-case-bind ((error-bind &body body)
					   ((condition) &body protected-form))
  (let ((gerror-bind (gensym "ERROR-BIND")))
    `(let ((,gerror-bind ,error-bind))
       (handler-case
           (handler-bind ((error ,gerror-bind)
                          #+sbcl (sb-sys:interactive-interrupt ,gerror-bind)
                          #+ccl (ccl:interrupt-signal-condition ,gerror-bind)
                          #+ecl (ext:interactive-interrupt ,gerror-bind))
             #+ccl
             (let ((ccl:*break-hook*
                    #'(lambda (condition hook)
                        (declare (ignore hook))
                        (error condition))))
               ,@body)
             #-ccl
             (progn ,@body))
         ((or error
	   #+sbcl sb-sys:interactive-interrupt
	   #+ccl ccl:interrupt-signal-condition)
	     (,condition)
	   ,@protected-form)))))

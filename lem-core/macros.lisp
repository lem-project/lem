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

(defmacro with-debug-output ((filename) &body body)
  `(with-open-file (out ,filename
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
       ,@body)))

#+sbcl
(defmacro with-profile (&body body)
  `(progn
     (sb-profile:profile "LEM" "LEM-BASE" "LEM-INTERFACE")
     ,@body
     (with-debug-output ("PROFILE")
       (sb-profile:report))
     (sb-profile:unprofile)))

(defmacro handler-case-bind ((error-bind &body body)
                             ((condition) &body protected-form))
  `(handler-case
       (handler-bind ((error ,error-bind))
         (progn ,@body))
     (error (,condition) ,@protected-form)))

(in-package :lem)

(export '(*program-name*
          *debug-p*
          save-excursion
          with-current-buffer
          with-window-range
          with-buffer-read-only
          with-current-window
          with-pop-up-typeout-window
          handler-case-bind))

(defvar *program-name* "Lem")
(defvar *debug-p* nil)

(defmacro define-class (name () default-arg-expr &body slots)
  (let ((garg (gensym "ARG"))
        (gval (gensym "VAL")))
    `(progn
       (defclass ,name ()
         ,(mapcar #'(lambda (slot)
                      `(,slot :initarg ,(intern (symbol-name slot) :keyword)
                              :initform nil))
                  slots))
       ,@(mapcan #'(lambda (slot)
                     (let ((name (intern (format nil "~a-~a" name slot))))
                       `((defun ,name (&optional (,garg ,default-arg-expr))
                           (slot-value ,garg ',slot))
                         (defun (setf ,name)
                             (,gval &optional (,garg ,default-arg-expr))
                           (setf (slot-value ,garg ',slot) ,gval)
                           ,gval))))
                 slots))))

(defmacro save-excursion (&body body)
  (let ((gpoint (gensym))
        (gbuffer (gensym))
        (gview-point (gensym))
        (gwindow-buffer (gensym)))
    `(let ((,gpoint (current-point))
           (,gbuffer (current-buffer))
           (,gview-point (marker-point (window-view-marker (current-window))))
           (,gwindow-buffer (window-buffer (current-window))))
       (unwind-protect (progn ,@body)
         (cond ((find ,gbuffer (buffer-list))
                (unless (eq ,gbuffer (current-buffer))
                  (setf (current-buffer) ,gbuffer))
                (setf (marker-point (window-view-marker (current-window)))
                      (let ((point-max (point-max (window-buffer (current-window)))))
                        (if (point< ,gview-point point-max)
                            ,gview-point
                            point-max)))
                (point-set ,gpoint)
                (unless (eq ,gwindow-buffer (window-buffer (current-window)))
                  (setf (window-buffer (current-window)) ,gwindow-buffer)))
               ((minibufferp ,gbuffer)
                (setf (current-buffer) ,gbuffer)
                (point-set ,gpoint (minibuffer))))))))

(defmacro with-current-buffer ((buffer point) &body body)
  `(save-excursion
     (setf (current-buffer) ,buffer)
     (setf (current-point) ,point)
     ,@body))

(defmacro with-window-range ((start-linum-var end-linum-var)
                             window &body body)
  (let ((gwindow (gensym "WINDOW")))
    `(let ((,gwindow ,window))
       (window-see ,gwindow)
       (let* ((,start-linum-var (window-view-linum ,gwindow))
              (,end-linum-var (+ ,start-linum-var (window-height ,gwindow))))
         ,@body))))

(defmacro with-buffer-read-only (buffer flag &body body)
  (let ((gbuffer (gensym "BUFFER"))
        (gtmp (gensym "GTMP")))
    `(let* ((,gbuffer ,buffer)
            (,gtmp (buffer-read-only-p ,gbuffer)))
       (setf (buffer-read-only-p ,gbuffer) ,flag)
       (unwind-protect (progn ,@body)
         (setf (buffer-read-only-p ,gbuffer) ,gtmp)))))

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

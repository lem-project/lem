(in-package :lem)

(export '(*program-name*
          *debug-p*
          save-excursion
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
  `(invoke-save-excursion (lambda () ,@body)))

(defmacro with-marker (bindings &body body)
  (let ((cleanups
         (mapcan (lambda (b)
                   (destructuring-bind (var marker &optional (kind :temporary)) b
                     (declare (ignore marker))
                     (unless (eq :temporary kind)
                       `((delete-point ,var)))))
                 bindings)))
    `(let ,(mapcar (lambda (b)
                     (destructuring-bind (var marker &optional (kind :temporary)) b
                       `(,var (copy-marker ,marker ,kind))))
                   bindings)
       ,(if cleanups
            `(unwind-protect (progn ,@body)
               ,@cleanups)
            `(progn ,@body)))))

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

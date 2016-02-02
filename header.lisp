;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*program-name*
          *debug-p*
          *tab-size*
          *enable-syntax-highlight*
          *last-input-key*
          *scroll-recenter-p*
          *auto-mode-alist*
          set-attr
          get-attr
          make-attr
          define-continue-flag
          if-continue-flag
          when-interrupted-flag
          when-continue-flag
          save-excursion
          with-window-range
          with-buffer-read-only
          with-current-window))

(defvar *program-name* "Lem")
(defvar *debug-p* nil)

(defvar *current-window*)
(defvar *minibuf-window*)
(defvar *buffer-list* nil)

(defvar *tab-size* 8)

(defvar *enable-syntax-highlight* t)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *last-input-key*)
(defvar *universal-argument* nil)

(defvar *scroll-recenter-p* t)

(defvar *auto-mode-alist* nil)

(defvar *attribute-name-table* (make-hash-table :test 'equal))

(defun get-attr (name)
  (gethash name *attribute-name-table*))

(defun set-attr (name attr)
  (unless (integerp attr)
    (setq attr (get-attr name)))
  (check-type attr integer)
  (setf (gethash name *attribute-name-table*) attr))

(defun make-attr (&key color reverse-p bold-p underline-p)
  (logior (or (get-attr color) 0)
          (if reverse-p
              charms/ll:a_reverse
              0)
          (if bold-p
              charms/ll:a_bold
              0)
          (if underline-p
              charms/ll:a_underline
              0)))

(defvar *continue-command-flags*
  (list :next-line :kill :undo :abbrev :yank :completion))

(defun define-continue-flag (keyword)
  (push keyword *continue-command-flags*))

(defun make-flags ()
  (mapcar #'(lambda (sym)
              (cons sym nil))
          *continue-command-flags*))

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

(defmacro if-continue-flag (flag then &optional else)
  (let ((gflag (gensym)))
    `(let ((,gflag ,flag))
       (if (cdr (assoc ,gflag *last-flags*))
           ,then
           ,else)
       (push (cons ,gflag t) *last-flags*)
       (push (cons ,gflag t) *curr-flags*))))

(defmacro when-interrupted-flag (flag &body body)
  `(if-continue-flag ,flag nil (progn ,@body)))

(defmacro when-continue-flag (flag &body body)
  `(if-continue-flag ,flag (progn ,@body) nil))

(defmacro save-excursion (&body body)
  (let ((gpoint (gensym))
        (gbuffer (gensym)))
    `(let ((,gpoint (point))
           (,gbuffer (window-buffer)))
       (unwind-protect (progn ,@body)
         (cond ((find ,gbuffer *buffer-list*)
                (when (not (eq ,gbuffer (window-buffer)))
                  (set-buffer ,gbuffer nil))
                (point-set ,gpoint))
               ((eq ,gbuffer (window-buffer *minibuf-window*))
                (point-set ,gpoint *minibuf-window*)))))))

(defmacro with-window-range ((start-linum-var end-linum-var)
                             window &body body)
  (let ((gwindow (gensym "WINDOW")))
    `(let ((,gwindow ,window))
       (window-adjust-view ,gwindow)
       (let* ((,start-linum-var (window-vtop-linum ,gwindow))
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
    `(let ((,gprev-window (selected-window))
           (,gwindow ,window))
       (select-window ,gwindow)
       (unwind-protect (progn ,@body)
         (unless (deleted-window-p ,gprev-window)
           (select-window ,gprev-window))))))

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
                          #+ccl (ccl:interrupt-signal-condition ,gerror-bind))
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

(let ((raw-mode))
  (defun raw-p ()
    raw-mode)
  (defun raw ()
    (setq raw-mode t)
    (charms/ll:raw))
  (defun noraw ()
    (setq raw-mode nil)
    (charms/ll:noraw)))

(defmacro with-raw (raw-p &body body)
  (let ((g-old-raw (gensym))
        (g-new-raw (gensym)))
    `(let ((,g-old-raw (raw-p))
           (,g-new-raw ,raw-p))
       (if ,g-new-raw
           (raw)
           (noraw))
       (unwind-protect (progn ,@body)
         (if ,g-old-raw
             (raw)
             (noraw))))))

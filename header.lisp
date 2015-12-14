;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*program-name*
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
          with-buffer-read-only))

(defvar *program-name* "Lem")

(defvar *current-window*)
(defvar *buffer-list* nil)

(defvar *tab-size* 8)

(defvar *enable-syntax-highlight* t)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *last-input-key*)
(defvar *universal-argument* nil)

(defvar *scroll-recenter-p* t)

(defvar *auto-mode-alist* nil)

(defvar *editor-lock* (bt:make-lock))
(defvar *getch-wait-p* nil)

(defvar *color-names* '(:yellow
                        :green
                        :blue
                        :magenta
                        :red
                        :cyan))

(defvar *attribute-name-table* (make-hash-table))

(defun set-attr (name attr)
  (setf (gethash name *attribute-name-table*)
        attr))

(defun get-attr (name)
  (gethash name *attribute-name-table*))

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

(defvar *continue-command-flags* (list :kill :undo :abbrev :yank :completion))

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
        (gmax-col (gensym))
        (gbuffer (gensym)))
    `(let ((,gpoint (point))
           (,gmax-col (window-max-col))
           (,gbuffer (window-buffer)))
       (unwind-protect (progn ,@body)
         (when (find ,gbuffer *buffer-list*)
           (when (not (eq ,gbuffer (window-buffer)))
             (set-buffer ,gbuffer nil))
           (point-set ,gpoint)
           (setf (window-max-col) ,gmax-col))))))

(defmacro with-window-range ((start-linum-var end-linum-var)
                             window &body body)
  (let ((gwindow (gensym "WINDOW")))
    `(let ((,gwindow ,window))
       (window-adjust-view ,gwindow)
       (let* ((,start-linum-var (window-vtop-linum ,gwindow))
              (,end-linum-var (+ ,start-linum-var (window-nlines ,gwindow))))
         ,@body))))

(defmacro with-buffer-read-only (buffer flag &body body)
  (let ((gbuffer (gensym "BUFFER"))
        (gtmp (gensym "GTMP")))
    `(let* ((,gbuffer ,buffer)
            (,gtmp (buffer-read-only-p ,gbuffer)))
       (setf (buffer-read-only-p ,gbuffer) ,flag)
       (unwind-protect (progn ,@body)
         (setf (buffer-read-only-p ,gbuffer) ,gtmp)))))

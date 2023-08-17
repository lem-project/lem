(defpackage :lem-vi-mode/commands/utils
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/jump-motions
                :with-jump-motion)
  (:import-from :lem-vi-mode/visual
                :visual-p
                :visual-line-p
                :apply-visual-range
                :vi-visual-end)
  (:import-from :lem/common/command
                :ensure-command)
  (:import-from :alexandria
                :with-gensyms
                :ensure-list)
  (:export :bolp
           :eolp
           :goto-eol
           :fall-within-line
           :read-universal-argument
           :*cursor-offset*
           :vi-command
           :vi-motion
           :vi-motion-type
           :vi-operator
           :define-vi-motion
           :define-vi-operator))
(in-package :lem-vi-mode/commands/utils)

(defun bolp (point)
  "Return t if POINT is at the beginning of a line."
  (zerop (point-charpos point)))

(defun eolp (point)
  "Return t if POINT is at the end of line."
  (let ((len (length (line-string point))))
    (or (zerop len)
        (>= (point-charpos point)
            (1- len)))))

(defun goto-eol (point)
  "Goto end of a line."
  (line-end point)
  (unless (bolp point)
    (character-offset point *cursor-offset*)))

(defun fall-within-line (point)
  (when (eolp point)
    (goto-eol point)))

(defun read-universal-argument ()
  (loop :for key := (read-key)
        :for char := (key-to-char key)
        :while (and char (digit-char-p char))
        :collect (digit-char-p char) :into digits
        :finally (unread-key key)
                 (return-from read-universal-argument
                   (and digits
                        (parse-integer (format nil "~{~D~}" digits))))))

(defclass vi-command () ())

(defclass vi-motion (vi-command)
  ((type :type keyword
         :initarg :type
         :initform :exclusive
         :accessor vi-motion-type)
   (default-n-arg :type (or null integer)
                  :initarg :default-n-arg
                  :initform 1
                  :accessor vi-motion-default-n-arg)))

(defclass vi-operator (vi-command) ())

(defvar *vi-origin-point*)
(defvar *cursor-offset* -1)

(defun parse-vi-motion-arg-list (arg-list)
  (check-type arg-list list)
  (cond
    ((null arg-list)
     (values () ()))
    ((eq (first arg-list) '&optional)
     (values
       arg-list
       '("p")
       (second (ensure-list (second arg-list)))))
    (t (values arg-list '("P") nil))))

(defmacro define-vi-motion (name arg-list (&key type jump) &body body)
  (check-type type (or null (member :inclusive :exclusive :line)))
  (check-type jump boolean)
  (multiple-value-bind (arg-list arg-descriptor default-n-arg)
      (parse-vi-motion-arg-list arg-list)
    `(define-command (,name (:advice-classes vi-motion)
                            (:initargs
                             :type ,(or type :exclusive)
                             :default-n-arg ,default-n-arg))
       ,arg-list ,arg-descriptor
       (with-point ((*vi-origin-point* (current-point)))
         (,(if jump 'with-jump-motion 'progn)
           ,@body)))))

(defun call-vi-motion-command (command n)
  (let* ((command (ensure-command command))
         (n (or n
                (typecase command
                  (vi-motion
                    (with-slots (default-n-arg) command
                      default-n-arg))
                  (otherwise 1)))))
    (call-command command n)))

(defvar *vi-operator-arguments* nil)

(defmacro define-vi-operator (name arg-list (&key motion keep-visual restore-point) &body body)
  (with-gensyms (start end n type command command-name)
    `(define-command (,name (:advice-classes vi-operator)) (&optional ,n) ("P")
       (with-point ((*vi-origin-point* (current-point)))
         (unwind-protect
             (if *vi-operator-arguments*
                 (destructuring-bind ,(and arg-list
                                           `(&optional ,@arg-list))
                     (subseq *vi-operator-arguments* 0 ,(length arg-list))
                   ,@body)
                 (with-point ((,start (current-point))
                              (,end (current-point)))
                   (let ((,type (if (visual-line-p)
                                    :line
                                    :exclusive)))
                     (if (visual-p)
                         (apply-visual-range
                           (lambda (vstart vend)
                             (setf ,start vstart
                                   ,end vend)))
                         ,(if motion
                              `(progn
                                 (let ((,command (get-command ',motion)))
                                   (let ((*cursor-offset* 0))
                                     (ignore-errors
                                       (call-vi-motion-command ,command ,n)))
                                   (when (typep ,command 'vi-motion)
                                     (setf ,type (vi-motion-type ,command))))
                                 (move-point ,end (current-point)))
                              `(let* ((,n (read-universal-argument))
                                      (,command-name (read-command))
                                      (,command (get-command ,command-name)))
                                 (typecase ,command
                                   (vi-operator
                                     ;; Recursive call of the operator like 'dd', 'cc'
                                     (when (eq ,command-name ',name)
                                       (setf ,type :line)
                                       (line-offset ,end (1- (or ,n 1)))))
                                   (otherwise
                                     (let ((*cursor-offset* 0))
                                       (ignore-errors
                                         (call-vi-motion-command ,command ,n)))
                                     (when (and (typep ,command 'vi-motion)
                                                (or (eq (vi-motion-type ,command) :line)
                                                    (point/= ,end (current-point))))
                                       (setf ,type (vi-motion-type ,command)))
                                     (move-point ,end (current-point)))))))
                     (when (point< ,end ,start)
                       (rotatef ,start ,end))
                     (ecase ,type
                       (:exclusive)
                       (:inclusive (character-offset ,end 1))
                       (:line (unless (visual-p)
                                (line-start ,start)
                                (line-end ,end))))
                     (let ((*vi-operator-arguments* (list ,start ,end ,type)))
                       (destructuring-bind ,(and arg-list
                                                 `(&optional ,@arg-list))
                           (subseq *vi-operator-arguments* 0 ,(length arg-list))
                         ,@body)))))
           ,@(when restore-point
               '((move-point (current-point) *vi-origin-point*)))
           ,@(unless keep-visual
               '((when (visual-p) (vi-visual-end)))))))))

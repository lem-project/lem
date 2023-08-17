(defpackage :lem-vi-mode/commands/utils
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/jump-motions
                :with-jump-motion)
  (:import-from :lem-vi-mode/visual
                :visual-p
                :visual-line-p
                :visual-block-p
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

(defvar *cursor-offset* -1)

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
  (check-type type (or null (member :inclusive :exclusive :line :block)))
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

(defun vi-operator-region (n motion)
  (check-type n (or null (integer 0)))
  (check-type motion (or null symbol))
  (flet ((call-motion (command uarg)
           (let ((*cursor-offset* 0))
             (ignore-errors
               (call-vi-motion-command command uarg))))
         (command-motion-type (command)
           (if (typep command 'vi-motion)
               (vi-motion-type command)
               :exclusive)))
    (with-point ((start (current-point)))
      (if motion
          (let ((command (get-command motion)))
            (call-motion command n)
            (values
              start
              (copy-point (current-point))
              (command-motion-type command)))
          (let* ((uarg (or (read-universal-argument) n))
                 (command-name (read-command))
                 (command (get-command command-name)))
            (typecase command
              (vi-operator
                (if (eq command-name (command-name (this-command)))
                    ;; Recursive call of the operator like 'dd', 'cc'
                    (with-point ((end (current-point)))
                      (line-offset end (1- (or uarg 1)))
                      (values start end :line))
                    ;; Ignore an invalid operator (like 'dJ')
                    nil))
              (otherwise
                (call-motion command uarg)
                (values start
                        (copy-point (current-point))
                        (command-motion-type command)))))))))

(defun call-vi-operator (n fn &key motion keep-visual restore-point)
  (flet ((call-with-region (fn start end type)
           (when (point< end start)
             (rotatef start end))
           (ecase type
             (:line (unless (visual-p)
                      (line-start start)
                      (line-end end)))
             (:block)
             (:inclusive
              (unless (point= start end)
                (character-offset end 1)))
             (:exclusive))
           (let ((*vi-operator-arguments* (list start end type)))
             (funcall fn start end type))))
    (with-point ((*vi-origin-point* (current-point)))
      (unwind-protect
           (if *vi-operator-arguments*
               (apply fn *vi-operator-arguments*)
               (if (visual-p)
                   (apply-visual-range
                    (lambda (start end)
                      (call-with-region fn start end
                                        (cond
                                          ((visual-line-p) :line)
                                          ((visual-block-p) :block)
                                          (t :exclusive)))))
                   (multiple-value-bind (start end type)
                       (vi-operator-region n motion)
                     (call-with-region fn start end type))))
        (when restore-point
          (move-point (current-point) *vi-origin-point*))
        (unless keep-visual
          (when (visual-p)
            (vi-visual-end)))))))

(defmacro define-vi-operator (name arg-list (&key motion keep-visual restore-point) &body body)
  (with-gensyms (n extra-args)
    `(define-command (,name (:advice-classes vi-operator)) (&optional ,n) ("P")
       (call-vi-operator ,n
                         (lambda (,@(and arg-list `(&optional ,@arg-list))
                                  &rest ,extra-args)
                           (declare (ignore ,extra-args))
                           ,@body)
                         :motion ',motion
                         :keep-visual ,keep-visual
                         :restore-point ,restore-point))))

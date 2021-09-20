(in-package :lem)

(export '(define-command))

(eval-when (:compile-toplevel :load-toplevel)
  (defun gen-defcommand-arg-parser (universal-argument arg-descriptors)
    (cond
      ((string= "p" (car arg-descriptors))
       `(list (or ,universal-argument 1)))
      ((string= "P" (car arg-descriptors))
       `(list ,universal-argument))
      ((string= "r" (car arg-descriptors))
       `(progn
          (check-marked)
          (list (region-beginning) (region-end))))
      (t
       (cons 'list
             (mapcar #'(lambda (arg-descriptor)
                         (cond
                           ((char= #\s (aref arg-descriptor 0))
                            `(prompt-for-string ,(subseq arg-descriptor 1)))
                           ((char= #\n (aref arg-descriptor 0))
                            `(prompt-for-integer ,(subseq arg-descriptor 1)))
                           ((char= #\b (aref arg-descriptor 0))
                            `(prompt-for-buffer ,(subseq arg-descriptor 1)
                                                :default (buffer-name (current-buffer))
                                                :existing t))
                           ((char= #\B (aref arg-descriptor 0))
                            `(prompt-for-buffer ,(subseq arg-descriptor 1)
                                                :default (buffer-name (other-buffer))
                                                :existing nil))
                           ((char= #\f (aref arg-descriptor 0))
                            `(prompt-for-file
                              ,(subseq arg-descriptor 1)
                              :directory (buffer-directory)
                              :default nil
                              :existing t))
                           ((char= #\F (aref arg-descriptor 0))
                            `(prompt-for-file
                              ,(subseq arg-descriptor 1)
                              :directory (buffer-directory)
                              :default nil
                              :existing nil))
                           (t
                            (error "Illegal arg-descriptor: ~a" arg-descriptor))))
                     arg-descriptors)))))

  (alexandria:with-unique-names (arguments)
    (defun gen-defcommand-body (fn-name
                                parms
                                universal-argument
                                arg-descriptors)
      `(block ,fn-name
         ,(if (null arg-descriptors)
              (progn
                (assert (null parms))
                `(,fn-name))
              `(destructuring-bind (&rest ,arguments)
                   ,(if (stringp (car arg-descriptors))
                        (gen-defcommand-arg-parser universal-argument arg-descriptors)
                        (car arg-descriptors))
                 (apply #',fn-name ,arguments)))))))

(defun primary-class (options)
  (let ((value (alexandria:assoc-value options :primary-class)))
    (cond ((null value)
           +primary-command-class-name+)
          (t
           (alexandria:length= (assert value) 1)
           (first value)))))

(defmacro define-command (name-and-options params (&rest arg-descriptors) &body body)
  (destructuring-bind (name . options) (uiop:ensure-list name-and-options)
    (let ((primary-class (primary-class options))
          (advice-classes (alexandria:assoc-value options :advice-classes))
          (command-name (string-downcase name)))
      (alexandria:with-unique-names (command universal-argument)
        `(progn
           (add-command ,command-name (make-cmd :name ',name))
           (defun ,name ,params
             ;; コマンドではなく直接この関数を呼び出した場合
             ;; - *this-command*が束縛されない
             ;; - executeのフックが使えない
             ,@body)
           (defclass ,name (,primary-class ,@advice-classes) ())
           (defmethod execute ((,command ,name) ,universal-argument)
             (declare (ignorable ,universal-argument))
             ,(gen-defcommand-body name
                                   params
                                   universal-argument
                                   arg-descriptors)))))))

#|
;;; example 1
(defclass foo-advice () ())

(define-command (foo-1 (:advice-classes foo-advice)) (p) ("p")
  ...body)

(define-command (foo-2 (:advice-classes foo-advice)) (s) ("sInput: ")
  ...body)

(defclass execute ((command foo-advice) argument)
  ;; :advice-classesをfoo-adviceにしたfoo-1とfoo-2コマンドだけが呼び出される
  )

;;; example 2
(defclass bar-primary () ())

(define-command (bar-1 (:primary-class bar-primary)) (p) ("p")
  ...body)

(define-command (bar-2 (:primary-class bar-primary)) (s) ("sInput: ")
  ...body)

(defclass execute ((command bar-primary) argument)
  ;; :primary-classをbar-primaryにしたbar-1,bar-2コマンドだけが呼び出される
  )

(defclass execute ((command primary-command) argument)
  ;; :primary-classがbar-primaryのときはこれは呼び出されない
  )
|#

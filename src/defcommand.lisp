(in-package :lem)

(eval-when (:compile-toplevel :load-toplevel)
  (defun parse-arg-descriptors (arg-descriptors universal-argument)
    (let* ((pre-forms '())
           (forms
             (mapcar (lambda (arg-descriptor)
                       (cond ((and (stringp arg-descriptor)
                                   (< 0 (length arg-descriptor)))
                              (ecase (char arg-descriptor 0)
                                (#\p
                                 `(list (or ,universal-argument 1)))
                                (#\P
                                 `(list ,universal-argument))
                                (#\s
                                 `(list (prompt-for-string ,(subseq arg-descriptor 1))))
                                (#\n
                                 `(list (prompt-for-integer ,(subseq arg-descriptor 1))))
                                (#\b
                                 `(list (prompt-for-buffer ,(subseq arg-descriptor 1)
                                                           :default (buffer-name (current-buffer))
                                                           :existing t)))
                                (#\B
                                 `(list (prompt-for-buffer ,(subseq arg-descriptor 1)
                                                           :default (buffer-name (other-buffer))
                                                           :existing nil)))
                                (#\f
                                 `(list (prompt-for-file
                                         ,(subseq arg-descriptor 1)
                                         :directory (buffer-directory)
                                         :default nil
                                         :existing t)))
                                (#\F
                                 `(list (prompt-for-file
                                         ,(subseq arg-descriptor 1)
                                         :directory (buffer-directory)
                                         :default nil
                                         :existing nil)))
                                (#\r
                                 (push '(check-marked) pre-forms)
                                 '(list
                                   (region-beginning)
                                   (region-end)))))
                             ((and (consp arg-descriptor)
                                   (eq :splice (first arg-descriptor)))
                              (assert (alexandria:length= arg-descriptor 2))
                              (second arg-descriptor))
                             (t
                              `(list ,arg-descriptor))))
                     arg-descriptors)))
      (if (null pre-forms)
          `(append ,@forms)
          `(progn
             ,@pre-forms
             (append ,@forms)))))

  (alexandria:with-unique-names (arguments)
    (defun gen-defcommand-body (fn-name
                                universal-argument
                                arg-descriptors)
      `(block ,fn-name
         (destructuring-bind (&rest ,arguments)
             ,(parse-arg-descriptors arg-descriptors universal-argument)
           (apply #',fn-name ,arguments))))))

(defun primary-class (options)
  (let ((value (alexandria:assoc-value options :primary-class)))
    (cond ((null value)
           +primary-command-class-name+)
          (t
           (assert (alexandria:length= value 1))
           (first value)))))

(defmacro define-command (&whole form name-and-options params (&rest arg-descriptors) &body body)
  (destructuring-bind (name . options) (uiop:ensure-list name-and-options)
    (let ((primary-class (primary-class options))
          (advice-classes (alexandria:assoc-value options :advice-classes))
          (command-name (alexandria:if-let (elt (assoc :name options))
                          (second elt)
                          (string-downcase name))))
      (alexandria:with-unique-names (command universal-argument)
        `(progn
           (add-command ,command-name
                        (make-cmd :name ',name
                                  :form ',form
                                  :source-location #+sbcl (sb-c:source-location) #-sbcl nil))
           (defun ,name ,params
             ;; コマンドではなく直接この関数を呼び出した場合
             ;; - *this-command*が束縛されない
             ;; - executeのフックが使えない
             ,@body)
           (defclass ,name (,primary-class ,@advice-classes) ())
           (defmethod execute ((,command ,name) ,universal-argument)
             (declare (ignorable ,universal-argument))
             ,(gen-defcommand-body name
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

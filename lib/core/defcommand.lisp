(in-package :lem)

(export '(define-command
          make-command-table
          add-command
          remove-command
          find-command
          exist-command-p))

(eval-when (:compile-toplevel :load-toplevel)
  (defun gen-defcommand-arg-parser (universal-argument arg-descripters)
    (cond
      ((string= "p" (car arg-descripters))
       `(list (or ,universal-argument 1)))
      ((string= "P" (car arg-descripters))
       `(list ,universal-argument))
      ((string= "r" (car arg-descripters))
       `(progn
          (check-marked)
          (list (region-beginning) (region-end))))
      (t
       (cons 'list
             (mapcar #'(lambda (arg-descripter)
                         (cond
                           ((char= #\s (aref arg-descripter 0))
                            `(prompt-for-string ,(subseq arg-descripter 1)))
                           ((char= #\n (aref arg-descripter 0))
                            `(prompt-for-integer ,(subseq arg-descripter 1)))
                           ((char= #\b (aref arg-descripter 0))
                            `(prompt-for-buffer ,(subseq arg-descripter 1)
                                                :default (buffer-name (current-buffer))
                                                :existing t))
                           ((char= #\B (aref arg-descripter 0))
                            `(prompt-for-buffer ,(subseq arg-descripter 1)
                                                :default (buffer-name (other-buffer))
                                                :existing nil))
                           ((char= #\f (aref arg-descripter 0))
                            `(prompt-for-file
                              ,(subseq arg-descripter 1)
                              :directory (buffer-directory)
                              :default nil
                              :existing t))
                           ((char= #\F (aref arg-descripter 0))
                            `(prompt-for-file
                              ,(subseq arg-descripter 1)
                              :directory (buffer-directory)
                              :default nil
                              :existing nil))
                           (t
                            (error "Illegal arg-descripter: ~a" arg-descripter))))
                     arg-descripters)))))

  (alexandria:with-unique-names (arguments)
    (defun gen-defcommand-body (fn-name
                                parms
                                universal-argument
                                arg-descripters)
      `(block ,fn-name
         ,(if (null arg-descripters)
              (progn
                (assert (null parms))
                `(,fn-name))
              `(destructuring-bind (&rest ,arguments)
                   ,(if (stringp (car arg-descripters))
                        (gen-defcommand-arg-parser universal-argument arg-descripters)
                        (car arg-descripters))
                 (apply #',fn-name ,arguments)))))))

(defun get-command (symbol)
  (alexandria:when-let (class (find-class symbol nil))
    (make-instance class)))

(defclass command () ())
(defgeneric execute (command argument))

(defmethod execute ((command symbol) argument)
  (alexandria:if-let (class (find-class command nil))
    (execute (make-instance class) argument)
    (editor-error "~A: command not found" command)))

(defmacro define-command (name params (&rest arg-descripters) &body body)
  (alexandria:with-unique-names (command universal-argument)
    (let ((command-name (string-downcase name)))
      `(progn
         (add-command ,command-name (make-cmd :name ',name))
         (defun ,name ,params
           ,@body)
         (defclass ,name (command) ())
         (defmethod execute ((,command ,name) ,universal-argument)
           (declare (ignorable ,universal-argument))
           ,(gen-defcommand-body name
                                 params
                                 universal-argument
                                 arg-descripters))))))

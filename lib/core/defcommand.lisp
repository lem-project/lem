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

(defvar *command-table*)

(defstruct cmd name)

(defstruct command-table
  (table (make-hash-table :test 'equal)))

(defun add-command (name cmd &optional (command-table *command-table*))
  (check-type name string)
  (setf (gethash name (command-table-table command-table)) cmd))

(defun remove-command (name &optional (command-table *command-table*))
  (remhash name (command-table-table command-table)))

(defun all-command-names (&optional (command-table *command-table*))
  (alexandria:hash-table-keys (command-table-table command-table)))

(defun find-command (command-name &optional (command-table *command-table*))
  (gethash command-name (command-table-table command-table)))

(defun exist-command-p (command-name &optional (command-table *command-table*))
  (not (null (find-command command-name command-table))))

(defun find-command-symbol (command-name &optional (command-table *command-table*))
  (cmd-name (gethash command-name (command-table-table command-table))))

(defun get-command (symbol)
  (get symbol 'command))

(defmacro define-command (name parms (&rest arg-descripters) &body body)
  (let ((gcmd (gensym (symbol-name name)))
        (command-name (string-downcase name)))
    (alexandria:with-unique-names (universal-argument)
      `(progn
         (setf (get ',name 'command) ',gcmd)
         (add-command ,command-name (make-cmd :name ',name))
         (defun ,name ,parms ,@body)
         (defun ,gcmd (,universal-argument)
           (declare (ignorable ,universal-argument))
           ,(gen-defcommand-body
             name
             parms
             universal-argument
             arg-descripters))
         ',name))))

(setf *command-table* (make-command-table))

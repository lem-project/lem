(in-package :lem)

(export '(define-command
          find-command
          exist-command-p))

(defvar *command-table* (make-hash-table :test 'equal))

(eval-when (:compile-toplevel :load-toplevel)
  (let ((garg (gensym "ARG"))
        (g-args (gensym)))
    (defun define-command-gen-args (name arg-descripters)
      (declare (ignorable name))
      (cond
        ((string= "p" (car arg-descripters))
         `(list (or ,garg 1)))
        ((string= "P" (car arg-descripters))
         `(list ,garg))
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
    (defun define-command-gen-cmd (cmd-name fn-name parms arg-descripters)
      `(defun ,cmd-name (,garg)
         (declare (ignorable ,garg))
         (block ,fn-name
         ,(if (null arg-descripters)
              (progn
                (assert (null parms))
                `(,fn-name))
              `(destructuring-bind (&rest ,g-args)
                   ,(if (stringp (car arg-descripters))
                        (define-command-gen-args cmd-name arg-descripters)
                        (car arg-descripters))
                 (apply #',fn-name ,g-args))))))))

(defun find-command (name)
  (car (gethash name *command-table*)))

(defun find-command-symbol (name)
  (cdr (gethash name *command-table*)))

(defun get-command (symbol)
  (get symbol 'command))

(defun command-name (command)
  (get command 'name))

(defmacro define-command (name parms (&rest arg-descripters) &body body)
  (let ((gcmd (gensym (symbol-name name)))
        (command-name (string-downcase name)))
    `(progn
       (setf (get ',name 'command) ',gcmd)
       (setf (get ',gcmd 'name) ,command-name)
       (setf (gethash ,command-name *command-table*) (cons ',gcmd ',name))
       (defun ,name ,parms ,@body)
       ,(define-command-gen-cmd gcmd name parms arg-descripters)
       ',name)))

(defun all-command-names ()
  (alexandria:hash-table-keys *command-table*))

(defun exist-command-p (str)
  (if (find-command str) t nil))

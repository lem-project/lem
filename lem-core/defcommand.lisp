(in-package :lem)

(export '(define-command
          exist-command-p))

(defvar *command-table* (make-hash-table :test 'equal))

(eval-when (:compile-toplevel :load-toplevel)
  (defun collect-variables (parameters)
    (let ((acc))
      (dolist (parm parameters)
        (cond ((consp parm)
               (when (symbolp (car parm))
                 (push (car parm) acc)))
              ((not (char= #\& (schar (symbol-name parm) 0)))
               (push parm acc))))
      (nreverse acc)))
  (let ((garg (gensym "ARG")))
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
                                                  (buffer-name (current-buffer))
                                                  t))
                             ((char= #\B (aref arg-descripter 0))
                              `(prompt-for-buffer ,(subseq arg-descripter 1)
                                                  (buffer-name (other-buffer))
                                                  nil))
                             ((char= #\f (aref arg-descripter 0))
                              `(prompt-for-file
                                ,(subseq arg-descripter 1)
                                (buffer-directory)
                                nil
                                t))
                             ((char= #\F (aref arg-descripter 0))
                              `(prompt-for-file
                                ,(subseq arg-descripter 1)
                                (buffer-directory)
                                nil
                                nil))
                             (t
                              (error "Illegal arg-descripter: ~a" arg-descripter))))
                       arg-descripters)))))
    (defun define-command-gen-cmd (cmd-name fn-name parms arg-descripters)
      `(defun ,cmd-name (,garg)
         (declare (ignorable ,garg))
         ,(if (null arg-descripters)
              (progn
                (assert (null parms))
                `(,fn-name))
              `(destructuring-bind ,parms
                   ,(if (stringp (car arg-descripters))
                        (define-command-gen-args cmd-name arg-descripters)
                        (car arg-descripters))
                 (,fn-name ,@(collect-variables parms))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun function-to-command-name (f)
    (check-type f symbol)
    (string-downcase f)))

(defun find-command (name)
  (gethash name *command-table*))

(defun function-to-command (f)
  (find-command (function-to-command-name f)))

(defun command-name (command)
  (get command 'name))

(defun string-to-command (str)
  (intern (string-upcase str) :lem))

(defmacro define-command (name parms (&rest arg-descripters) &body body)
  (let ((gcmd (gensym (symbol-name name))))
    `(progn
       (setf (get ',gcmd 'name) (string-downcase ',name))
       (setf (gethash ,(function-to-command-name name)
                      *command-table*)
             ',gcmd)
       (defun ,name ,parms ,@body)
       ,(define-command-gen-cmd gcmd name parms arg-descripters)
       ',name)))

(defun all-command-names ()
  (let ((names))
    (maphash (lambda (name cmd)
               (declare (ignore cmd))
               (push name names))
             *command-table*)
    (nreverse names)))

(defun exist-command-p (str)
  (if (find-command str) t nil))

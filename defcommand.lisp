;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(command-completion
          define-command
          exist-command-p
          execute-command
          apropos-command))

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
                            `(minibuf-read-string ,(subseq arg-descripter 1)))
                           ((char= #\n (aref arg-descripter 0))
                            `(minibuf-read-number ,(subseq arg-descripter 1)))
                           ((char= #\b (aref arg-descripter 0))
                            `(minibuf-read-buffer ,(subseq arg-descripter 1)
                                                  (buffer-name (current-buffer))
                                                  t))
                           ((char= #\B (aref arg-descripter 0))
                            `(minibuf-read-buffer ,(subseq arg-descripter 1)
                                                  (buffer-name (other-buffer))
                                                  nil))
                           ((char= #\f (aref arg-descripter 0))
                            `(minibuf-read-file
                              ,(subseq arg-descripter 1)
                              (buffer-directory)
                              nil
                              t))
                           ((char= #\F (aref arg-descripter 0))
                            `(minibuf-read-file
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

(defmacro define-command (name parms (&rest arg-descripters) &body body)
  (let ((gcmd (gensym (symbol-name name))))
    `(progn
       (setf (gethash ,(string-downcase (symbol-name name))
                      *command-table*)
             ',gcmd)
       (defun ,name ,parms ,@body)
       ,(define-command-gen-cmd gcmd name parms arg-descripters))))

(defun cmd-call (cmd arg)
  (cond ((fboundp cmd)
         (run-hooks 'pre-command-hook) ;!!!
         (prog1 (funcall (gethash (string-downcase (symbol-name cmd))
                                  *command-table*)
                         arg)
           (buffer-undo-boundary)       ;!!!
           (run-hooks 'post-command-hook) ;!!!
           ))
        (t
         (minibuf-print (format nil "undefined command: ~a" cmd))
         nil)))

(defun command-completion (str)
  (let ((names))
    (maphash #'(lambda (name cmd)
                 (declare (ignore cmd))
                 (push name names))
             *command-table*)
    (completion str (nreverse names))))

(defun exist-command-p (str)
  (if (gethash str *command-table*) t nil))

(define-key *global-keymap* (kbd "M-x") 'execute-command)
(define-command execute-command (name)
  ((list (minibuf-read-line
          "M-x "
          ""
          'command-completion
          'exist-command-p)))
  (let ((cmd (gethash name *command-table*)))
    (if cmd
        (funcall cmd *universal-argument*)
        (minibuf-print "invalid command"))))

(define-command apropos-command (str) ("sApropos: ")
  (info-popup (get-buffer-create "*Apropos*")
              #'(lambda (out)
                  (maphash #'(lambda (name cmd)
                               (declare (ignore cmd))
                               (when (search str name)
                                 (dolist (kbd (search-keybind-all name))
                                   (princ (format nil "~a~a~a~%"
                                                  name
                                                  #\tab
                                                  (kbd-to-string kbd))
                                          out))))
                           *command-table*))))

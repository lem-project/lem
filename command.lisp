(in-package :lem)

(defvar *command-names* nil)

(eval-when (:compile-toplevel :load-toplevel)
(let ((garg (gensym "ARG")))
  (defun define-command-gen-args (name arg-descripters)
    (cond
     ((string= "p" (car arg-descripters))
      `(list (or ,garg 1)))
     ((string= "P" (car arg-descripters))
      `(list ,garg))
     ((string= "r" (car arg-descripters))
      `(if (buffer-check-marked (window-buffer))
         (list (region-beginning) (region-end))
         (return-from ,name nil)))
     (t
      (cons 'list
        (mapcar (lambda (arg-descripter)
                  (cond
                   ((char= #\s (aref arg-descripter 0))
                    `(read-string ,(subseq arg-descripter 1)))
                   ((char= #\n (aref arg-descripter 0))
                    `(read-number ,(subseq arg-descripter 1)))
                   ((char= #\b (aref arg-descripter 0))
                    `(read-buffer ,(subseq arg-descripter 1)
                       (buffer-name (window-buffer))
                       t))
                   ((char= #\B (aref arg-descripter 0))
                    `(read-buffer ,(subseq arg-descripter 1)
                       (buffer-name *prev-buffer*)
                       nil))
                   ((char= #\f (aref arg-descripter 0))
                    `(read-file-name
                      ,(subseq arg-descripter 1)
                      (current-directory)
                      nil
                      t))
                   ((char= #\F (aref arg-descripter 0))
                    `(read-file-name
                      ,(subseq arg-descripter 1)
                      (current-directory)
                      nil
                      nil))
                   (t
                    (error "Illegal arg-descripter: ~a" arg-descripter))))
          arg-descripters)))))
  (defun define-command-gen-cmd (name parms arg-descripters body)
    `(defun ,name (,garg)
       (declare (ignorable ,garg))
       ,(if (null arg-descripters)
          (progn (assert (null parms))
            `(progn ,@body))
          `(destructuring-bind ,parms
             ,(if (stringp (car arg-descripters))
                (define-command-gen-args name arg-descripters)
                (car arg-descripters))
             ,@body))))))

(defmacro define-command (name parms (&rest arg-descripters) &body body)
  (let ((gcmd (gensym (symbol-name name))))
    `(progn
      (pushnew ,(string-downcase (symbol-name name)) *command-names*)
      (setf (get ',name 'command) ',gcmd)
      (defun ,name ,parms ,@body)
      ,(define-command-gen-cmd gcmd parms arg-descripters body))))

(defun cmd-call (cmd arg)
  (funcall (get cmd 'command) arg))

(defun command-completion (str)
  (completion str *command-names*))

(defun exist-command-p (str)
  (find str *command-names* :test 'equal))

(define-key *global-keymap* "M-x" 'execute-command)
(define-command execute-command (name)
  ((list (read-minibuffer
          "M-x "
          ""
          'command-completion
          'exist-command-p)))
  (cmd-call (intern (string-upcase name) :lem)
    *universal-argument*))

(define-command apropos (str) ("sApropos: ")
  (let* ((buffer (get-buffer-create "*Apropos*"))
         (out (make-buffer-output-stream buffer)))
    (popup buffer
           (lambda ()
             (dolist (name *command-names*)
               (when (search str name)
                 (let ((result (keybind-find-from-command name)))
                   (fresh-line out)
                   (princ (if result
                            (format nil "~a~a~a~a~a"
                                    name
                                    #\tab
                                    (car result)
                                    #\tab
                                    (cadr result))
                            name)
                          out))))))))

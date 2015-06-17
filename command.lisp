(in-package :lem)

(defvar *command-table* (make-hash-table))
(defvar *keybind-table* (make-hash-table :test 'equal))

(defun keys-to-keystr (keys)
  (apply 'concatenate 'string
    (mapcar (lambda (c)
              (cond
               ((key::ctrl-p c)
                (format nil "C-~c"
                  (char-downcase (code-char (+ 64 (char-code c))))))
               ((char= c key::escape)
                "M-")
               (t
                (string c))))
      keys)))

(defun find-command (keys)
  (let ((cmd (gethash (keys-to-keystr keys) *keybind-table*)))
    (when cmd
      (get cmd 'command))))

(defun define-key (keystr name)
  (setf (gethash keystr *keybind-table*) name))

(let ((garg (gensym "ARG")))
  (defun defcommand-gen-args (arg-descripter)
    (cond
     ((string= "p" arg-descripter)
      `(list (or ,garg 1)))
     ((string= "P" arg-descripter)
      `(list ,garg))
     ((string= "r" arg-descripter)
      `(list (region-beginning) (region-end)))
     (t
      (cons 'list
        (mapcar (lambda (arg-descripter)
                  (cond
                   ((char= #\s (aref arg-descripter 0))
                    `(mb-read-string ,(subseq arg-descripter 1)))
                   ((char= #\b (aref arg-descripter 0))
                    `(mb-read-buffer ,(subseq arg-descripter 1)
                       (buffer-name (window-buffer))
                       t))
                   ((char= #\B (aref arg-descripter 0))
                    `(mb-read-buffer ,(subseq arg-descripter 1)
                       (buffer-name *prev-buffer*)
                       nil))
                   ((char= #\f (aref arg-descripter 0))
                    `(mb-read-file-name
                      ,(subseq arg-descripter 1)
                      (current-directory)
                      nil
                      t))
                   ((char= #\F (aref arg-descripter 0))
                    `(mb-read-file-name
                      ,(subseq arg-descripter 1)
                      (current-directory)
                      nil
                      nil))
                   (t
                    (error "Illegal arg-descripter: ~a" arg-descripter))))
          (split-string arg-descripter #\newline))))))
  (defun defcommand-gen-cmd (name parms arg-descripter body)
    `(defun ,name (,garg)
       (declare (ignorable ,garg))
       ,(if (null arg-descripter)
          (progn (assert (null parms))
            `(progn ,@body))
          `(destructuring-bind ,parms
             ,(if (stringp arg-descripter)
                (defcommand-gen-args arg-descripter)
                arg-descripter)
             ,@body)))))

(defmacro defcommand (name parms (&optional arg-descripter) &body body)
  (let ((gcmd (gensym (symbol-name name))))
    `(progn
      (setf (get ',name 'command) ',gcmd)
      (defun ,name ,parms ,@body)
      ,(defcommand-gen-cmd gcmd parms arg-descripter body))))

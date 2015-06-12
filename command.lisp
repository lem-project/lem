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
  (gethash (keys-to-keystr keys) *keybind-table*))

(defun add-command (func cmd &optional keystr)
  (setf (gethash cmd *command-table*) func)
  (when keystr
    (setf (gethash keystr *keybind-table*) func)))

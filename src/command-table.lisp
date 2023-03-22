(in-package :lem)

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

(unless (boundp '*command-table*)
  (setf *command-table* (make-command-table)))

(in-package :lem)

(defvar *command-table*)

(defstruct command-table
  (table (make-hash-table :test 'equal)))

(defun add-command (name command &optional (command-table *command-table*))
  (check-type name string)
  (check-type command primary-command)
  (setf (gethash name (command-table-table command-table)) command))

(defun remove-command (name &optional (command-table *command-table*))
  (remhash name (command-table-table command-table)))

(defun all-command-names (&optional (command-table *command-table*))
  (alexandria:hash-table-keys (command-table-table command-table)))

(defun find-command (command-name &optional (command-table *command-table*))
  (gethash command-name (command-table-table command-table)))

(defun exist-command-p (command-name &optional (command-table *command-table*))
  (not (null (find-command command-name command-table))))

(unless (boundp '*command-table*)
  (setf *command-table* (make-command-table)))

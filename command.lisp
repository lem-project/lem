(in-package :lem)

(defvar *command-table* (make-hash-table))
(defvar *keybind-table* (make-hash-table :test 'equal))

(defun command-find-keybind (keys)
  (gethash keys *keybind-table*))

(defun add-command (func cmd &rest keys)
  (setf (gethash cmd *command-table*) func)
  (when keys
    (setf (gethash keys *keybind-table*) func)))

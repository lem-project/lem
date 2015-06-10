(in-package :lem)

(defvar *command-table* (make-hash-table))
(defvar *keybind-table* (make-hash-table :test 'equal))

(defun command-init ()
  (flet ((add (name fn &rest keys)
           (setf (gethash name *command-table*) fn)
	   (setf (gethash keys *keybind-table*) fn)))
    (add 'next-char #'buffer-next-char key::ctrl-f)
    (add 'prev-char #'buffer-prev-char key::ctrl-b)
    (add 'next-line #'buffer-next-line key::ctrl-n)
    (add 'prev-line #'buffer-prev-line key::ctrl-p)
    (add 'beginning-of-line #'buffer-bol key::ctrl-a)
    (add 'end-of-line #'buffer-eol key::ctrl-e)
    (add 'delete-char #'buffer-delete-char key::ctrl-d)
    (add 'newline #'buffer-insert-newline key::ctrl-j)
    (add 'exit-lem #'exit-lem key::ctrl-x key::ctrl-c)
    ))

(defun command-find-keybind (keys)
  (gethash keys *keybind-table*))


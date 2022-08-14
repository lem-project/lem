(in-package :lem)

(defvar *killring* (make-killring 10))

(defun current-killring ()
  *killring*)

(defun copy-to-clipboard-with-killring (string)
  (push-killring-item (current-killring) string)
  (when (enable-clipboard-p)
    (copy-to-clipboard string)))

(defun yank-from-clipboard-or-killring ()
  (or (and (enable-clipboard-p) (get-clipboard-data))
      (peek-killring-item (current-killring) 0)))

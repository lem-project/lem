(in-package :lem)

(defvar *killring* (make-killring 10))

(defun copy-to-clipboard-with-killring (string)
  (push-killring-item *killring* string)
  (when (enable-clipboard-p)
    (copy-to-clipboard string)))

(defun yank-from-clipboard-or-killring ()
  (or (and (enable-clipboard-p) (get-clipboard-data))
      (peek-killring-item *killring* 0)))

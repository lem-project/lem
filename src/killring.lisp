(in-package :lem)

(defvar *killring* (make-killring 10))

(defun kill-push (string)
  (push-killring-item *killring* string)
  (when (enable-clipboard-p)
    (copy-to-clipboard string)))

(defun current-kill-ring (&key (use-clipboard (enable-clipboard-p)))
  (or (and use-clipboard (get-clipboard-data))
      (peek-killring-item *killring* 0)))

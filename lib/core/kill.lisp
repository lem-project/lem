(in-package :lem)

(export '(with-killring
          kill-push
          kill-ring-rotate
          kill-ring-rotate-undo
          kill-ring-new))

(defvar *kill-before-p* nil)
(defvar *kill-options* '())

(defvar *killring* (make-killring 10))

(defmacro with-killring ((&key options before) &body body)
  `(let ((*kill-options* ,options)
         (*kill-before-p* ,before))
     ,@body))

(defun kill-push (string)
  (let ((element (killring-add *killring*
                               (make-killring-element string *kill-options*)
                               *kill-before-p*)))
    (when (enable-clipboard-p)
      (copy-to-clipboard (killring-element-string element)))
    element))

(defun current-kill-ring ()
  (or (and (enable-clipboard-p)
           (get-clipboard-data))
      (killring-nth *killring* 0)))

(defun kill-ring-nth (n)
  (check-type n (integer 0 *))
  (killring-nth *killring* n))

(defun kill-ring-rotate ()
  (killring-rotate *killring*))

(defun kill-ring-rotate-undo ()
  (killring-rotate-undo *killring*))

(defun kill-ring-new ()
  (killring-new *killring*))

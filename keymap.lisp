(in-package :lem)

(defvar *keymaps* nil)

(defstruct (keymap (:constructor make-keymap-internal))
  undef-hook
  alist)

(defun make-keymap (&optional undef-hook)
  (let ((keymap (make-keymap-internal :undef-hook undef-hook)))
    (push keymap *keymaps*)
    keymap))

(defvar *global-keymap*
  (make-keymap 'undefined-key))

(defun define-key (keymap kbd cmd-name)
  (push (cons kbd cmd-name)
    (keymap-alist keymap)))

(defvar *current-keymap* *global-keymap*)

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

(defun keymap-find-command (keymap keys)
  (let ((cmd (cdr
              (assoc (keys-to-keystr keys)
                (keymap-alist keymap)
                :test 'equal))))
    cmd))

(defun key-undef-hook (keymap keys)
  (when (keymap-undef-hook keymap)
    (funcall (keymap-undef-hook keymap) keys)))

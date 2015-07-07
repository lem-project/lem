(in-package :lem)

(export '(*keymaps*
          keymap
          keymap-name
          keymap-undef-hook
          keymap-parent
          keymap-table
          make-keymap
          *global-keymap*
          define-key))

(defvar *keymaps* nil)

(defstruct (keymap (:constructor make-keymap-internal))
  name
  undef-hook
  parent
  table)

(defun make-keymap (name &optional undef-hook parent)
  (let ((keymap (make-keymap-internal
                 :name name
                 :undef-hook undef-hook
                 :parent parent
                 :table (make-hash-table :test 'equal))))
    (push keymap *keymaps*)
    keymap))

(defvar *global-keymap* (make-keymap "global" 'undefined-key))

(defun define-key (keymap kbd cmd-name)
  (setf (gethash kbd (keymap-table keymap))
        cmd-name))

(defun kbd-to-string (key)
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
      key)))

(defun keymap-find-command (keymap key)
  (let ((cmd (gethash (kbd-to-string key)
                       (keymap-table keymap))))
    (or cmd
      (let ((keymap (keymap-parent keymap)))
        (when keymap
          (keymap-find-command keymap key))))))

(defun keybind-find-from-command (name)
  (let ((name (intern (string-upcase name) :lem)))
    (dolist (keymap *keymaps*)
      (maphash (lambda (key val)
                 (when (eq name val)
                   (return-from keybind-find-from-command
                     (list key (keymap-name keymap)))))
               (keymap-table keymap)))))

(defun key-undef-hook (keymap key)
  (when (keymap-undef-hook keymap)
    (funcall (keymap-undef-hook keymap) key)))

(defun insertion-key-p (key)
  (when (or (< 31 (char-code (car key)))
            (char= key::ctrl-i (car key)))
    (car key)))

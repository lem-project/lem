(in-package :lem)

(export '(*keymaps*
          keymap
          keymap-name
          keymap-undef-hook
          keymap-parent
          keymap-table
          make-keymap
          *global-keymap*
          define-key
          kbd-to-string
          kbd))

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

(defun define-key (keymap key cmd-name)
  (when (typep key 'kbd)
    (setq key (slot-value key 'list)))
  (unless (and (listp key)
               (dolist (k key t)
                 (unless (characterp k)
                   (return nil))))
    (error "define-key: ~s is illegal key" key))
  (setf (gethash key (keymap-table keymap))
        cmd-name))

(defun kbd-to-string (key)
  (apply 'concatenate 'string
         (mapcar (lambda (c)
                   (cond
                     ((key::ctrl-p c)
                      (format nil "C-~c"
                              (char-downcase
                               (code-char
                                (+ 64 (char-code c))))))
                     ((char= c key::escape)
                      "M-")
                     (t
                      (string c))))
                 key)))

(defclass kbd ()
  ((list :initarg :list)))

(defmethod print-object ((k kbd) stream)
  (format stream "(~A ~S)" 'kbd (kbd-to-string (slot-value k 'list))))

(defun kbd (str)
  (make-instance
   'kbd
   :list (let ((key))
           (do ((i 0 (1+ i)))
               ((>= i (length str)))
             (case (aref str i)
               (#\C
                (assert (char= #\- (aref str (incf i))))
                (push (code-char
                       (- (char-code
                           (char-upcase
                            (aref str (incf i))))
                          64))
                      key))
               (#\M
                (assert (char= #\- (aref str (incf i))))
                (push key::escape key))
               (t
                (push (aref str i) key))))
           (nreverse key))))

(defun keymap-find-command (keymap key)
  (when (typep key 'kbd)
    (setq key (slot-value key 'list)))
  (let ((cmd (gethash key (keymap-table keymap))))
    (or cmd
        (let ((keymap (keymap-parent keymap)))
          (when keymap
            (keymap-find-command keymap key))))))

(defun search-keybind-all (name)
  (let ((name (intern (string-upcase name) :lem))
        (acc))
    (dolist (keymap *keymaps*)
      (maphash (lambda (key val)
                 (when (eq name val)
                   (push (list key (keymap-name keymap)) acc)))
               (keymap-table keymap)))
    acc))

(defun key-undef-hook (keymap key)
  (when (typep key 'kbd)
    (setq key (slot-value key 'list)))
  (when (keymap-undef-hook keymap)
    (funcall (keymap-undef-hook keymap) key)))

(defun insertion-key-p (key)
  (when (typep key 'kbd)
    (setq key (slot-value key 'list)))
  (when (or (< 31 (char-code (car key)))
            (char= key::ctrl-i (car key)))
    (car key)))

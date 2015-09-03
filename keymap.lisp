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
               (every #'characterp key))
    (error "define-key: ~s is illegal key" key))
  (setf (gethash key (keymap-table keymap))
        cmd-name))

(defun kbd-to-string (key)
  (format nil "~{~A~^~}"
          (loop for c- on key
             for c = (first c-)
             collect (cond
                       ((ctrl-p c)
                        (format nil "C-~c"
                                (char-downcase
                                 (code-char
                                  (+ 64 (char-code c))))))
                       ((char= c escape) "M")
                       ((gethash c *key->symbol*))
                       (t (format nil "~A" c)))
             collect (cond ((not (cdr c-))"")
                           ((char= c escape) "-")
                           (t " ")))))

(defclass kbd ()
  ((list :initarg :list)))

(defmethod print-object ((k kbd) stream)
  (format stream "(~A ~S)" 'kbd (kbd-to-string (slot-value k 'list))))

(defun kbd-string (str)
  (make-instance
   'kbd
   :list (let (result)
           (labels ((f (str)
                      (if (and (>= (length str) 2)
                               (eql (aref str 0) #\M)
                               (eql (aref str 1) #\-)
                               (push escape result))
                          (f (subseq str 2))
                          (push (gethash str *string->key*) result))))
             (loop with beg = 0
                for i from 0
                for c across str
                when (eql c #\space)
                do (f (subseq str beg i))
                  (setq beg (1+ i))
                finally (f (subseq str beg i))))
           (nreverse result))))

(defun kbd-keys (keys)
  (make-instance 'kbd :list keys))

(defun kbd (string-or-first-key &rest keys)
  (etypecase string-or-first-key
    (string
     (kbd-string string-or-first-key))
    (character
     (kbd-keys (cons string-or-first-key keys)))))

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
      (maphash #'(lambda (key val)
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
            (char= C-i (car key)))
    (car key)))

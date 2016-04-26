(in-package :lem)

(export '(*keymaps*
          keymap
          keymap-undef-hook
          keymap-parent
          keymap-table
          make-keymap
          *global-keymap*
          kbd-p
          define-key
          kbd-to-string
          kbd
          keymap-find-keybind
          search-keybind-all
          insertion-key-p))

(defvar *keymaps* nil)

(defstruct (keymap (:constructor %make-keymap))
  undef-hook
  parent
  table)

(defun make-keymap (&optional undef-hook parent)
  (let ((keymap (%make-keymap
                 :undef-hook undef-hook
                 :parent parent
                 :table (make-hash-table :test 'equal))))
    (push keymap *keymaps*)
    keymap))

(defvar *global-keymap* (make-keymap 'self-insert))

(defclass kbd ()
  ((list :initarg :list
         :reader kbd-list)))

(defvar *kbd-cache-table* (make-hash-table :test 'equal))

(defun make-kbd (list)
  (or (gethash list *kbd-cache-table*)
      (setf (gethash list *kbd-cache-table*)
            (make-instance 'kbd :list list))))

(defmethod print-object ((k kbd) stream)
  (format stream "(~A ~S)" 'kbd (kbd-to-string k)))

(defun kbd-p (x)
  (typep x 'kbd))

(defun define-key (keymap key cmd-name)
  (let ((kbd (typecase key
               (list (apply #'kbd key))
               (string (kbd key))
               (t key))))
    (unless (and (kbd-p kbd)
                 (every #'characterp (kbd-list kbd)))
      (error "define-key: ~s is illegal key" key))
    (setf (gethash kbd (keymap-table keymap))
          cmd-name)))

(defun kbd-to-string (key)
  (format nil "~{~A~^~}"
          (loop for c- on (kbd-list key)
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

(defun kbd-string-1 (str)
  (if (and (>= (length str) 2)
           (eql (aref str 0) #\M)
           (eql (aref str 1) #\-))
      (cons escape (kbd-string-1 (subseq str 2)))
      (list (gethash str *string->key*))))

(defun kbd-string (str)
  (make-kbd
   (mapcan #'(lambda (str)
               (if (and (< 4 (length str))
                        (string= str "C-M-" :end1 4))
                   (kbd-string-1 (concatenate 'string
                                              "M-C-" (subseq str 4)))
                   (kbd-string-1 str)))
           (split-string str #\space))))

(defun kbd-keys (keys)
  (make-kbd keys))

(defun kbd (string-or-first-key &rest keys)
  (etypecase string-or-first-key
    (string
     (kbd-string string-or-first-key))
    (character
     (kbd-keys (cons string-or-first-key keys)))))

(defun keymap-find-keybind (keymap key)
  (let ((cmd (gethash key (keymap-table keymap))))
    (or cmd
        (let ((keymap (keymap-parent keymap)))
          (when keymap
            (keymap-find-keybind keymap key)))
        (keymap-undef-hook keymap))))

(defun search-keybind-all (name)
  (let ((name (intern (string-upcase name) :lem))
        (keys))
    (dolist (keymap *keymaps*)
      (maphash #'(lambda (key val)
                   (when (eq name val)
                     (push key keys)))
               (keymap-table keymap)))
    keys))

(defun key-undef-hook (keymap key)
  (when (keymap-undef-hook keymap)
    (funcall (keymap-undef-hook keymap) key)))

(defun insertion-key-p (key)
  (let ((first-key (car (kbd-list key))))
    (when (or (< 31 (char-code first-key))
              (char= C-i first-key))
      first-key)))

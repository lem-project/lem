(in-package :lem-core)

(defvar *keymaps* nil)

(defvar *special-keymap* nil)

(deftype key-sequence ()
  '(trivial-types:proper-list key))

(defun keyseq-to-string (key-sequence)
  (check-type key-sequence key-sequence)
  (format nil "~{~A~^ ~}" key-sequence))

(defstruct (keymap (:constructor %make-keymap))
  undef-hook
  parent
  table
  function-table
  name)

(defmethod print-object ((object keymap) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (when (keymap-name object)
      (princ (keymap-name object) stream))))

(defun make-keymap (&key undef-hook parent name)
  (let ((keymap (%make-keymap
                 :undef-hook undef-hook
                 :parent parent
                 :table (make-hash-table :test 'eq)
                 :function-table (make-hash-table :test 'eq)
                 :name name)))
    (push keymap *keymaps*)
    keymap))

(defun prefix-command-p (command)
  (hash-table-p command))

(defun define-key (keymap keyspec command-name)
  (check-type keyspec (or symbol string))
  (check-type command-name symbol)
  (typecase keyspec
    (symbol
     (setf (gethash keyspec (keymap-function-table keymap))
           command-name))
    (string
     (let ((keys (parse-keyspec keyspec)))
       (define-key-internal keymap keys command-name))))
  (values))

(defun define-key-internal (keymap keys symbol)
  (loop :with table := (keymap-table keymap)
        :for rest :on (uiop:ensure-list keys)
        :for k := (car rest)
        :do (cond ((null (cdr rest))
                   (setf (gethash k table) symbol))
                  (t
                   (let ((next (gethash k table)))
                     (if (and next (prefix-command-p next))
                         (setf table next)
                         (let ((new-table (make-hash-table :test 'eq)))
                           (setf (gethash k table) new-table)
                           (setf table new-table))))))))

(defun parse-keyspec (string)
  (labels ((fail ()
             (editor-error "parse error: ~A" string))
           (parse (str)
             (loop :with ctrl :and meta :and super :and hypher :and shift
                   :do (cond
                         ((ppcre:scan "^[cmshCMSH]-" str)
                          (ecase (char-downcase (char str 0))
                            ((#\c) (setf ctrl t))
                            ((#\m) (setf meta t))
                            ((#\s) (setf super t))
                            ((#\h) (setf hypher t)))
                          (setf str (subseq str 2)))
                         ((ppcre:scan "^[sS]hift-" str)
                          (setf shift t)
                          (setf str (subseq str 6)))
                         ((string= str "")
                          (fail))
                         ((and (not (insertion-key-sym-p str))
                               (not (named-key-sym-p str)))
                          (fail))
                         (t
                          (cond ((and ctrl (string= str "i"))
                                 (setf ctrl nil
                                       str "Tab"))
                                ((and ctrl (string= str "m"))
                                 (setf ctrl nil
                                       str "Return")))
                          (return (make-key :ctrl ctrl
                                            :meta meta
                                            :super super
                                            :hypher hypher
                                            :shift shift
                                            :sym (or (named-key-sym-p str)
                                                     str))))))))
    (mapcar #'parse (uiop:split-string string :separator " "))))

(defun traverse-keymap (keymap fun)
  (labels ((f (table prefix)
             (maphash (lambda (k v)
                        (if (prefix-command-p v)
                            (f v (cons k prefix))
                            (funcall fun (reverse (cons k prefix)) v)))
                      table)))
    (f (keymap-table keymap) nil)))

(defun keymap-find-keybind (keymap key cmd)
  (let ((table (keymap-table keymap)))
    (labels ((f (k)
               (let ((cmd (gethash k table)))
                 (if (prefix-command-p cmd)
                     (setf table cmd)
                     cmd))))
      (let ((parent (keymap-parent keymap)))
        (when parent
          (setf cmd (keymap-find-keybind parent key cmd))))
      (or (etypecase key
            (key
             (f key))
            (list
             (let (cmd)
               (dolist (k key)
                 (unless (setf cmd (f k))
                   (return)))
               cmd)))
          (gethash cmd (keymap-function-table keymap))
          (keymap-undef-hook keymap)
          cmd))))

(defun insertion-key-p (key)
  (let* ((key (typecase key
                (list (first key))
                (otherwise key)))
         (sym (key-sym key)))
    (cond ((match-key key :sym "Return") #\Return)
          ((match-key key :sym "Tab") #\Tab)
          ((match-key key :sym "Space") #\Space)
          ((and (insertion-key-sym-p sym)
                (match-key key :sym sym))
           (char sym 0)))))

(defun all-keymaps ()
  (let ((keymaps
          (loop :for mode :in (all-active-modes (current-buffer))
                :when (mode-keymap mode)
                :collect :it)))
    (when *special-keymap*
      (push *special-keymap* keymaps))
    (nreverse keymaps)))

(defun lookup-keybind (key)
  (let (cmd)
    (loop :with buffer := (current-buffer)
          :for keymap :in (all-keymaps)
          :do (setf cmd (keymap-find-keybind keymap key cmd)))
    cmd))

(defun find-keybind (key)
  (let ((cmd (lookup-keybind key)))
    (when (symbolp cmd)
      cmd)))

(defun collect-command-keybindings (command keymap)
  (let ((bindings '()))
    (traverse-keymap keymap
                     (lambda (kseq cmd)
                       (when (eq cmd command)
                         (push kseq bindings))))
    (nreverse bindings)))

(defvar *abort-key*)

(defun abort-key-p (key)
  (and (key-p key)
       (eq *abort-key* (lookup-keybind key))))

(defmacro with-special-keymap ((keymap) &body body)
  `(let ((*special-keymap* (or ,keymap *special-keymap*)))
     ,@body))

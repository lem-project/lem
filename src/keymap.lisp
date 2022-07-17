(in-package :lem)

(defvar *keymaps* nil)

(defstruct (keymap (:constructor %make-keymap) (:print-function %print-keymap))
  undef-hook
  parent
  table
  function-table
  name)

(defun %print-keymap (object stream depth)
  (declare (ignorable depth))
  (print-unreadable-object (object stream :identity t :type t)
    (when (keymap-name object)
      (format stream "~A" (keymap-name object)))))

(defun make-keymap (&key undef-hook parent name)
  (let ((keymap (%make-keymap
                 :undef-hook undef-hook
                 :parent parent
                 :table (make-hash-table :test 'eq)
                 :function-table (make-hash-table :test 'eq)
                 :name name)))
    (push keymap *keymaps*)
    keymap))

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
                     (if (and next (hash-table-p next))
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

(defun keyseq-to-string (kseq)
  (with-output-to-string (out)
    (loop :for key* :on kseq
          :for key := (first key*)
          :do (princ key out)
              (when (rest key*)
                (write-char #\space out)))))

(defun keymap-find-keybind (keymap key cmd)
  (let ((table (keymap-table keymap)))
    (labels ((f (k)
               (let ((cmd (gethash k table)))
                 (if (hash-table-p cmd)
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

(defun keymap-flatten-map (keymap fun)
  (labels ((f (table prefix)
             (maphash (lambda (k v)
                        (if (hash-table-p v)
                            (f v (cons k prefix))
                            (funcall fun (reverse (cons k prefix)) v)))
                      table)))
    (f (keymap-table keymap) nil)))

(defun lookup-keybind (key)
  (let (cmd)
    (loop with buffer = (current-buffer)
          for mode in (nreverse (all-active-modes (current-buffer)))
          do (when (mode-keymap mode)
               (setf cmd (keymap-find-keybind (mode-keymap mode) key cmd))))
    cmd))

(defun find-keybind (key)
  (let ((cmd (lookup-keybind key)))
    (when (symbolp cmd)
      cmd)))

(defun abort-key-p (key)
  (and (key-p key)
       (eq 'keyboard-quit (lookup-keybind key))))

(in-package :lem)

(export '(*keymaps*
          keymap
          make-keymap
          *global-keymap*
          undefined-key
          define-key
          keyseq-to-string
          find-keybind
          insertion-key-p
          lookup-keybind))

(defvar *keymaps* nil)

(defstruct (keymap (:constructor %make-keymap) (:print-function %print-keymap))
  undef-hook
  insertion-hook
  parent
  table
  name)

(defun %print-keymap (object stream depth)
  (declare (ignorable depth))
  (print-unreadable-object (object stream :identity t :type t)
    (when (keymap-name object)
      (format stream "~A" (keymap-name object)))))

(defun make-keymap (&key undef-hook insertion-hook parent name)
  (let ((keymap (%make-keymap
                 :undef-hook undef-hook
                 :insertion-hook insertion-hook
                 :parent parent
                 :table (make-hash-table :test 'equalp)
                 :name name)))
    (push keymap *keymaps*)
    keymap))

(defun define-key (keymap keyspec symbol)
  (check-type symbol symbol)
  (let ((keys (typecase keyspec
                (symbol (loop :for (key1 . keymap1) :in (get keyspec 'keybind)
                              :do (define-key keymap key1 symbol))
                        (return-from define-key))
                (string (parse-keyspec keyspec)))))
    (pushnew (cons keyspec keymap) (get symbol 'keybind) :test 'equal)
    (define-key-internal keymap keys symbol)))

(defun define-key-internal (keymap keys symbol)
  (loop :with table := (keymap-table keymap)
        :for rest :on (uiop:ensure-list keys)
        :for k := (car rest)
        :do (cond ((null (cdr rest))
                   (setf (gethash k table) symbol))
                  (t
                   (let ((next (gethash k table)))
                     (if next
                         (setf table next)
                         (let ((new-table (make-hash-table :test 'equalp)))
                           (setf (gethash k table) new-table)
                           (setf table new-table))))))))


(defvar *key-sym-table* (make-hash-table :test 'equal))

(defun set-key-sym (name &optional (value name))
  (setf (gethash name *key-sym-table*) value))

(set-key-sym "Tab")
(set-key-sym "Return")
(set-key-sym "Escape")
(set-key-sym "Delete")
(set-key-sym "Down")
(set-key-sym "Up")
(set-key-sym "Left")
(set-key-sym "Right")
(set-key-sym "Backspace")
(set-key-sym "F0")
(set-key-sym "F1")
(set-key-sym "F2")
(set-key-sym "F3")
(set-key-sym "F4")
(set-key-sym "F5")
(set-key-sym "F6")
(set-key-sym "F7")
(set-key-sym "F8")
(set-key-sym "F9")
(set-key-sym "F10")
(set-key-sym "F11")
(set-key-sym "F12")
(set-key-sym "Home")
(set-key-sym "PageDown")
(set-key-sym "PageUp")
(set-key-sym "End")
(set-key-sym "Space" " ")

(defun parse-keyspec (string)
  (labels ((fail ()
             (editor-error "parse error: ~A" string))
           (parse (str)
             (loop :with ctrl :and meta :and super :and hypher
                   :do (cond
                         ((ppcre:scan "^[cmshCMSH]-" str)
                          (ecase (char-downcase (char str 0))
                            ((#\c) (setf ctrl t))
                            ((#\m) (setf meta t))
                            ((#\s) (setf super t))
                            ((#\h) (setf hypher t)))
                          (setf str (subseq str 2)))
                         ((string= str "")
                          (fail))
                         ((and (< 1 (length str))
                               (not (gethash str *key-sym-table*)))
                          (fail))
                         (t
                          (return (make-key :ctrl ctrl
                                            :meta meta
                                            :super super
                                            :hypher hypher
                                            :sym (or (gethash str *key-sym-table*)
                                                     str))))))))
    (mapcar #'parse (uiop:split-string string :separator " "))))

(defun keyseq-to-string (kseq)
  (with-output-to-string (out)
    (loop :for key* :on kseq
          :for key := (first key*)
          :do (with-slots (ctrl meta super hypher sym) key
                (when hypher (write-string "H-" out))
                (when super (write-string "S-" out))
                (when meta (write-string "M-" out))
                (when ctrl (write-string "C-" out))
                (write-string sym out)
                (when (rest key*)
                  (write-char #\space out))))))

(defun keymap-find-keybind (keymap key)
  (let ((table (keymap-table keymap)))
    (labels ((f (k)
               (let ((cmd (gethash k table)))
                 (if (hash-table-p cmd)
                     (setf table cmd)
                     cmd))))
      (or (etypecase key
            (key
             (f key))
            (list
             (let (cmd)
               (dolist (k key)
                 (unless (setf cmd (f k))
                   (return)))
               cmd)))
          (let ((parent (keymap-parent keymap)))
            (when parent
              (keymap-find-keybind parent key)))
          (and (keymap-insertion-hook keymap)
               (insertion-key-p key)
               (keymap-insertion-hook keymap))
          (keymap-undef-hook keymap)))))

(defun insertion-key-p (key)
  (let ((sym
          (key-sym (typecase key
                     (list (first key))
                     (otherwise key)))))
    (when (= 1 (length sym))
      (let ((c (char sym 0)))
        (when (or (< 31 (char-code c))
                  (char= c #\tab))
          c)))))

(defun keymap-flatten-map (keymap fun)
  (labels ((f (table prefix)
             (maphash (lambda (k v)
                        (if (hash-table-p v)
                            (f v (cons k prefix))
                            (funcall fun (reverse (cons k prefix)) v)))
                      table)))
    (f (keymap-table keymap) nil)))

(defvar *global-keymap* (make-keymap :name '*global-keymap*
                                     :undef-hook 'self-insert))

(define-command undefined-key () ()
  (editor-error "Key not found: ~A"
                (keyseq-to-string (last-read-key-sequence))))

(defun lookup-keybind (key)
  (flet ((f (list)
           (some (lambda (mode)
                   (when (mode-keymap mode)
                     (keymap-find-keybind (mode-keymap mode) key)))
                 list)))
    (let ((buffer (current-buffer)))
      (or (f (buffer-minor-modes buffer))
          (f *global-minor-mode-list*)
          (keymap-find-keybind (mode-keymap (buffer-major-mode buffer)) key)
          (keymap-find-keybind *global-keymap* key)))))

(defun find-keybind (key)
  (let ((cmd (lookup-keybind key)))
    (when (symbolp cmd)
      (get-command cmd))))

(defun abort-key-p (key)
  (and (or (characterp key) (key-p key))
       (eq 'keyboard-quit (lookup-keybind key))))

(in-package :lem-lisp-mode)

(defun request-walk (string &optional package)
  (lisp-eval-from-string
   (format nil "(cl:ignore-errors (swank/lem-extras:walk ~S))" string)
   (or package (current-package))))

(define-major-mode lisp-definitions-mode lem.menu-mode:menu-mode
    (:name "lisp-definitions"
     :keymap *lisp-definitions-mode-keymap*))

(define-key *lisp-definitions-mode-keymap* "." 'lisp-definitions-show-source)
(define-key *lisp-definitions-mode-keymap* "C-c C-c" 'lisp-definitions-compile-defun)

(defun lisp-definitions-buffer ()
  (get-buffer "*lisp-definitions*"))

(defun lisp-definitions-cache-table ()
  (alexandria:when-let ((buffer (lisp-definitions-buffer)))
    (buffer-value buffer 'cache-table)))

(defun call-with-definitions-cache-table (src-buffer function)
  (let ((defs-buffer (lisp-definitions-buffer)))
    (when (and defs-buffer
               (equal (buffer-name src-buffer) (buffer-value defs-buffer 'buffer-name)))
      (alexandria:when-let ((cache-table (lisp-definitions-cache-table)))
        (funcall function cache-table)))))

(defun cache-key (def)
  (destructuring-bind (type name name-id form pos) def
    (declare (ignore form pos))
    (list type name name-id)))

(defun lisp-definitions-load-file-hook (filename)
  (call-with-definitions-cache-table
   (find filename (buffer-list) :key #'buffer-filename)
   (lambda (cache-table)
     (clrhash cache-table))))

(defun lisp-definitions-before-compile-hook (start end)
  (let ((text (points-to-string start end))
        (package (buffer-package (point-buffer start))))
    (call-with-definitions-cache-table
     (point-buffer start)
     (lambda (cache-table)
       (loop :for def :in (request-walk text package)
             :for (type name name-id form pos) := def
             :do (setf (gethash (cache-key def) cache-table) form))))))

(define-command lisp-definitions-list () ()
  (require-swank-extras)
  (let ((filename
          (merge-pathnames "swank-extras.lisp"
                           (asdf:system-source-directory :lem-lisp-mode))))
    (lisp-eval `(swank:load-file ,filename)))
  (let ((srcbuffer (current-buffer)))
    (add-hook (variable-value 'load-file-functions :buffer srcbuffer)
              'lisp-definitions-load-file-hook)
    (add-hook (variable-value 'before-compile-functions :buffer srcbuffer)
              'lisp-definitions-before-compile-hook)
    (let ((cache-table
            (let ((buffer (lisp-definitions-buffer)))
              (or (and buffer
                       (buffer-value buffer 'cache-table))
                  (make-hash-table :test 'equal))))
          (definitions (request-walk (points-to-string (buffer-start-point srcbuffer)
                                                       (buffer-end-point srcbuffer))
                                     (buffer-package srcbuffer))))
      (let ((menu (make-instance 'lem.menu-mode:menu
                                 :buffer-name "*lisp-definitions*"
                                 :columns '("MOD" "Name  " ""))))
        (dolist (def definitions)
          (destructuring-bind (type name _name-id form pos) def
            (declare (ignore _name-id))
            (let* ((item (make-instance 'lem.menu-mode:menu-item
                                        :plist (list 'pos pos)))
                   (changed (multiple-value-bind (old win)
                                (gethash (cache-key def) cache-table)
                              (when win
                                (not (equal old form))))))
              (lem.menu-mode:append-menu-item item (if changed " * " ""))
              (lem.menu-mode:append-menu-item item name)
              (lem.menu-mode:append-menu-item item type)
              (lem.menu-mode:append-menu menu item)
              (unless changed
                (setf (gethash (cache-key def) cache-table) form)))))
        (lem.menu-mode:display-menu menu 'lisp-definitions-mode)
        (let ((buffer (lisp-definitions-buffer)))
          (setf (buffer-value buffer 'buffer-name) (buffer-name srcbuffer))
          (setf (buffer-value buffer 'cache-table) cache-table))))))

(define-command lisp-definitions-show-source () ()
  (when (eq (current-buffer) (lisp-definitions-buffer))
    (alexandria:when-let ((buffer (get-buffer (buffer-value (current-buffer) 'buffer-name))))
      (let ((pos (lem.menu-mode:menu-property-at (current-point) 'pos)))
        (setf (current-window) (pop-to-buffer buffer))
        (and (move-to-position (current-point) pos)
             (form-offset (current-point) -1))))))

(define-command lisp-definitions-compile-defun () ()
  (when (eq (current-buffer) (lisp-definitions-buffer))
    (with-current-window (current-window)
      (when (lisp-definitions-show-source)
        (lisp-compile-defun)))))

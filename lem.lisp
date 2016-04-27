(in-package :lem)

(export '(find-keybind
          describe-key
          describe-bindings
          universal-argument
          self-insert
          lem))

(defvar *running-p* nil)

(define-key *global-keymap* (kbd "C-g") 'keyboard-quit)
(define-command keyboard-quit () ()
  (setq *macro-recording-p* nil)
  (buffer-mark-cancel (current-buffer))
  (message "Quit"))

(defun find-keybind (key)
  (let ((cmd (or (some #'(lambda (mode)
                           (keymap-find-keybind (mode-keymap mode) key))
                       (buffer-minor-modes))
                 (keymap-find-keybind (mode-keymap (buffer-major-mode)) key)
                 (keymap-find-keybind *global-keymap* key))))
    (function-to-command cmd)))

(define-key *global-keymap* (kbd "C-x ?") 'describe-key)
(define-command describe-key () ()
  (message "describe-key: ")
  (redraw-display)
  (let* ((key (read-key-sequence))
         (cmd (find-keybind key)))
    (message "describe-key: ~a ~a"
             (kbd-to-string key)
             (command-name cmd))))

(defun describe-bindings-internal (s name keymap &optional first-p)
  (unless first-p
    (princ C-L s)
    (terpri s))
  (let ((column-width 16))
    (princ name s)
    (terpri s)
    (format s "~va~a~%" column-width "key" "binding")
    (format s "~va~a~%" column-width "---" "-------")
    (maphash #'(lambda (k v)
                 (format s "~va~a~%"
                         column-width
                         (kbd-to-string k)
                         (symbol-name v)))
             (keymap-table keymap))
    (terpri s)))

(define-command describe-bindings () ()
  (info-popup (get-buffer-create "*bindings*")
              #'(lambda (s)
                  (describe-bindings-internal s
                                              "Major Mode Bindings"
                                              (mode-keymap (major-mode))
                                              t)
                  (describe-bindings-internal s
                                              "Global Bindings"
                                              *global-keymap*)
                  (dolist (mode (buffer-minor-modes))
                    (describe-bindings-internal s
                                                (mode-name mode)
                                                (mode-keymap mode))))))

(define-key *global-keymap* (kbd "C-u") 'universal-argument)
(define-command universal-argument () ()
  (let ((numlist)
        n)
    (do ((c (minibuf-read-char "C-u 4")
            (minibuf-read-char
             (format nil "C-u ~{~a~}" numlist))))
        (nil)
      (cond
        ((char= c C-u)
         (setq numlist
               (mapcar 'digit-char-p
                       (coerce
                        (format nil "~a"
                                (* 4
                                   (if numlist
                                       (parse-integer
                                        (format nil "~{~a~}" numlist))
                                       4)))
                        'list))))
        ((and (char= c #\-) (null numlist))
         (setq numlist (append numlist (list #\-))))
        ((setq n (digit-char-p c))
         (setq numlist
               (append numlist (list n))))
        (t
         (unread-key c)
         (let ((arg (if numlist
                        (parse-integer (format nil "~{~a~}" numlist))
                        4)))
           (return (funcall (find-keybind (read-key-sequence))
                            arg))))))))

(define-command self-insert (n) ("p")
  (let ((c (insertion-key-p *last-read-key-sequence*)))
    (cond (c
           (insert-char c n))
          (t
           (editor-error "Key not found: ~a"
                         (kbd-to-string *last-read-key-sequence*))))))

(defun load-init-file ()
  (flet ((test (path)
               (when (cl-fad:file-exists-p path)
                 (load path)
                 (message "Load file: ~a" path)
                 t)))
    (or (test (merge-pathnames "lem.rc" (truename ".")))
        (test (merge-pathnames ".lemrc" (user-homedir-pathname))))))

(defun popup-backtrace (condition)
  (info-popup (get-buffer-create "*Error*")
              #'(lambda (out)
                  (princ condition out)
                  (fresh-line out)
                  (uiop/image:print-backtrace
                   :stream out :count 100))))

(defmacro with-error-handler (() &body body)
  `(handler-case-bind (#'(lambda (condition)
                           (handler-bind ((error #'bailout))
                             (popup-backtrace condition)))
                       ,@body)
                      ((condition) (declare (ignore condition)))))

(defun lem-main ()
  (macrolet ((form (&body body)
                   `(cond (*debug-p*
                           (handler-bind ((error #'bailout)
                                          #+sbcl (sb-sys:interactive-interrupt #'bailout))
                             ,@body))
                          (t
                           ,@body))))
    (do ((*curr-flags* (make-flags) (make-flags))
         (*last-flags* (make-flags) *curr-flags*))
        (nil)
      (with-error-handler ()
        (form
         (redraw-display)
         (message nil)
         (let* ((key (read-key-sequence))
                (cmd (find-keybind key)))
           (handler-case (cmd-call cmd nil)
             (editor-abort ()
               (keyboard-quit))
             (readonly ()
               (message "Read Only"))
             (editor-error (c)
               (message (editor-error-message c))))))))))

(let ((passed nil))
  (defun lem-init (args)
    (term-init)
    (setq *running-p* t)
    (when (not passed)
      (setq passed t)
      (display-init)
      (window-init)
      (minibuf-init)
      (with-error-handler ()
        (load-init-file)))
    (dolist (arg args)
      (find-file arg))))

(defun lem-finallize ()
  (term-finallize)
  (setq *running-p* nil))

(defvar +exit-tag+ (gensym "EXIT"))

(defun exit-editor (&optional report)
  (throw +exit-tag+ report))

(defun lem-1 (args)
  (let ((report
         (catch +exit-tag+
           (unwind-protect
             (progn
               (lem-init args)
               (lem-main))
             (lem-finallize)))))
    (when report
      (format t "~&~a~%" report))))

(defun check-init ()
  (when *running-p*
    (error "~A is already running" *program-name*)))

(defun lem (&rest args)
  (check-init)
  (lem-1 args))

(defun bailout (condition)
  (exit-editor
   (with-output-to-string (stream)
     (princ condition stream)
     (uiop/image:print-backtrace
      :stream stream
      :condition condition))))

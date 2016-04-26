(in-package :lem)

(export '(macro-running-p
          read-key
          unread-key
          find-keybind
          describe-key
          describe-bindings
          begin-macro
          end-macro
          execute-macro
          apply-macro-to-region-lines
          sit-for
          universal-argument
          input-key
          self-insert
          lem))

(defvar *running-p* nil)

(defvar *input-history* (lem.queue:make-queue 100))

(defvar *macro-recording-p* nil)
(defvar *macro-chars* nil)
(defvar *macro-running-p* nil)

(defun macro-running-p () *macro-running-p*)

(defvar *input-queue* nil)

(defun read-key ()
  (let ((char (if (null *input-queue*)
                  (get-char nil)
                  (pop *input-queue*))))
    (lem.queue:enqueue *input-history* char)
    (when *macro-recording-p*
      (push char *macro-chars*))
    char))

(defun unread-key (char)
  (when *macro-recording-p*
    (pop *macro-chars*))
  (push char *input-queue*))

(defun input-enqueue (c)
  (check-type c character)
  (setf *input-queue*
        (nconc *input-queue*
               (list c))))

(defun input-queue-length ()
  (length *input-queue*))

(defun uninput-key (key)
  (mapc 'input-enqueue (kbd-list key)))

(define-key *global-keymap* (kbd "C-g") 'keyboard-quit)
(define-command keyboard-quit () ()
  (setq *universal-argument* nil)
  (setq *macro-recording-p* nil)
  (buffer-mark-cancel (current-buffer))
  (message "Quit"))

(defun find-keybind (key)
  (or (some #'(lambda (mode)
                (keymap-find-keybind (mode-keymap mode) key))
            (buffer-minor-modes))
      (keymap-find-keybind (mode-keymap (buffer-major-mode)) key)
      (keymap-find-keybind *global-keymap* key)))

(define-key *global-keymap* (kbd "C-x ?") 'describe-key)
(define-command describe-key () ()
  (message "describe-key: ")
  (let* ((key (input-key))
         (cmd (find-keybind key)))
    (message "describe-key: ~a ~a"
             (kbd-to-string key)
             cmd)))

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

(define-key *global-keymap* (kbd "C-x (") 'begin-macro)
(define-command begin-macro () ()
  (cond (*macro-recording-p*
         (message "Macro already active")
         nil)
        (t
         (message "Start macro")
         (setq *macro-recording-p* t)
         (setq *macro-chars* nil)
         t)))

(define-key *global-keymap* (kbd "C-x )") 'end-macro)
(define-command end-macro () ()
  (cond (*macro-running-p* t)
        ((not *macro-recording-p*)
         (message "Macro not active"))
        (t
         (setq *macro-recording-p* nil)
         (message "End macro")
         t)))

(define-key *global-keymap* (kbd "C-x e") 'execute-macro)
(define-command execute-macro (n) ("p")
  (cond (*macro-recording-p*
         (message "Macro already active")
         nil)
        (*macro-running-p*
         nil)
        (t
         (let ((*macro-running-p* t)
               (*universal-argument* nil))
           (prog1
               (loop
                 :named outer
                 :repeat n
                 :while *macro-running-p*
                 :do (let ((length (input-queue-length)))
                       (mapc 'input-enqueue (reverse *macro-chars*))
                       (loop :while (< length (input-queue-length)) :do
                         (unless *macro-running-p*
                           (loop :while (< length (input-queue-length))
                             :do (read-key))
                           (return-from outer nil))
                         (main-step)))
                 :finally (return-from outer t)))))))

(define-command apply-macro-to-region-lines () ()
  (apply-region-lines (region-beginning)
                      (region-end)
                      #'(lambda ()
                          (execute-macro 1)))
  t)

(defun sit-for (seconds &optional (update-window-p t))
  (when update-window-p (redraw-display))
  (multiple-value-bind (char timeout-p)
      (get-char (floor (* seconds 1000)))
    (cond (timeout-p t)
          ((char= char C-g) (error 'editor-abort)) ;???
          (t (unread-key char)
             nil))))

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
        (setq *universal-argument*
              (if numlist
                  (parse-integer (format nil "~{~a~}" numlist))
                  4))
        (return (main-step)))))))

(defun input-key ()
  (let ((key
         (let ((c (read-key)))
           (if (or (char= c C-x)
                   (char= c escape))
               (let ((c2 (read-key)))
                 (if (char= c2 escape)
                     (kbd c c2 (read-key))
                     (kbd c c2)))
               (kbd c)))))
    (setq *last-input-key* key)))

(defun main-step ()
  (let ((key (input-key)))
    (prog1 (let ((cmd (find-keybind key)))
             (and cmd
                  (or (handler-case (cmd-call cmd *universal-argument*)
                        (editor-abort ()
                                      (keyboard-quit)
                                      nil)
                        (readonly ()
                                  (message "Read Only")
                                  nil)
                        (editor-error (c)
                                      (message (editor-error-message c))
                                      nil))
                      (setq *macro-running-p* nil))))
      (setq *universal-argument* nil))))

(define-command self-insert (n) ("p")
  (let ((c (insertion-key-p *last-input-key*)))
    (cond (c
           (insert-char c n))
          (t
           (message
            "Key not found: ~a"
            (kbd-to-string *last-input-key*))))))

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
         (main-step))))))

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

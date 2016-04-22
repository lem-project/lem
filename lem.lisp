;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*lem-error-file*
          macro-running-p
          exit-lem
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

(defvar *lem-error-file* "~/.lem-error")
(defvar *initialized-p* nil)
(defvar *running-p* nil)

(defvar *exit*)

(defvar *input-history* (lem.queue:make-queue 100))

(defvar *macro-recording-p* nil)
(defvar *macro-chars* nil)
(defvar *macro-running-p* nil)

(defun macro-running-p () *macro-running-p*)

(defvar *input-queue* nil)

(defun getch (&optional (abort-jump t))
  (let* ((code (cond ((null *input-queue*)
                      (loop
                        :for code := (charms/ll:getch)
                        :while (= -1 code)
                        :finally (return code)))
                     (t
                      (pop *input-queue*))))
         (char (code-char code)))
    (lem.queue:enqueue *input-history* char)
    (when *macro-recording-p*
      (push char *macro-chars*))
    (cond ((= code 410)
           (minibuf-resize)
           (update-screen-size)
           (getch))
          ((and (char= char C-g) abort-jump)
           (error 'editor-abort))
          (t char))))

(defun ungetch (char)
  (when *macro-recording-p*
    (pop *macro-chars*))
  (push (char-code char) *input-queue*))

(defun input-enqueue (c)
  (setf *input-queue*
        (nconc *input-queue*
               (list (etypecase c
                       (character (char-code c))
                       (fixnum c))))))

(defun input-queue-length ()
  (length *input-queue*))

(defun uninput-key (key)
  (mapc 'input-enqueue (kbd-list key)))

(define-key *global-keymap* (kbd "C-g") 'keyboard-quit)
(define-command keyboard-quit () ()
  (setq *universal-argument* nil)
  (setq *macro-recording-p* nil)
  (buffer-mark-cancel (current-buffer))
  (minibuf-print "Quit"))

(define-key *global-keymap* (kbd "C-x C-c") 'exit-lem)
(define-command exit-lem () ()
  (when (or (not (any-modified-buffer-p))
            (minibuf-y-or-n-p "Modified buffers exist. Leave anyway"))
    (setq *exit* t)))

(define-command quick-exit () ()
  (save-some-buffers t)
  (setq *exit* t))

(defun find-keybind (key)
  (or (some #'(lambda (mode)
                (mode-find-keybind mode key))
            (buffer-minor-modes))
      (mode-find-keybind (buffer-major-mode) key)
      (keymap-find-keybind *global-keymap* key)))

(define-key *global-keymap* (kbd "C-x ?") 'describe-key)
(define-command describe-key () ()
  (minibuf-print "describe-key: ")
  (let* ((key (input-key))
         (cmd (find-keybind key)))
    (minibuf-print (format nil "describe-key: ~a ~a"
                           (kbd-to-string key)
                           cmd))))

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
         (minibuf-print "Macro already active")
         nil)
        (t
         (minibuf-print "Start macro")
         (setq *macro-recording-p* t)
         (setq *macro-chars* nil)
         t)))

(define-key *global-keymap* (kbd "C-x )") 'end-macro)
(define-command end-macro () ()
  (cond (*macro-running-p* t)
        ((not *macro-recording-p*)
         (minibuf-print "Macro not active"))
        (t
         (setq *macro-recording-p* nil)
         (minibuf-print "End macro")
         t)))

(define-key *global-keymap* (kbd "C-x e") 'execute-macro)
(define-command execute-macro (n) ("p")
  (cond (*macro-recording-p*
         (minibuf-print "Macro already active")
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
                             :do (getch))
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
  (when update-window-p (redraw-screen))
  (charms/ll:timeout (floor (* seconds 1000)))
  (let ((code (charms/ll:getch)))
    (charms/ll:timeout -1)
    (cond ((= code -1)
           t)
          ((= code (char-code C-g))
           (error 'editor-abort))
          (t
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
        (ungetch c)
        (setq *universal-argument*
              (if numlist
                  (parse-integer (format nil "~{~a~}" numlist))
                  4))
        (return (main-step)))))))

(defun input-char (code)
  (let* ((nbytes (utf8-bytes code)))
    (if (= nbytes 1)
        (code-char code)
        (aref (babel:octets-to-string
               (coerce
                (cons code
                      (loop repeat (1- nbytes)
                        collect (char-code (getch))))
                '(vector (unsigned-byte 8))))
              0))))

(defun input-key ()
  (let ((key
         (let ((c (getch nil)))
           (if (or (char= c C-x)
                   (char= c escape))
               (let ((c2 (getch nil)))
                 (if (char= c2 escape)
                     (kbd c c2 (getch nil))
                     (kbd c c2)))
               (kbd (input-char
                     (char-code c)))))))
    (setq *last-input-key* key)))

(defun execute (key)
  (let* ((cmd (find-keybind key))
         (buffer (current-buffer))
         (prev-modified (buffer-modified-p buffer))
         (prev-window-vtop-linum (window-vtop-linum (current-window)))
         (prev-window-tree *window-tree*))
    (prog1 (and cmd
                (or (handler-case (cmd-call cmd *universal-argument*)
                      (editor-abort ()
                        (keyboard-quit)
                        nil)
                      (readonly ()
                        (minibuf-print "Read Only")
                        nil)
                      (editor-error (c)
                        (minibuf-print (editor-error-message c))
                        nil))
                    (setq *macro-running-p* nil)))
      (when (and *enable-syntax-highlight*
                 (not *macro-running-p*)
                 (eq buffer (current-buffer)))
        (let ((curr-modified (buffer-modified-p (current-buffer))))
          (cond ((eq :one-line (window-redraw-flag (current-window)))
                 (syntax-scan-lines (current-window)
                                    #1=(window-current-linum)
                                    (1+ #1#)))
                ((or (not (eql curr-modified prev-modified))
                     (/= prev-window-vtop-linum
                         (window-vtop-linum (current-window)))
                     (/= 0 (window-offset-view (current-window))))
                 (syntax-scan-window (current-window)))
                ((eq *window-tree* prev-window-tree)
                 )))))))

(defun main-step ()
  (let ((key (input-key)))
    (minibuf-clear)
    (prog1 (execute key)
      (setq *universal-argument* nil))))

(define-command self-insert (n) ("p")
  (let ((c (insertion-key-p *last-input-key*)))
    (cond (c
           (insert-char c n))
          (t
           (minibuf-print (format nil
                                  "Key not found: ~a"
                                  (kbd-to-string *last-input-key*)))))))

(defun load-init-file ()
  (flet ((test (path)
               (when (cl-fad:file-exists-p path)
                 (load path)
                 (minibuf-print (format nil "Load file: ~a" path))
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
                           (handler-case (popup-backtrace condition)
                             (error (condition)
                                    (throw 'serious-error
                                      (with-output-to-string (stream)
                                        (princ condition stream)
                                        (uiop/image:print-backtrace
                                         :stream stream
                                         :condition condition))))))
                       ,@body)
                      ((condition) (declare (ignore condition)))))

(defun idle ()
  (when (null *input-queue*)
    (run-hooks 'idle-hook)
    (cond ((exist-running-timer-p)
           (charms/ll:timeout 20)
           (unwind-protect
             (loop
               (let ((code (charms/ll:getch)))
                 (cond ((= code -1)
                        (when (update-timer)
                          (redraw-screen)))
                       (t
                        (charms/ll:ungetch code)
                        (return)))))
             (charms/ll:timeout -1))))))

(defun lem-main ()
  (flet ((body ()
           (redraw-screen)
           (idle)
           (main-step)))
    (do ((*exit*)
         (*curr-flags* (make-flags) (make-flags))
         (*last-flags* (make-flags) *curr-flags*))
        (*exit*)
      (with-error-handler ()
        (if *debug-p*
            (handler-bind ((error #'save-error)
                           #+sbcl (sb-sys:interactive-interrupt #'save-error))
              (body))
            (body))))))

(defun lem-init (args)
  (term-init)
  (setq *running-p* t)
  (when (not *initialized-p*)
    (setq *initialized-p* t)
    (screen-init)
    (window-init)
    (minibuf-init)
    (with-error-handler ()
      (load-init-file)))
  (dolist (arg args)
    (find-file arg)))

(defun lem-finallize ()
  (term-finallize)
  (setq *running-p* nil))

(defun lem-1 (args)
  (let ((error-report
         (catch 'serious-error
           (unwind-protect
             (progn
               (lem-init args)
               (lem-main))
             (lem-finallize)))))
    (when error-report
      (format t "~&~a~%" error-report))))

(defun check-init ()
  (when *running-p*
    (error "~a is already initialized" *program-name*)))

(defun lem (&rest args)
  (check-init)
  (lem-1 args))

(defun save-error (condition)
  (with-open-file (out *lem-error-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-circle* t))
      (format out "~&~%~%~%~%~a~%" condition)
      (format out "~s~%"
              (lem.queue:queue-to-list *input-history*))
      (uiop/image:print-backtrace :stream out :count 100)
      (throw 'serious-error
        (with-output-to-string (stream)
          (princ condition stream)
          (uiop/image:print-backtrace
           :stream stream
           :condition condition)))
      )))

(defun dired-buffer (filename)
  (save-excursion
    (lem.dired:dired filename)
    (current-buffer)))

#+sbcl
(push #'(lambda (x)
          (if x
              (lem x)
              (lem))
          t)
      sb-ext:*ed-functions*)

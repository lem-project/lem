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
          universal-argument
          input-key
          self-insert
          lem
          lem-save-error))

(defvar *lem-error-file* "~/.lem-error")
(defvar *init-flag* nil)

(defvar *exit*)
(defvar *self-insert-prev-time* nil)

(defvar *input-history* (queue:make-queue 100))

(defvar *macro-recording-p* nil)
(defvar *macro-chars* nil)
(defvar *macro-running-p* nil)

(defun macro-running-p () *macro-running-p*)

(let ((keys nil))
  (defun getch (&optional (abort-jump t))
    (let* ((code (cond (*getch-wait-flag*
                        (loop while (null keys))
                        (pop keys))
                       (keys
                        (pop keys))
                       (t
                        (loop for result = (cl-charms/low-level:wgetch (window-win))
                           while (minusp result)
                           finally (return result)))))
           (char (code-char code)))
      (queue:enqueue *input-history* char)
      (when *macro-recording-p*
        (push char *macro-chars*))
      (cond
       ((= code 410)
        (minibuf-resize)
        (adjust-screen-size)
        (getch))
       ((and (char= char C-g) abort-jump)
        (throw 'abort 'abort))
       (t char))))
  (defun ungetch (c)
    (when *macro-recording-p*
      (pop *macro-chars*))
    (push (char-code c) keys))
  (defun getch-count-ungetch ()
    (length keys))
  (defun getch-flush ()
    (setq keys nil)))

(define-key *global-keymap* (kbd "C-g") 'keyboard-quit)
(define-command keyboard-quit () ()
  (setq *universal-argument* nil)
  (setq *macro-recording-p* nil)
  (delete-completion-window)
  (minibuf-print "Quit"))

(define-key *global-keymap* (kbd "C-x C-c") 'exit-lem)
(define-command exit-lem () ()
  (when (or (not (any-modified-buffer-p))
            (minibuf-y-or-n-p "Modified buffers exist. Leave anyway"))
    (setq *exit* t)))

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

(define-command describe-bindings () ()
  (let ((column-width 16)
        (keymaps (append (mapcar #'mode-keymap (buffer-minor-modes))
                         (list (mode-keymap (major-mode))
                               *global-keymap*)))
        (tmpbuf (get-buffer-create "*bindings*")))
    (info-popup tmpbuf
                #'(lambda (s)
                    (loop :for keymap :in keymaps :do
                      (princ (lem:keymap-name keymap) s)
                      (terpri s)
                      (format s "~va~a~%" column-width "key" "binding")
                      (format s "~va~a~%" column-width "---" "-------")
                      (maphash #'(lambda (k v)
                                   (format s "~va~a~%"
                                           column-width
                                           (kbd-to-string k)
                                           (symbol-name v)))
                               (keymap-table keymap))
                      (terpri s))))))

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
                 :do (let ((length (getch-count-ungetch)))
                       (dolist (c *macro-chars*)
                         (ungetch c))
                       (loop :while (< length (getch-count-ungetch)) :do
                         (unless *macro-running-p*
                           (loop :while (< length (getch-count-ungetch))
                             :do (getch))
                           (return-from outer nil))
                         (main-step)))
                 :finally (return-from outer t))
             (setf (window-redraw-flag) :all))))))

(define-command apply-macro-to-region-lines () ()
  (apply-region-lines (region-beginning)
                      (region-end)
                      #'(lambda ()
                          (execute-macro 1)))
  t)

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
        (return
          (prog1 (execute (input-key))
            (setq *universal-argument* nil))))))))

(defun input-char (code &optional getchar-fn)
  (let* ((nbytes (utf8-bytes code))
         (char (if (= nbytes 1)
                   (code-char code)
                   (aref (babel:octets-to-string
                          (coerce
                           (cons code
                                 (loop repeat (1- nbytes)
                                   collect (if getchar-fn
                                               (funcall getchar-fn)
                                               (char-code (getch)))))
                           '(vector (unsigned-byte 8))))
                         0))))
    char))

(defun input-key ()
  (let ((key
         (let ((c (getch nil)))
           (if (or (char= c C-x)
                   (char= c escape))
               (kbd c (getch nil))
               (kbd (input-char
                     (char-code c)))))))
    (setq *last-input-key* key)))

(defun execute (key)
  (let* ((cmd (find-keybind key))
         (buffer (window-buffer))
         (prev-modified (buffer-modified-p buffer))
         (prev-window-vtop-linum (window-vtop-linum)))
    (prog1 (and cmd
                (or (cmd-call cmd *universal-argument*)
                    (setq *macro-running-p* nil)))
      (when (and (not *macro-running-p*)
                 (eq buffer (window-buffer)))
        (let ((curr-modified (buffer-modified-p (window-buffer))))
          (cond ((eq :one-line (window-redraw-flag))
                 (syntax-scan-lines *current-window*
                                    #1=(window-cur-linum)
                                    (1+ #1#)))
                ((or (not (eql curr-modified prev-modified))
                     (/= prev-window-vtop-linum
                         (window-vtop-linum))
                     (/= 0 (window-offset-view *current-window*)))
                 (syntax-scan-window *current-window*))))))))

(defun main-step ()
  (let ((key (input-key)))
    (minibuf-clear)
    (execute key)
    (setq *universal-argument* nil)))

(define-command self-insert (n) ("p")
  (let ((c (insertion-key-p *last-input-key*)))
    (if c
        (progn
          (setf (window-redraw-flag) :one-line)
          (insert-char c (or *universal-argument* 1))
          (when (and (not *macro-running-p*)
                     *self-insert-prev-time*
                     (> 10
                        (- (get-internal-real-time)
                           *self-insert-prev-time*)))
            (exec-paste))
          (setq *self-insert-prev-time* (get-internal-real-time)))
        (minibuf-print (format nil
                               "Key not found: ~a"
                               (kbd-to-string *last-input-key*))))))

(defun exec-paste ()
  (cl-charms/low-level:timeout 10)
  (do ((code #1=(cl-charms/low-level:getch) #1#))
      ((= code -1))
    (when *macro-recording-p*
      (push (code-char code) *macro-chars*))
    (let* ((char (input-char code)))
      (if (or (char= char C-j)
              (char= char C-m))
          (insert-newline 1)
          (insert-char char 1))))
  (cl-charms/low-level:timeout -1)
  (window-update-all))

(defun load-init-file ()
  (flet ((test (path)
               (when (file-exist-p path)
                 (load path)
                 (minibuf-print (format nil "Load file: ~a" path))
                 t)))
    (or (test (merge-pathnames "lem.rc" (truename ".")))
        (test (merge-pathnames ".lemrc" (user-homedir-pathname))))))

(defun attr-init ()
  (when (/= 0 (cl-charms/low-level:has-colors))
    (cl-charms/low-level:start-color)
    (cl-charms/low-level:use-default-colors)
    (loop
      :for color
      :in (list cl-charms/low-level:color_yellow
                cl-charms/low-level:color_green
                cl-charms/low-level:color_blue
                cl-charms/low-level:color_magenta
                cl-charms/low-level:color_red
                cl-charms/low-level:color_cyan)
      :for attr-name :in *color-names*
      :for num :from 1
      :do
      (cl-charms/low-level:init-pair num color -1)
      (set-attr attr-name (cl-charms/low-level:color-pair num)))
    (syntax-init-attributes))
  (set-attr :highlight cl-charms/low-level:a_reverse)
  (set-attr :search-highlight
            (logior (get-attr :highlight)
                    (get-attr :cyan))))

(defun lem-init (args)
  (cl-charms/low-level:initscr)
  (attr-init)
  (cl-charms/low-level:noecho)
  (cl-charms/low-level:cbreak)
  (cl-charms/low-level:raw)
  (cl-charms/low-level:nonl)
  (cl-charms/low-level:refresh)
  (unless *init-flag*
    (setq *init-flag* t)
    (window-init)
    (minibuf-init)
    (load-init-file))
  (dolist (arg args)
    (find-file arg)))

(defun lem-finallize ()
  (cl-charms/low-level:endwin))

(defun lem-main ()
  (do ((*exit* nil)
       (*curr-flags* (make-flags) (make-flags))
       (*last-flags* (make-flags) *curr-flags*))
      (*exit*)
    (window-require-update)
    (case (catch 'abort
            (main-step)
            nil)
      (readonly
       (minibuf-print "Read Only"))
      (abort
       (keyboard-quit)))))

(defun lem (&rest args)
  (labels ((handler (cdt)
                    (info-popup (get-buffer-create "*Error*")
                                #'(lambda (out)
                                    (princ cdt out)
                                    #+sbcl
                                    (sb-debug:backtrace 100 out)))
                    cdt)
           (f ()
              (handler-case
                  (handler-bind
                      ((error #'handler))
                    (lem-main))
                (error (cdt)
                       (f)))))
    (unwind-protect
      (progn
        (lem-init args)
        (f))
      (lem-finallize))))

#+sbcl
(defun dump-error (condition)
  (with-open-file (out *lem-error-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (let ((*print-circle* t))
      (format out "~&~%~%~%~%~a~%" condition)
      (format out "~s~%"
              (queue:queue-to-list *input-history*))
      (sb-debug:backtrace 100 out))))

(defun lem-save-error (&rest args)
  (lem-init args)
  (unwind-protect (progn
                    #+sbcl (handler-bind
                               ((sb-sys:interactive-interrupt #'dump-error)
                                (error #'dump-error))
                             (lem-main))
                    #-sbcl (lem-main))
    (lem-finallize)))

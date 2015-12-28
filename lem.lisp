;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*lem-error-file*
          uninput-key
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
          popup-backtrace
          lem
          lem-save-error
          lem-new-term
          xterm-fd
          run-xterm-p))

(defvar *lem-error-file* "~/.lem-error")
(defvar *initialized-p* nil)
(defvar *running-p* nil)

(defvar *exit*)

(defvar *input-history* (queue:make-queue 100))

(defvar *macro-recording-p* nil)
(defvar *macro-chars* nil)
(defvar *macro-running-p* nil)

(defun macro-running-p () *macro-running-p*)

(defvar *input-queue* (make-growlist))

(defun getch (&optional (abort-jump t))
  (let* ((code (cond (*getch-wait-p*
                      (loop :while (grow-null-p *input-queue*)
                        :do (sleep 0.01))
                      (grow-rem-left *input-queue*))
                     ((grow-null-p *input-queue*)
                      (loop
                        :for code := (charms/ll:wgetch (window-win))
                        :while (= -1 code)
                        :finally (return code)))
                     (t
                      (grow-rem-left *input-queue*))))
         (char (code-char code)))
    (queue:enqueue *input-history* char)
    (when *macro-recording-p*
      (push char *macro-chars*))
    (cond ((= code 410)
           (minibuf-resize)
           (adjust-screen-size)
           (getch))
          ((and (char= char C-g) abort-jump)
           (throw 'abort 'abort))
          (t char))))

(defun ungetch (char)
  (when *macro-recording-p*
    (pop *macro-chars*))
  (grow-add-left *input-queue* (char-code char)))

(defun input-enqueue (c)
  (grow-add-right *input-queue* (etypecase c
                                  (character (char-code c))
                                  (fixnum c))))

(defun input-queue-length ()
  (length (grow-list *input-queue*)))

(defun uninput-key (key)
  (mapc 'input-enqueue (kbd-list key)))

(define-key *global-keymap* (kbd "C-g") 'keyboard-quit)
(define-command keyboard-quit () ()
  (setq *universal-argument* nil)
  (setq *macro-recording-p* nil)
  (buffer-mark-cancel (window-buffer))
  (delete-completion-window)
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
                 :finally (return-from outer t))
             (setf (window-redraw-flag) :all))))))

(define-command apply-macro-to-region-lines () ()
  (apply-region-lines (region-beginning)
                      (region-end)
                      #'(lambda ()
                          (execute-macro 1)))
  t)

(defun sit-for (seconds)
  (window-update-all)
  (charms/ll:timeout (floor (* seconds 1000)))
  (let ((code (charms/ll:getch)))
    (charms/ll:timeout -1)
    (cond ((= code -1)
           t)
          ((= code (char-code C-g))
           (throw 'abort 'abort))
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
         (buffer (window-buffer))
         (prev-modified (buffer-modified-p buffer))
         (prev-window-vtop-linum (window-vtop-linum))
         (prev-window-tree *window-tree*))
    (prog1 (and cmd
                (or (cmd-call cmd *universal-argument*)
                    (setq *macro-running-p* nil)))
      (when (and *enable-syntax-highlight*
                 (not *macro-running-p*)
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
                 (syntax-scan-window *current-window*))
                ((eq *window-tree* prev-window-tree)
                 (setf (window-redraw-flag) :unnecessary))))))))

(defun main-step ()
  (let ((key (input-key)))
    (minibuf-clear)
    (prog1 (execute key)
      (setq *universal-argument* nil))))

(let ((prev-time))
  (define-command self-insert (n) ("p")
    (let ((c (insertion-key-p *last-input-key*)))
      (cond (c
             (setf (window-redraw-flag) :one-line)
             (insert-char c n)
             (when (and prev-time
                        (> 10
                           (- (get-internal-real-time)
                              prev-time)))
               (exec-paste))
             (setq prev-time (get-internal-real-time)))
            (t
             (minibuf-print (format nil
                                    "Key not found: ~a"
                                    (kbd-to-string *last-input-key*))))))))

(defun exec-paste ()
  (charms/ll:timeout 10)
  (loop :for code := (charms/ll:getch) :do
    (when (= code -1)
      (return))
    (when *macro-recording-p*
      (push (code-char code) *macro-chars*))
    (let ((char (input-char code)))
      (if (or (char= char C-j)
              (char= char C-m))
          (insert-newline)
          (insert-char char 1))))
  (charms/ll:timeout -1))

(defun load-init-file ()
  (flet ((test (path)
               (when (cl-fad:file-exists-p path)
                 (load path)
                 (minibuf-print (format nil "Load file: ~a" path))
                 t)))
    (or (test (merge-pathnames "lem.rc" (truename ".")))
        (test (merge-pathnames ".lemrc" (user-homedir-pathname))))))

(defun attr-init ()
  (when (/= 0 (charms/ll:has-colors))
    (charms/ll:start-color)
    (charms/ll:use-default-colors)
    (let ((colors
           (list (cons :yellow charms/ll:color_yellow)
                 (cons :green charms/ll:color_green)
                 (cons :blue charms/ll:color_blue)
                 (cons :magenta charms/ll:color_magenta)
                 (cons :red charms/ll:color_red)
                 (cons :cyan charms/ll:color_cyan)
                 (cons :white charms/ll:color_white)
                 (cons :black charms/ll:color_black)))
          (n 0))
      (flet ((add-color (ncurses-fg ncurses-bg name)
                        (incf n)
                        (charms/ll:init-pair n ncurses-fg ncurses-bg)
                        (set-attr name (charms/ll:color-pair n))))
        (loop :for (fg . ncurses-fg) :in colors :do
          (loop :for (bg . ncurses-bg) :in colors :do
            (add-color ncurses-fg
                       ncurses-bg
                       (cons fg bg)))
          (add-color ncurses-fg -1 fg))))
    (syntax-init-attributes)
    (set-attr :highlight charms/ll:a_reverse)
    (set-attr :search-highlight
              (logior (get-attr :highlight)
                      (get-attr :cyan)))))

(defun popup-backtrace (condition)
  (info-popup (get-buffer-create "*Error*")
              #'(lambda (out)
                  (princ condition out)
                  (fresh-line out)
                  (uiop/image:print-backtrace
                   :stream out :count 100))))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error #'popup-backtrace))
         ,@body)
     (error ())))

(defun idle ()
  (when (grow-null-p *input-queue*)
    (charms/ll:wtimeout (window-win) 20)
    (unwind-protect
      (loop
        (let ((code (charms/ll:wgetch (window-win))))
          (cond ((= code -1)
                 (when (update-timer)
                   (window-maybe-update)))
                (t
                 (charms/ll:ungetch code)
                 (return)))))
      (charms/ll:wtimeout (window-win) -1))))

(defun lem-main ()
  (flet ((body ()
               (window-maybe-update)
               (idle)
               (case (catch 'abort
                       (main-step)
                       nil)
                 (readonly
                  (minibuf-print "Read Only"))
                 (abort
                  (keyboard-quit)))))
    (do ((*exit*)
         (*curr-flags* (make-flags) (make-flags))
         (*last-flags* (make-flags) *curr-flags*))
        (*exit*)
      (with-error-handler ()
        (if *debug-p*
            (handler-bind ((error #'save-error)
                           (sb-sys:interactive-interrupt #'save-error))
              (body))
            (body))))))

(defun lem-init (args)
  (attr-init)
  (charms/ll:noecho)
  (charms/ll:cbreak)
  (charms/ll:raw)
  (charms/ll:nonl)
  (charms/ll:refresh)
  (setq *running-p* t)
  (cond ((not *initialized-p*)
         (setq *initialized-p* t)
         (window-init)
         (minibuf-init)
         (with-error-handler ()
           (load-init-file)))
        (t
         (dolist (window (cons *minibuf-window* (window-list)))
           (setf (window-win window)
                 (charms/ll:newwin (window-nlines window)
                                   (window-ncols window)
                                   (window-y window)
                                   (window-x window)))
           (charms/ll:keypad (window-win window) 1))))
  (dolist (arg args)
    (find-file arg)))

(defun lem-finallize ()
  (dolist (window (cons *minibuf-window* (window-list)))
    (charms/ll:delwin (window-win window)))
  (charms/ll:endwin)
  (dolist (window (cons *minibuf-window* (window-list)))
    (charms/ll:delscreen (window-win window)))
  (charms/ll:delscreen charms/ll:*stdscr*)
  (setq *running-p* nil))

(defun lem-internal (args)
  (unwind-protect
    (progn
      (lem-init args)
      (lem-main))
    (lem-finallize)))

(defun check-init ()
  (when *running-p*
    (error "~a is already initialized" *program-name*)))

(defun lem (&rest args)
  (check-init)
  (charms/ll:initscr)
  (lem-internal args))

(defun new-xterm (geometry foreground background title font)
  (let ((tmpfile (temp-file-name))
        tty-name)
    (uiop:run-program
     (concatenate 'string
                  "xterm"
                  (if title (format nil " -title ~a" title) "")
                  (if foreground (format nil " -fg ~a" foreground) "")
                  (if background (format nil " -bg ~a" background) "")
                  (if font (format nil " -fn ~a" font) "")
                  (if geometry (format nil " -geometry ~a" geometry) "")
                  (format nil " -e 'tty > ~a && sleep 100000' &" tmpfile)))
    (loop
      (sleep 0.1)
      (multiple-value-bind (unused-value error-p)
          (ignore-errors
           (with-open-file (in tmpfile)
             (setq tty-name (read-line in))))
        (declare (ignore unused-value))
        (unless error-p (return))))
    (delete-file tmpfile)
    tty-name))

(cffi:defcfun "fopen" :pointer (path :string) (mode :string))
(cffi:defcfun "fclose" :int (fp :pointer))
(cffi:defcfun "fileno" :int (fd :pointer))

(defvar *xterm-fd* nil)

(defun xterm-fd ()
  *xterm-fd*)

(defun run-xterm-p ()
  (not (null *xterm-fd*)))

(defun lem-new-term (&key (geometry "80x24")
                          (foreground nil)
                          (background nil)
                          (title *program-name*)
                          (font nil))
  (check-init)
  (let* ((tty-name (new-xterm geometry foreground background title font))
         (io (fopen tty-name "r+")))
    (setq *xterm-fd* (fileno io))
    (cffi:with-foreign-string (term "xterm")
      (charms/ll:newterm term io io))
    (when (stringp geometry)
      (ppcre:register-groups-bind (width height)
                                  ("^(\\d+)x(\\d+)$" geometry)
                                  (when (and width height)
                                    (charms/ll:resizeterm (parse-integer height)
                                                          (parse-integer width)))))
    #+sbcl
    (sb-thread:make-thread
     #'(lambda ()
         (sb-thread:with-new-session ()
           (unwind-protect (lem-internal nil)
             (fclose io)))))
    #-sbcl
    (bt:make-thread
     #'(lambda ()
         (unwind-protect (lem-internal nil)
           (fclose io))))))

(defun save-error (condition)
  (with-open-file (out *lem-error-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-circle* t))
      (format out "~&~%~%~%~%~a~%" condition)
      (format out "~s~%"
              (queue:queue-to-list *input-history*))
      (uiop/image:print-backtrace :stream out :count 100))))

(defun dired (filename)
  (dired:dired filename))

#+sbcl
(push #'(lambda (x)
          (if x
              (lem x)
              (lem))
          t)
      sb-ext:*ed-functions*)

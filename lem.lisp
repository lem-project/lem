(in-package :lem)

(export '(*lem-error-file*
          exit-lem
          describe-key
          begin-macro
          end-macro
          execute-macro
          apply-macro-to-region-lines
          universal-argument
          input-key
          undefined-key
          lem
          lem-save-error))

(defvar *lem-error-file* "~/.lem-error")
(defvar *init-flag* nil)

(defvar *exit*)

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
      (when *macro-recording-p*
        (push char *macro-chars*))
      (cond
       ((= code 410)
        (minibuf-resize)
        (adjust-screen-size)
        (getch))
       ((and (char= char key::ctrl-g) abort-jump)
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
  (minibuf-print "Quit"))

(define-key *global-keymap* (kbd "C-xC-c") 'exit-lem)
(define-command exit-lem () ()
  (when (or (not (any-modified-buffer-p))
            (minibuf-y-or-n-p "Modified buffers exist. Leave anyway"))
    (setq *exit* t)))

(define-key *global-keymap* (kbd "C-x?") 'describe-key)
(define-command describe-key () ()
  (minibuf-print "describe-key: ")
  (let* ((key (input-key))
         (cmd (mode-find-keybind key)))
    (minibuf-print (format nil "describe-key: ~a ~a"
                           (kbd-to-string key)
                           cmd))))

(define-key *global-keymap* (kbd "C-x(") 'begin-macro)
(define-command begin-macro () ()
  (cond (*macro-recording-p*
         (minibuf-print "Macro already active")
         nil)
        (t
         (minibuf-print "Start macro")
         (setq *macro-recording-p* t)
         (setq *macro-chars* nil)
         t)))

(define-key *global-keymap* (kbd "C-x)") 'end-macro)
(define-command end-macro () ()
  (cond (*macro-running-p* t)
        ((not *macro-recording-p*)
         (minibuf-print "Macro not active"))
        (t
         (setq *macro-recording-p* nil)
         (minibuf-print "End macro")
         t)))

(define-key *global-keymap* (kbd "C-xe") 'execute-macro)
(define-command execute-macro (n) ("p")
  (let ((*macro-running-p* t)
        (*universal-argument* nil))
    (loop repeat n while *macro-running-p* do
      (let ((length (getch-count-ungetch)))
        (dolist (c *macro-chars*)
          (ungetch c))
        (loop while (and *macro-running-p*
                         (< length (getch-count-ungetch)))
          do (main-step))))))

(define-command apply-macro-to-region-lines () ()
  (apply-region-lines (region-beginning)
                      (region-end)
                      (lambda ()
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
       ((char= c key::ctrl-u)
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
                   (aref (bytes-to-string
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
  (let ((c (getch nil)))
    (if (or (char= c key::ctrl-x)
            (char= c key::escape))
        (list c (getch nil))
        (list (input-char
               (char-code c))))))

(defun execute (key)
  (let* ((keymap (current-mode-keymap))
         (cmd (mode-find-keybind key)))
    (if cmd
        (if (cmd-call cmd *universal-argument*)
            t
            (setq *macro-running-p* nil))
        (key-undef-hook keymap key))))

(defun main-step ()
  (let ((key (input-key)))
    (minibuf-clear)
    (delete-completion-window)
    (execute key)
    (setq *universal-argument* nil)))

(let ((prev-time))
  (defun undefined-key (key)
    (let ((c (insertion-key-p key)))
      (cond (c (insert-char c (or *universal-argument* 1))
               (when (and (not *macro-running-p*)
                          prev-time
                          (> 10
                             (- (get-internal-real-time)
                                prev-time)))
                 (exec-paste))
               (setq prev-time (get-internal-real-time)))
            (t (minibuf-print (format nil
                                      "Key not found: ~a"
                                      (kbd-to-string key))))))))

(defun exec-paste ()
  (cl-charms/low-level:timeout 10)
  (do ((code #1=(cl-charms/low-level:getch) #1#))
      ((= code -1))
    (when *macro-recording-p*
      (push (code-char code) *macro-chars*))
    (let* ((char (input-char code)))
      (if (or (char= char key::ctrl-j)
              (char= char key::ctrl-m))
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

(defun lem-init (args)
  (cl-charms/low-level:initscr)
  (when (/= 0 (cl-charms/low-level:has-colors))
    (cl-charms/low-level:start-color)
    (cl-charms/low-level:use-default-colors)
    (loop
      for (i . color)
      in (list (cons *yellow*  cl-charms/low-level:color_yellow)
               (cons *green*   cl-charms/low-level:color_green)
               (cons *blue*    cl-charms/low-level:color_blue)
               (cons *magenta* cl-charms/low-level:color_magenta)
               (cons *red*     cl-charms/low-level:color_red)
               (cons *cyan*    cl-charms/low-level:color_cyan))
      do (cl-charms/low-level:init-pair i color -1)))
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
    (window-update-all)
    (case (catch 'abort
            (main-step)
            nil)
      (readonly
       (minibuf-print "Read Only"))
      (abort
       (keyboard-quit)))))

(defun lem (&rest args)
  (labels ((f ()
              (handler-case
                  (handler-bind ((error #'lisp-error-clause))
                    (lem-main))
                (error (cdt)
                       (f)))))
    (unwind-protect
      (progn
        (lem-init args)
        (f))
      (lem-finallize))))

(defun lem-save-error (&rest args)
  (let ((*print-circle* t))
    (with-open-file (out *lem-error-file*
                         :direction :output
                         :if-exists :overwrite
                         :if-does-not-exist :create)
      (let ((*error-output* out))
        (lem-init args)
        #+sbcl
        (handler-bind ((sb-sys:interactive-interrupt
                        #'(lambda (c) (declare (ignore c))
                            (sb-debug:backtrace 100 out))))
          (lem-main))
        #-sbcl
        (lem-main)
        (lem-finallize)))))

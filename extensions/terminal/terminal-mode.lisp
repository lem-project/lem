(uiop:define-package :lem-terminal/terminal-mode
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi))
  (:local-nicknames (:terminal :lem-terminal/terminal)))
(in-package :lem-terminal/terminal-mode)

(define-major-mode terminal-mode ()
    (:name "Terminal"
     :keymap *terminal-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t)
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil))

(define-key *terminal-mode-keymap* 'self-insert 'terminal-input)

(defun buffer-terminal (buffer)
  (buffer-value buffer 'terminal))

(defun (setf buffer-terminal) (terminal buffer)
  (setf (buffer-value buffer 'terminal) terminal))

(defun buffer-timer (buffer)
  (buffer-value buffer 'timer))

(defun (setf buffer-timer) (timer buffer)
  (setf (buffer-value buffer 'timer) timer))

(defun call-with-error-handler (function)
  (handler-bind ((error (lambda (c)
                          (message "~A"
                                   (with-output-to-string (out)
                                     (uiop:println c out)
                                     (uiop:print-backtrace :condition c :stream out))))))
    (funcall function)))

(defmacro with-error-handler (() &body body)
  `(call-with-error-handler (lambda () ,@body)))

(defvar *timer*
  (make-idle-timer (lambda ()
                     (dolist (terminal (terminal:terminals))
                       (when (eq (current-buffer) (terminal::terminal-buffer terminal))
                         (ignore-errors
                           (with-error-handler ()
                             (terminal:update terminal)
                             (terminal:render terminal))))))
                   :name "Terminal"))

(defun make-terminal-buffer (buffer-name)
  (let* ((buffer (make-buffer buffer-name))
         (terminal (terminal:create :cols 80 :rows 24 :buffer buffer)))
    (change-buffer-mode buffer 'terminal-mode)
    (setf (buffer-terminal buffer) terminal)
    (setf (buffer-timer buffer) *timer*)
    (start-timer *timer* 10 :repeat t)

    (add-hook (variable-value 'kill-buffer-hook :buffer buffer) 'on-kill-buffer)

    buffer))

(defun on-kill-buffer (buffer)
  (let ((terminal (buffer-terminal buffer)))
    (when terminal
      (terminal:destroy terminal)))
  (let ((timer (buffer-timer buffer)))
    (when timer
      (stop-timer timer))))

(define-command terminal () ()
  (let ((buffer (make-terminal-buffer "*terminal*")))
    (pop-to-buffer buffer)))


(defun get-current-terminal ()
  (let ((terminal (buffer-terminal (current-buffer))))
    (assert terminal)
    terminal))

(define-command terminal-input () ()
  (let ((char (get-self-insert-char)))
    (when char
      (let ((terminal (get-current-terminal)))
        (terminal:input-character terminal char)))))

(defmacro define-terminal-key-command (name keyspec vterm-key)
  (alexandria:with-unique-names (terminal)
    `(progn
       (define-key *terminal-mode-keymap* ,keyspec ',name )
       (define-command ,name () ()
         (let ((,terminal (get-current-terminal)))
           (terminal:input-key ,terminal ,vterm-key))))))

(define-terminal-key-command terminal-key-return "Return" ffi::vterm_key_enter)
(define-terminal-key-command terminal-key-backspace "Backspace" ffi::vterm_key_backspace)
(define-terminal-key-command terminal-key-tab "Tab" ffi::vterm_key_tab)
(define-terminal-key-command terminal-key-escape "Escape" ffi::vterm_key_escape)
(define-terminal-key-command terminal-key-up "Up" ffi::vterm_key_up)
(define-terminal-key-command terminal-key-down "Down" ffi::vterm_key_down)
(define-terminal-key-command terminal-key-left "Left" ffi::vterm_key_left)
(define-terminal-key-command terminal-key-right "Right" ffi::vterm_key_right)
(define-terminal-key-command terminal-key-insert "Insert" ffi::vterm_key_ins)
(define-terminal-key-command terminal-key-delete "Delete" ffi::vterm_key_del)
(define-terminal-key-command terminal-key-home "Home" ffi::vterm_key_home)
(define-terminal-key-command terminal-key-end "End" ffi::vterm_key_end)
(define-terminal-key-command terminal-key-pageup "PageUp" ffi::vterm_key_pageup)
(define-terminal-key-command terminal-key-pagedown "PageDown" ffi::vterm_key_pagedown)
(define-terminal-key-command terminal-key-f1 "F1" (+ 1 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f2 "F2" (+ 2 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f3 "F3" (+ 3 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f4 "F4" (+ 4 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f5 "F5" (+ 5 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f6 "F6" (+ 6 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f7 "F7" (+ 7 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f8 "F8" (+ 8 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f9 "F9" (+ 9 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f10 "F10" (+ 10 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f11 "F11" (+ 11 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f12 "F12" (+ 12 ffi::vterm_key_function_0))

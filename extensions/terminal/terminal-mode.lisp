(uiop:define-package :lem-terminal/terminal-mode
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi))
  (:local-nicknames (:terminal :lem-terminal/terminal)))
(in-package :lem-terminal/terminal-mode)

(define-major-mode terminal-mode ()
    (:name "Terminal"
     :keymap *terminal-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t)
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) nil))

(define-key *terminal-mode-keymap* 'self-insert 'terminal-input)
(define-key *terminal-mode-keymap* 'undefined-key 'terminal-input)

#+(or)
(loop :for code :from 1 :to 255
      :when (graphic-char-p (code-char code))
      :do (define-key *terminal-mode-keymap*
            (princ-to-string (make-key :sym (string (code-char code))))
            'terminal-input)
          (define-key *terminal-mode-keymap*
            (princ-to-string (make-key :sym (string (code-char code)) :ctrl t))
            'terminal-input)
          (define-key *terminal-mode-keymap*
            (princ-to-string (make-key :sym (string (code-char code)) :meta t))
            'terminal-input)
          (define-key *terminal-mode-keymap*
            (princ-to-string (make-key :sym (string (code-char code)) :shift t))
            'terminal-input)
          (define-key *terminal-mode-keymap*
            (princ-to-string (make-key :sym (string (code-char code)) :ctrl t :meta t))
            'terminal-input)
          (define-key *terminal-mode-keymap*
            (princ-to-string (make-key :sym (string (code-char code)) :ctrl t :shift t))
            'terminal-input)
          (define-key *terminal-mode-keymap*
            (princ-to-string (make-key :sym (string (code-char code)) :meta t :shift t))
            'terminal-input)
          (define-key *terminal-mode-keymap*
            (princ-to-string (make-key :sym (string (code-char code)) :ctrl t :meta t :shift t))
            'terminal-input))

(defun buffer-terminal (buffer)
  (buffer-value buffer 'terminal))

(defun (setf buffer-terminal) (terminal buffer)
  (setf (buffer-value buffer 'terminal) terminal))

(defun make-terminal-buffer ()
  (let* ((buffer (make-buffer (unique-buffer-name "*Terminal*")))
         (terminal (terminal:create :cols 80 :rows 24 :buffer buffer)))
    (setf (buffer-terminal buffer) terminal)
    (change-buffer-mode buffer 'terminal-mode)
    (add-hook (variable-value 'kill-buffer-hook :buffer buffer) 'on-kill-buffer)
    buffer))

(defun on-kill-buffer (buffer)
  (let ((terminal (buffer-terminal buffer)))
    (when terminal
      (terminal:destroy terminal))))

(define-command terminal () ()
  (let* ((buffer (make-terminal-buffer))
         (window (pop-to-buffer buffer)))
    (resize-terminal (buffer-terminal buffer) window)))

(defun get-current-terminal ()
  (let ((terminal (buffer-terminal (current-buffer))))
    (assert terminal)
    terminal))

(define-command terminal-input () ()
  (let ((terminal (get-current-terminal))
        (keyseq (last-read-key-sequence)))
    (dolist (key keyseq)
      (let ((mod (logior (if (key-ctrl key) ffi::vterm_mod_ctrl 0)
                         (if (key-meta key) ffi::vterm_mod_alt 0)
                         (if (key-shift key) ffi::vterm_mod_shift 0))))
        (alexandria:switch ((key-sym key) :test #'equal)
          ("Return" (terminal:input-key terminal ffi::vterm_key_enter :mod mod))
          ("Backspace" (terminal:input-key terminal ffi::vterm_key_backspace :mod mod))
          ("Tab" (terminal:input-key terminal ffi::vterm_key_tab :mod mod))
          ("Escape" (terminal:input-key terminal ffi::vterm_key_escape :mod mod))
          ("Up" (terminal:input-key terminal ffi::vterm_key_up :mod mod))
          ("Down" (terminal:input-key terminal ffi::vterm_key_down :mod mod))
          ("Left" (terminal:input-key terminal ffi::vterm_key_left :mod mod))
          ("Right" (terminal:input-key terminal ffi::vterm_key_right :mod mod))
          ("Insert" (terminal:input-key terminal ffi::vterm_key_ins :mod mod))
          ("Delete" (terminal:input-key terminal ffi::vterm_key_del :mod mod))
          ("Home" (terminal:input-key terminal ffi::vterm_key_home :mod mod))
          ("End" (terminal:input-key terminal ffi::vterm_key_end :mod mod))
          ("PageUp" (terminal:input-key terminal ffi::vterm_key_pageup :mod mod))
          ("PageDown" (terminal:input-key terminal ffi::vterm_key_pagedown :mod mod))
          ("F1" (terminal:input-key terminal (+ 1 ffi::vterm_key_function_0) :mod mod))
          ("F2" (terminal:input-key terminal (+ 2 ffi::vterm_key_function_0) :mod mod))
          ("F3" (terminal:input-key terminal (+ 3 ffi::vterm_key_function_0) :mod mod))
          ("F4" (terminal:input-key terminal (+ 4 ffi::vterm_key_function_0) :mod mod))
          ("F5" (terminal:input-key terminal (+ 5 ffi::vterm_key_function_0) :mod mod))
          ("F6" (terminal:input-key terminal (+ 6 ffi::vterm_key_function_0) :mod mod))
          ("F7" (terminal:input-key terminal (+ 7 ffi::vterm_key_function_0) :mod mod))
          ("F8" (terminal:input-key terminal (+ 8 ffi::vterm_key_function_0) :mod mod))
          ("F9" (terminal:input-key terminal (+ 9 ffi::vterm_key_function_0) :mod mod))
          ("F10" (terminal:input-key terminal (+ 10 ffi::vterm_key_function_0) :mod mod))
          ("F11" (terminal:input-key terminal (+ 11 ffi::vterm_key_function_0) :mod mod))
          ("F12" (terminal:input-key terminal (+ 12 ffi::vterm_key_function_0) :mod mod))
          ("Space" (terminal:input-character terminal #\Space :mod mod))
          (otherwise
           (when (= 1 (length (key-sym key)))
             (terminal:input-character terminal (char (key-sym key) 0) :mod mod))))))))

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

(defmethod execute ((mode terminal-mode) command argment)
  (typecase command
    ((or next-window
         previous-window
         split-active-window-vertically
         split-active-window-horizontally
         delete-other-windows
         delete-active-window
         select-buffer
         kill-buffer
         find-file
         execute-command
         terminal-resize)
     (call-next-method))
    (otherwise
     (terminal-input))))

(defun resize-terminal (terminal window)
  (terminal:resize terminal
                   :rows (1- (window-height window))
                   :cols (1- (window-width window))))

(define-command terminal-resize () ()
  (let ((terminal (get-current-terminal))
        (window (current-window)))
    (resize-terminal terminal window)))

(defun on-window-size-change (window)
  (alexandria:when-let (terminal (buffer-terminal (window-buffer window)))
    (resize-terminal terminal window)))

(add-hook *window-size-change-functions*
          'on-window-size-change)

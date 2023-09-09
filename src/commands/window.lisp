(defpackage :lem-core/commands/window
  (:use :cl
        :lem-core
        :lem-core/commands/move)
  (:export :select-buffer
           :kill-buffer
           :previous-buffer
           :next-buffer
           :recenter
           :split-active-window-vertically
           :split-active-window-horizontally
           :next-window
           :window-move-up
           :window-move-down
           :window-move-right
           :window-move-left
           :delete-other-windows
           :delete-active-window
           :quit-active-window
           :grow-window
           :shrink-window
           :grow-window-horizontally
           :shrink-window-horizontally
           :scroll-down
           :scroll-up
           :find-file-next-window
           :read-file-next-window
           :select-buffer-next-window
           :switch-to-last-focused-window
           :compare-windows))
(in-package :lem-core/commands/window)

(define-key *global-keymap* "C-x b" 'select-buffer)
(define-key *global-keymap* "C-x k" 'kill-buffer)
(define-key *global-keymap* "C-x Left" 'previous-buffer)
(define-key *global-keymap* "C-x Right" 'next-buffer)
(define-key *global-keymap* "C-l" 'recenter)
(define-key *global-keymap* "C-x 2" 'split-active-window-vertically)
(define-key *global-keymap* "C-x 3" 'split-active-window-horizontally)
(define-key *global-keymap* "C-x o" 'next-window)
(define-key *global-keymap* "M-o" 'next-window)
(define-key *global-keymap* "M-O" 'previous-window)
(define-key *global-keymap* "C-x 1" 'delete-other-windows)
(define-key *global-keymap* "C-x 0" 'delete-active-window)
(define-key *global-keymap* "M-q" 'delete-active-window)
(define-key *global-keymap* "C-x ^" 'grow-window)
(define-key *global-keymap* "C-x C-z" 'shrink-window)
(define-key *global-keymap* "C-x }" 'grow-window-horizontally)
(define-key *global-keymap* "C-x {" 'shrink-window-horizontally)
(define-key *global-keymap* "C-Down" 'scroll-down)
(define-key *global-keymap* "M-Down" 'scroll-down)
(define-key *global-keymap* "C-Up" 'scroll-up)
(define-key *global-keymap* "M-Up" 'scroll-up)
(define-key *global-keymap* "C-x 4 f" 'find-file-next-window)
(define-key *global-keymap* "C-x 4 r" 'read-file-next-window)
(define-key *global-keymap* "C-x 4 b" 'select-buffer-next-window)
(define-key *global-keymap* "C-x 4 p f" 'project-find-file-next-window)

(defvar *balance-after-split-window* t)

(defun maybe-balance-windows ()
  (when *balance-after-split-window*
    (balance-windows)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-next-window-command (command prompt documentation)
    (if (exist-command-p (string-downcase command))
        `(define-command ,(intern (format nil "~a-NEXT-WINDOW"
                                          (string-upcase command)))
             (arg) (,prompt)
           ,documentation
           (when (one-window-p)
             (split-window-sensibly (current-window))
             (maybe-balance-windows))
           (next-window)
           (,command arg))
        (warn "command ~a is not defined." command)))

  (defun maybe-create-buffer (name)
    (when (prompt-for-y-or-n-p
           (format nil "Buffer ~a does not exist. Create" name))
      (make-buffer name)))

  (define-command select-buffer (name) ("BUse Buffer: ")
    "Switches to the selected buffer."
    (let ((buffer (or (get-buffer name)
                      (maybe-create-buffer name)
                      (error 'editor-abort))))
      (switch-to-buffer buffer))
    t))

(defun strip-buffer-from-frame-windows (buffer frame)
  (dolist (window (get-buffer-windows buffer :frame frame :include-floating-windows t))
    (with-current-window window
      (switch-to-buffer (or (get-previous-buffer buffer)
                            (first (last (buffer-list))))))))

(defmethod delete-buffer-using-manager :before
    ((manager lem-base::buffer-list-manager)
     buffer)
  (dolist (frame (all-frames))
    (strip-buffer-from-frame-windows buffer frame)))

(define-command kill-buffer (buffer-or-name) ("bKill buffer: ")
  "Delete buffer."
  (let ((buffer (get-buffer buffer-or-name)))
    (unless buffer
      (editor-error "buffer does not exist: ~A" buffer-or-name))
    (when (cdr (buffer-list))
      (delete-buffer buffer)))
  t)

(define-command previous-buffer () ()
  "Switches to the previous buffer."
  (switch-to-buffer
   (if (eq (current-buffer) (car (buffer-list)))
       (alexandria:lastcar (buffer-list))
       (loop :for rest :on (buffer-list)
             :do (when (eq (cadr rest) (current-buffer))
                   (return (car rest)))))
   nil))

(define-command next-buffer () ()
  "Switches to the next buffer."
  (switch-to-buffer (or (cadr (member (current-buffer) (buffer-list)))
                        (car (buffer-list)))
                    nil))

(define-command recenter (p) ("P")
  "Scroll so that the cursor is in the middle."
  (clear-screens-of-window-list)
  (unless p (window-recenter (current-window)))
  (redraw-display)
  t)

(define-command split-active-window-vertically (&optional n) ("P")
  "Split the current window vertically."
  (split-window-vertically (current-window) :height n)
  (unless n
    (maybe-balance-windows)))

(define-command split-active-window-horizontally (&optional n) ("P")
  "Split the current window horizontally."
  (split-window-horizontally (current-window) :width n)
  (unless n
    (maybe-balance-windows)))

(define-command next-window (&optional (n 1)) ("p")
  "Go to the next window."
  (let ((window-list
          (compute-window-list (current-window))))
    (when (minusp n)
      (setf n (- (length window-list) (abs n))))
    (let ((window (current-window)))
      (dotimes (_ n t)
        (setf window
              (get-next-window window window-list)))
      (switch-to-window window))))

(define-command previous-window (&optional (n 1)) ("p")
  (next-window (- n)))

(define-command switch-to-last-focused-window () ()
  "Go to the window that was last in focus."
  (let ((window (or (and (not (null (last-focused-window)))
                         (not (deleted-window-p (last-focused-window)))
                         (last-focused-window))
                    (get-next-window (current-window)))))
    (switch-to-window window)))

(define-command window-move-down () ()
  "Go to the window on the down."
  (alexandria:when-let ((window (down-window (current-window))))
    (switch-to-window window)))

(define-command window-move-up () ()
  "Go to the window on the up."
  (alexandria:when-let ((window (up-window (current-window))))
    (switch-to-window window)))

(define-command window-move-right () ()
  "Go to the window on the right."
  (alexandria:when-let ((window (right-window (current-window))))
    (switch-to-window window)))

(define-command window-move-left () ()
  "Go to the window on the left."
  (alexandria:when-let ((window (left-window (current-window))))
    (switch-to-window window)))

(define-command delete-other-windows () ()
  "Delete all other windows."
  (dolist (win (window-list))
    (unless (eq win (current-window))
      (delete-window win)))
  (window-set-pos (current-window)
                  (topleft-window-x (current-frame))
                  (topleft-window-y (current-frame)))
  (window-set-size (current-window)
                   (max-window-width (current-frame))
                   (max-window-height (current-frame)))
  t)

(define-command delete-active-window () ()
  "Delete the active window."
  (delete-window (current-window))
  (maybe-balance-windows))

(define-command quit-active-window (&optional kill-buffer) ("P")
  "Quit the active window. This is a command for a popped-up window."
  (quit-window (current-window)
               :kill-buffer kill-buffer))

(define-command grow-window (n) ("p")
  "Grow the window's height."
  (when (< n 0)
    (return-from grow-window (shrink-window (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (grow-window-height (current-window) n))

(define-command shrink-window (n) ("p")
  "Shrink the window's height."
  (when (< n 0)
    (return-from shrink-window (grow-window (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (shrink-window-height (current-window) n))

(define-command grow-window-horizontally (n) ("p")
  "Grow the window's width."
  (when (< n 0)
    (return-from grow-window-horizontally (shrink-window-horizontally (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (grow-window-width (current-window) n))

(define-command shrink-window-horizontally (n) ("p")
  "Shrink the window's width."
  (when (< n 0)
    (return-from shrink-window-horizontally (grow-window-horizontally (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (shrink-window-width (current-window) n))

(defmethod lem-core:scroll (window n)
  (scroll-down n window))

(define-command scroll-down (n &optional (window (current-window))) ("p")
  "Scroll down."
  (cond
    ((zerop n))
    ((minusp n)
     (scroll-up (- n)))
    (t
     (unless (window-scroll window n)
       (buffer-end (window-view-point window))
       (backward-line-wrap (window-view-point window)
                           window t))
     (with-current-window window
       (next-line (- (window-offset-view window)))))))

(define-command scroll-up (n &optional (window (current-window))) ("p")
  "Scroll up."
  (cond
    ((zerop n))
    ((minusp n)
     (scroll-down (- n)))
    (t
     (unless (window-scroll window (- n))
       (buffer-start (window-view-point window)))
     (with-current-window window
       (previous-line (window-offset-view window))))))

(define-next-window-command lem-core/commands/file:find-file "FFind File Other Window: " "Open a file in another window. Split the screen vertically if needed.")
(define-next-window-command lem-core/commands/file:read-file "FREAD File Other Window: " "Read a file in another window.")
(define-next-window-command lem-core/commands/window:select-buffer "BUse Buffer Other Window: " "Select a buffer in another window.")
(define-next-window-command lem-core/commands/project:project-find-file "pFind File in Project in Other Window: " "Open a file from the current project in another window. Split the screen vertically if needed.")
(define-next-window-command lem-core/commands/project:project-root-directory "pOpen Project Directory in Other Window: " "Open this project directory in another window. Split the screen vertically if needed.")

(define-command compare-windows (ignore-whitespace) ("p")
  (setf ignore-whitespace (/= ignore-whitespace 1))
  (when (one-window-p)
    (editor-error "Separate window for compare-windows."))
  (flet ((next-char (p)
           (loop
             :for c := (character-at p)
             :do (when (not (and ignore-whitespace
                                 (syntax-space-char-p c)))
                   (return c))
                 (unless (character-offset p 1)
                   (return nil)))))
    (loop :with window1 := (current-window)
          :with window2 := (get-next-window window1)
          :with p1 := (window-point window1)
          :with p2 := (window-point window2)
          :for c1 := (next-char p1)
          :for c2 := (next-char p2)
          :until (or (null c1)
                     (null c2)
                     (not (eql c1 c2)))
          :while (and (character-offset p1 1)
                      (character-offset p2 1)))))

(define-command toggle-line-wrap () ()
  "Toggle line wrap in the buffer."
  (setf (variable-value 'line-wrap) (not (variable-value 'line-wrap))))

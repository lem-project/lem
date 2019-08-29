(in-package :lem)

(export '(select-buffer
          kill-buffer
          previous-buffer
          next-buffer
          recenter
          split-active-window-vertically
          split-active-window-horizontally
          other-window
          window-move-up
          window-move-down
          window-move-right
          window-move-left
          delete-other-windows
          delete-current-window
          quit-window
          grow-window
          shrink-window
          grow-window-horizontally
          shrink-window-horizontally
          display-buffer
          scroll-down
          scroll-up
          find-file-other-window
          read-file-other-window
          select-buffer-other-window
          switch-to-last-focused-window))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-other-window-command (command prompt)
    (if (exist-command-p (string-downcase command))
        `(define-command ,(intern (format nil "~a-OTHER-WINDOW"
                                          (string-upcase command)))
           (arg) (,prompt)
           (if (one-window-p)
               (split-window-sensibly (current-window)))
           (other-window)
           (,command arg))
        (warn "command ~a is not defined." command)))

  (define-command select-buffer (name) ("BUse Buffer: ")
    (check-switch-minibuffer-window)
    (switch-to-buffer (make-buffer name))
    t))

(define-key *global-keymap* "C-x b" 'select-buffer)

(define-key *global-keymap* "C-x k" 'kill-buffer)
(define-command kill-buffer (buffer-or-name) ("bKill buffer: ")
  (check-switch-minibuffer-window)
  (let ((buffer (get-buffer buffer-or-name)))
    (unless buffer
      (editor-error "buffer does not exist: ~A" buffer-or-name))
    (when (cdr (buffer-list))
      (dolist (window (get-buffer-windows buffer))
        (with-current-window window
          (switch-to-buffer (or (get-previous-buffer buffer)
                                (car (last (buffer-list)))))))
      (delete-buffer buffer)))
  t)

(define-key *global-keymap* "C-x Left" 'previous-buffer)
(define-command previous-buffer () ()
  (switch-to-buffer
   (if (eq (current-buffer) (car (buffer-list)))
       (alexandria:lastcar (buffer-list))
       (loop :for rest :on (buffer-list)
             :do (when (eq (cadr rest) (current-buffer))
                   (return (car rest)))))
   nil))

(define-key *global-keymap* "C-x Right" 'next-buffer)
(define-command next-buffer () ()
  (switch-to-buffer (or (cadr (member (current-buffer) (buffer-list)))
                        (car (buffer-list)))
                    nil))

(define-key *global-keymap* "C-l" 'recenter)
(define-command recenter (p) ("P")
  (dolist (window (window-list))
    (screen-clear (window-screen window)))
  (unless p (window-recenter (current-window)))
  (redraw-display)
  t)

(define-key *global-keymap* "C-x 2" 'split-active-window-vertically)
(define-command split-active-window-vertically (&optional n) ("P")
  (split-window-vertically (current-window) n))

(define-key *global-keymap* "C-x 3" 'split-active-window-horizontally)
(define-command split-active-window-horizontally (&optional n) ("P")
  (split-window-horizontally (current-window) n))

(defvar *last-focused-window-id* nil)

(defun update-last-focused-window ()
  (setf *last-focused-window-id* (window-id (current-window))))

(define-key *global-keymap* "C-x o" 'other-window)
(define-command other-window (&optional (n 1)) ("p")
  (let ((window-list
          (append (alexandria:ensure-list
                   (active-minibuffer-window))
                  (window-list))))
    (when (minusp n)
      (setf n (- (length window-list) (abs n))))
    (update-last-focused-window)
    (dotimes (_ n t)
      (setf (current-window)
            (get-next-window (current-window)
                             window-list)))))

(define-key *global-keymap* "M-o" 'other-window-or-split-window)
(define-command other-window-or-split-window (&optional (n 1)) ("p")
  (when (one-window-p)
    (split-window-sensibly (current-window)))
  (other-window n))

(define-command switch-to-last-focused-window () ()
  (let ((window (or (and *last-focused-window-id*
                         (find-window *last-focused-window-id*))
                    (get-next-window (current-window)))))
    (update-last-focused-window)
    (setf (current-window) window)))

(define-command window-move-down () ()
  (alexandria:when-let ((window (down-window (current-window))))
    (setf (current-window) window)))

(define-command window-move-up () ()
  (alexandria:when-let ((window (up-window (current-window))))
    (setf (current-window) window)))

(define-command window-move-right () ()
  (alexandria:when-let ((window (right-window (current-window))))
    (setf (current-window) window)))

(define-command window-move-left () ()
  (alexandria:when-let ((window (left-window (current-window))))
    (setf (current-window) window)))

(define-key *global-keymap* "C-x 1" 'delete-other-windows)
(define-command delete-other-windows () ()
  (unless (minibuffer-window-active-p)
    (dolist (win (window-list))
      (unless (eq win (current-window))
        (delete-window win)))
    (window-set-pos (current-window) (window-topleft-x) (window-topleft-y))
    (window-set-size (current-window)
                     (window-max-width)
                     (window-max-height))
    t))

(define-key *global-keymap* "C-x 0" 'delete-current-window)
(define-command delete-current-window () ()
  (delete-window (current-window)))

(define-command quit-window (&optional window kill-buffer-p) ("P")
  (when (null window)
    (setf window (current-window)))
  (let ((parent-window (window-parameter window 'parent-window)))
    (cond
      ((and (not (one-window-p))
            (window-parameter window 'split-p))
       (if kill-buffer-p
           (kill-buffer (window-buffer window))
           (bury-buffer (window-buffer window)))
       (delete-window window)
       (unless (deleted-window-p parent-window)
         (setf (current-window) parent-window)))
      (t
       (if kill-buffer-p
           (kill-buffer (window-buffer window))
           (switch-to-buffer (bury-buffer (window-buffer window)) nil))
       (unless (deleted-window-p parent-window)
         (setf (current-window) parent-window))))))

(define-key *global-keymap* "C-x ^" 'grow-window)
(define-command grow-window (n) ("p")
  (when (< n 0)
    (return-from grow-window (shrink-window (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-internal x y n))
                           :vsplit))

(define-key *global-keymap* "C-x C-z" 'shrink-window)
(define-command shrink-window (n) ("p")
  (when (< n 0)
    (return-from shrink-window (grow-window (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-internal y x n))
                           :vsplit))

(define-key *global-keymap* "C-x }" 'grow-window-horizontally)
(define-command grow-window-horizontally (n) ("p")
  (when (< n 0)
    (return-from grow-window-horizontally (shrink-window-horizontally (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-horizontally-internal x y n))
                           :hsplit))

(define-key *global-keymap* "C-x {" 'shrink-window-horizontally)
(define-command shrink-window-horizontally (n) ("p")
  (when (< n 0)
    (return-from shrink-window-horizontally (grow-window-horizontally (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-horizontally-internal y x n))
                           :hsplit))

(defun display-buffer (buffer &optional force-split-p)
  (multiple-value-bind (window split-p)
      (pop-to-buffer buffer force-split-p)
    (declare (ignore split-p))
    window))

(define-key *global-keymap* "C-Down" 'scroll-down)
(define-key *global-keymap* "M-Down" 'scroll-down)
(define-command scroll-down (n) ("p")
  (cond
    ((minusp n)
     (scroll-up (- n)))
    (t
     (window-scroll (current-window) n)
     (let ((offset (window-offset-view (current-window))))
       (unless (zerop offset)
         (line-offset (current-point) (- offset)))))))

(define-key *global-keymap* "C-Up" 'scroll-up)
(define-key *global-keymap* "M-Up" 'scroll-up)
(define-command scroll-up (n) ("p")
  (cond
    ((minusp n)
     (scroll-down (- n)))
    (t
     (window-scroll (current-window) (- n))
     (let ((offset (window-offset-view (current-window))))
       (unless (zerop offset)
         (line-offset (current-point) (- offset)))))))

(define-other-window-command find-file "FFind File Other Window: ")
(define-key *global-keymap* "C-x 4 f" 'find-file-other-window)

(define-other-window-command read-file "FREAD File Other Window: ")
(define-key *global-keymap* "C-x 4 r" 'read-file-other-window)

(define-other-window-command select-buffer "BUse Buffer Other Window: ")
(define-key *global-keymap* "C-x 4 b" 'select-buffer-other-window)

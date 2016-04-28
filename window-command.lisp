(in-package :lem)

(export '(select-buffer
          kill-buffer
          recenter
          split-active-window-vertically
          split-active-window-horizontally
          other-window
          delete-other-windows
          delete-current-window
          quit-window
          grow-window
          shrink-window
          grow-window-horizontally
          shrink-window-horizontally
          pop-to-buffer
          display-buffer
          scroll-down
          scroll-up))

(define-key *global-keymap* (kbd "C-x b") 'select-buffer)
(define-command select-buffer (name) ("BUse Buffer: ")
  (check-switch-minibuffer-window)
  (set-buffer (get-buffer-create name))
  t)

(define-key *global-keymap* (kbd "C-x k") 'kill-buffer)
(define-command kill-buffer (buffer-or-name) ("bKill buffer: ")
  (check-switch-minibuffer-window)
  (let ((buffer (get-buffer buffer-or-name)))
    (when (cdr (buffer-list))
      (dolist (window (get-buffer-windows buffer))
        (with-current-window window
          (set-buffer (get-next-buffer (current-buffer)))))
      (delete-buffer buffer)))
  t)

(define-key *global-keymap* (kbd "C-l") 'recenter)
(define-command recenter () ()
  (dolist (window (window-list))
    (screen-clear (window-screen window)))
  (window-recenter (current-window))
  (redraw-display)
  t)

(define-key *global-keymap* (kbd "C-x 2") 'split-active-window-vertically)
(define-command split-active-window-vertically () ()
  (split-window-vertically (current-window)))

(define-key *global-keymap* (kbd "C-x 3") 'split-active-window-horizontally)
(define-command split-active-window-horizontally () ()
  (split-window-horizontally (current-window)))

(define-key *global-keymap* (kbd "C-x o") 'other-window)
(define-command other-window (&optional (n 1)) ("p")
  (let ((window-list
          (append (mklist (active-minibuffer-window))
                  (window-list))))
    (when (minusp n)
      (setf n (- (length window-list) (abs n))))
    (dotimes (_ n t)
      (setf (current-window)
            (get-next-window (current-window)
                             window-list)))))

(define-key *global-keymap* (kbd "C-x 1") 'delete-other-windows)
(define-command delete-other-windows () ()
  (unless (minibuffer-window-active-p)
    (dolist (win (window-list))
      (unless (eq win (current-window))
        (delete-window win)))
    (window-set-pos (current-window) 0 0)
    (window-set-size (current-window)
                     (window-max-width)
                     (window-max-height))
    t))

(define-key *global-keymap* (kbd "C-x 0") 'delete-current-window)
(define-command delete-current-window () ()
  (delete-window (current-window)))

(define-command quit-window (&optional window kill-buffer-p) ("P")
  (unless (window-p window)
    (setq window (current-window)))
  (cond
   ((window-parameter window :split-p)
    (when kill-buffer-p
      (kill-buffer (window-buffer window)))
    (delete-window window))
   (t
    (if kill-buffer-p
        (kill-buffer (window-buffer window))
        (set-buffer (bury-buffer (window-buffer window))
                    nil)))))

(define-key *global-keymap* (kbd "C-x ^") 'grow-window)
(define-command grow-window (n) ("p")
  (when (< n 0)
    (return-from grow-window (shrink-window (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-internal x y n))
                           :vsplit))

(define-key *global-keymap* (kbd "C-x C-z") 'shrink-window)
(define-command shrink-window (n) ("p")
  (when (< n 0)
    (return-from shrink-window (grow-window (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-internal y x n))
                           :vsplit))

(define-key *global-keymap* (kbd "C-x }") 'grow-window-horizontally)
(define-command grow-window-horizontally (n) ("p")
  (when (< n 0)
    (return-from grow-window-horizontally (shrink-window-horizontally (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-horizontally-internal x y n))
                           :hsplit))

(define-key *global-keymap* (kbd "C-x {") 'shrink-window-horizontally)
(define-command shrink-window-horizontally (n) ("p")
  (when (< n 0)
    (return-from shrink-window-horizontally (grow-window-horizontally (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-horizontally-internal y x n))
                           :hsplit))

(defun pop-to-buffer (buffer)
  (check-switch-minibuffer-window)
  (if (eq buffer (current-buffer))
      (values (current-window) nil)
      (let ((split-p))
        (when (one-window-p)
          (setq split-p t)
          (split-window-sensibly (current-window)))
        (with-current-window (or (window-tree-find
                                  (window-tree)
                                  #'(lambda (window)
                                      (eq buffer (window-buffer window))))
                                 (get-next-window (current-window)))
          (set-buffer buffer)
          (values (current-window) split-p)))))

(defun display-buffer (buffer)
  (multiple-value-bind (window split-p)
      (pop-to-buffer buffer)
    (setf (window-parameter window :split-p) split-p)
    window))

(define-key *global-keymap* (kbd "C-down") 'scroll-down)
(define-command scroll-down (n) ("p")
  (if (minusp n)
      (scroll-up (- n))
      (dotimes (_ n t)
        (when (= (window-cursor-y (current-window)) 0)
          (unless (forward-line n)
            (return nil)))
        (let ((prev-linum (window-vtop-linum (current-window)))
              (prev-charpos (window-vtop-charpos (current-window))))
          (window-scroll (current-window) 1)
          (when (and (= prev-linum (window-vtop-linum (current-window)))
                     (= prev-charpos (window-vtop-charpos (current-window))))
            (return nil))))))

(define-key *global-keymap* (kbd "C-up") 'scroll-up)
(define-command scroll-up (n) ("p")
  (if (minusp n)
      (scroll-down (- n))
      (dotimes (_ n t)
        (when (and (= (window-cursor-y (current-window))
                      (- (window-height (current-window)) 2))
                   (/= 1 (window-vtop-linum (current-window))))
          (unless (forward-line (- n))
            (return nil)))
        (let ((prev-linum (window-vtop-linum (current-window)))
              (prev-charpos (window-vtop-charpos (current-window))))
          (window-scroll (current-window) -1)
          (when (and (= prev-linum (window-vtop-linum (current-window)))
                     (= prev-charpos (window-vtop-charpos (current-window))))
            (return nil))))))

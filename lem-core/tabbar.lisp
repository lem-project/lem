(in-package :lem)

(defstruct tabbar
  buffer
  window
  (prev-buffer-list '())
  (prev-current-buffer nil)
  (prev-display-width 0))

(defvar *tabbar* nil)

(defun tabbar-init ()
  (let* ((buffer (make-buffer " *tabbar*" :enable-undo-p nil))
         (window (make-floating-window buffer 0 0 (display-width) 1 nil)))
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (setf *tabbar*
          (make-tabbar :buffer buffer
                       :window window))))

(defun tabbar-require-update ()
  (block exit
    (unless (eq (current-buffer) (tabbar-prev-current-buffer *tabbar*))
      (return-from exit t))
    (unless (eq (buffer-list) (tabbar-prev-buffer-list *tabbar*))
      (return-from exit t))
    (unless (= (display-width) (tabbar-prev-display-width *tabbar*))
      (window-set-size (tabbar-window *tabbar*) (display-width) 1)
      (return-from exit t))
    nil))

(defun tabbar-draw ()
  (when (tabbar-require-update)
    (let* ((buffer (tabbar-buffer *tabbar*))
           (p (buffer-point buffer)))
      (erase-buffer buffer)
      (dolist (buffer (buffer-list))
        (insert-string p
                       (let ((name (buffer-name buffer)))
                         (if (< 20 (length name))
                             (format nil "[~A...]" (subseq name 0 17))
                             (format nil "[~A]" name)))
                       :attribute (if (eq buffer (current-buffer))
                                      'tabbar-active-tab-attribute
                                      'tabbar-attribute)))
      (let ((n (- (display-width) (point-column p))))
        (when (> n 0)
          (insert-string p (make-string n :initial-element #\space)
                         :attribute 'tabbar-attribute)))))
  (setf (tabbar-prev-buffer-list *tabbar*) (buffer-list))
  (setf (tabbar-prev-current-buffer *tabbar*) (current-buffer))
  (setf (tabbar-prev-display-width *tabbar*) (display-width))
  t)
  
(defun tabbar-clear-cache ()
  (setf (tabbar-window *tabbar*) nil)
  (setf (tabbar-buffer *tabbar*) nil)
  (setf (tabbar-prev-buffer-list *tabbar*) '())
  (setf (tabbar-prev-current-buffer *tabbar*) nil)
  (setf (tabbar-prev-display-width *tabbar*) 0))
  
(defun tabbar-off ()
  (when *use-tabbar*
    (setf *use-tabbar* nil)
    (delete-window (tabbar-window *tabbar*))
    (tabbar-clear-cache)
    (change-display-size-hook)))
  
(defun tabbar-on ()
  (unless *use-tabbar*
    (tabbar-init)
    (setf *use-tabbar* t)
    (change-display-size-hook)))

(defun enable-tabbar-p ()
  *use-tabbar*)

(define-command toggle-tabbar () ()
  (if (enable-tabbar-p)
      (tabbar-off)
      (tabbar-on)))

(define-key *global-keymap* (list (code-char 550)) 'tabbar-next) ; control + pagedown
(define-key *global-keymap* (list (code-char 555)) 'tabbar-prev) ; control + pageup

(define-command tabbar-next (n) ("p")
  (dotimes (_ n)
    (alexandria:when-let (buffer (get-next-buffer (current-buffer)))
      (switch-to-buffer buffer nil))))

(define-command tabbar-prev (n) ("p")
  (dotimes (_ n)
    (alexandria:when-let (buffer (get-previous-buffer (current-buffer)))
      (switch-to-buffer buffer nil))))

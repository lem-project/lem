(in-package :lem)

(defvar *balance-after-split-window* t)

(defun maybe-balance-windows ()
  (when *balance-after-split-window*
    (balance-windows)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-other-window-command (command prompt)
    (if (exist-command-p (string-downcase command))
        `(define-command ,(intern (format nil "~a-OTHER-WINDOW"
                                          (string-upcase command)))
             (arg) (,prompt)
           (when (one-window-p)
             (split-window-sensibly (current-window))
             (maybe-balance-windows))
           (other-window)
           (,command arg))
        (warn "command ~a is not defined." command)))

  (defun maybe-create-buffer (name)
    (when (prompt-for-y-or-n-p
           (format nil "Buffer ~a does not exist. Create" name))
      (make-buffer name)))

  (define-command select-buffer (name) ("BUse Buffer: ")
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
  (let ((buffer (get-buffer buffer-or-name)))
    (unless buffer
      (editor-error "buffer does not exist: ~A" buffer-or-name))
    (when (cdr (buffer-list))
      (delete-buffer buffer)))
  t)

(define-command previous-buffer () ()
  (switch-to-buffer
   (if (eq (current-buffer) (car (buffer-list)))
       (alexandria:lastcar (buffer-list))
       (loop :for rest :on (buffer-list)
             :do (when (eq (cadr rest) (current-buffer))
                   (return (car rest)))))
   nil))

(define-command next-buffer () ()
  (switch-to-buffer (or (cadr (member (current-buffer) (buffer-list)))
                        (car (buffer-list)))
                    nil))

(define-command recenter (p) ("P")
  (clear-screens-of-window-list)
  (unless p (window-recenter (current-window)))
  (redraw-display)
  t)

(define-command split-active-window-vertically (&optional n) ("P")
  (split-window-vertically (current-window) :height n)
  (unless n
    (maybe-balance-windows)))

(define-command split-active-window-horizontally (&optional n) ("P")
  (split-window-horizontally (current-window) :width n)
  (unless n
    (maybe-balance-windows)))

(defvar *last-focused-window* nil)

(defun update-last-focused-window ()
  (setf *last-focused-window* (current-window)))

(defmethod compute-window-list (current-window)
  (append (alexandria:ensure-list
           (active-prompt-window))
          (window-list)))

(define-command other-window (&optional (n 1)) ("p")
  (let ((window-list
          (compute-window-list (current-window))))
    (when (minusp n)
      (setf n (- (length window-list) (abs n))))
    (update-last-focused-window)
    (run-hooks (window-leave-hook (current-window)))
    (dotimes (_ n t)
      (setf (current-window)
            (get-next-window (current-window)
                             window-list)))))

(define-command switch-to-last-focused-window () ()
  (let ((window (or (and (not (null *last-focused-window*))
                         (not (deleted-window-p *last-focused-window*))
                         *last-focused-window*)
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

(define-command delete-other-windows () ()
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
  (delete-window (current-window))
  (maybe-balance-windows))

(define-command quit-active-window (&optional kill-buffer) ("P")
  (quit-window (current-window)
               :kill-buffer kill-buffer))

(define-command grow-window (n) ("p")
  (when (< n 0)
    (return-from grow-window (shrink-window (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (grow-window-height (current-window) n))

(define-command shrink-window (n) ("p")
  (when (< n 0)
    (return-from shrink-window (grow-window (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (shrink-window-height (current-window) n))

(define-command grow-window-horizontally (n) ("p")
  (when (< n 0)
    (return-from grow-window-horizontally (shrink-window-horizontally (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (grow-window-width (current-window) n))

(define-command shrink-window-horizontally (n) ("p")
  (when (< n 0)
    (return-from shrink-window-horizontally (grow-window-horizontally (- n))))
  (when (one-window-p)
    (editor-error "Only one window"))
  (shrink-window-width (current-window) n))

(defun display-buffer (buffer &optional force-split-p)
  (multiple-value-bind (window split-p)
      (pop-to-buffer buffer force-split-p)
    (declare (ignore split-p))
    window))

(define-command scroll-down (n) ("p")
  (cond
    ((minusp n)
     (scroll-up (- n)))
    (t
     (unless (window-scroll (current-window) n)
       (buffer-end (window-view-point (current-window)))
       (backward-line-wrap (window-view-point (current-window))
                           (current-window) t))
     (next-line (- (window-offset-view (current-window)))))))

(define-command scroll-up (n) ("p")
  (cond
    ((minusp n)
     (scroll-down (- n)))
    (t
     (unless (window-scroll (current-window) (- n))
       (buffer-start (window-view-point (current-window))))
     (previous-line (window-offset-view (current-window))))))

(define-other-window-command find-file "FFind File Other Window: ")

(define-other-window-command read-file "FREAD File Other Window: ")

(define-other-window-command select-buffer "BUse Buffer Other Window: ")

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

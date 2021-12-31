(in-package :lem)

(export '(*typeout-mode-keymap*
          typeout-mode
          pop-up-typeout-window))

(define-minor-mode typeout-mode
    (:name "typeout"
     :keymap *typeout-mode-keymap*
     :enable-hook (lambda ()
                    (setf (variable-value 'line-wrap :buffer (current-buffer)) nil))))

(define-key *typeout-mode-keymap* "q" 'dismiss-typeout-window)
(define-key *typeout-mode-keymap* "Space" 'next-page-or-dismiss-typeout-window)
(define-key *typeout-mode-keymap* "Backspace" 'previous-page)

(defvar *enable-piece-of-paper* t)

(defvar *typeout-window* nil)
(defvar *typeout-before-window* nil)
(defvar *typeout-window-rewinding-values* nil)

(defun pop-up-typeout-window* (buffer function &key focus erase (read-only t))
  (declare (ignore focus))
  (let ((window (display-buffer buffer)))
    (with-buffer-read-only buffer nil
      (when erase
        (erase-buffer buffer))
      (with-open-stream (out (make-buffer-output-stream (buffer-end-point buffer)))
        (when function
          (save-excursion
            (funcall function out)))
        (format out "~%--- Press Space to continue ---~%")))
    (setf (buffer-read-only-p buffer) read-only)
    (setf *typeout-before-window* (current-window))
    (setf *typeout-window* window)
    (setf (current-window) window)
    (typeout-mode t)
    (values)))

(defun pop-up-typeout-window (buffer function &key focus erase (read-only t))
  (unless *enable-piece-of-paper*
    (return-from pop-up-typeout-window
      (pop-up-typeout-window* buffer function
                              :focus focus
                              :erase erase
                              :read-only read-only)))
  (when (and *typeout-window*
             (not (eq buffer (window-buffer *typeout-window*))))
    (dismiss-typeout-window)
    (redraw-display))
  (with-buffer-read-only buffer nil
    (when erase
      (erase-buffer buffer))
    (when function
      (with-open-stream (out (make-buffer-output-stream (buffer-end-point buffer)))
        (funcall function out)
        (fresh-line out))))
  (when read-only
    (setf (buffer-read-only-p buffer) t))
  (let* ((window-height
           (min (floor (display-height) 2) (buffer-nlines buffer)))
         (window
           (cond (*typeout-window*
                  (lem::window-set-size *typeout-window* (display-width) window-height)
                  *typeout-window*)
                 (t
                  (let ((typeout-buffer-p (buffer-value buffer 'typeout-buffer-p))
                        (not-switchable-buffer-p (not-switchable-buffer-p buffer)))
                    (setf *typeout-window-rewinding-values*
                          (list typeout-buffer-p
                                not-switchable-buffer-p)))
                  (let ((window (make-floating-window :buffer buffer
                                                      :x 0
                                                      :y 0
                                                      :width (display-width)
                                                      :height window-height
                                                      :use-modeline-p t)))
                    (setf (window-modeline-format window) '(typeout-window-modeline))
                    (add-hook (window-delete-hook window)
                              'delete-typeout-window-hook)
                    (add-hook (window-switch-to-buffer-hook window)
                              'typeout-window-switch-to-buffer-hook)
                    (setf *typeout-window* window
                          *typeout-before-window* (current-window))
                    window)))))
    (setf (buffer-value buffer 'typeout-buffer-p) t)
    (setf (not-switchable-buffer-p buffer) t)
    (bury-buffer buffer)
    (setf (current-window) window)
    (typeout-mode t)
    (redraw-display)
    (values)))

(defun typeout-window-modeline (typeout-window)
  (values (let* ((posline (string-trim " " (modeline-posline typeout-window)))
                 (text (cond ((member posline '("All" "Bot") :test #'string=)
                              "Press Space to continue")
                             (t posline)))
                 (line (concatenate 'string
                                    (make-string (- (floor (display-width) 2)
                                                    (floor (length text) 2)
                                                    1)
                                                 :initial-element #\_)
                                    " "
                                    text
                                    " "))
                 (line (concatenate 'string
                                    line
                                    (make-string (- (display-width) (length text))
                                                 :initial-element #\_))))
            line)
          (make-attribute)
          nil))

(defun delete-typeout-window-hook ()
  (setf *typeout-window* nil
        *typeout-before-window* nil))

(defun typeout-window-switch-to-buffer-hook (&rest args)
  (declare (ignore args))
  (dismiss-typeout-window))

(define-command dismiss-typeout-window () ()
  (if *enable-piece-of-paper*
      (dismiss-typeout-window-1)
      (dismiss-typeout-window-2)))

(defun dismiss-typeout-window-1 ()
  (unless (deleted-window-p *typeout-window*)
    (setf (current-window) *typeout-window*)
    (typeout-mode nil)
    (when (deleted-window-p *typeout-before-window*)
      (setf *typeout-before-window* (first (window-list))))
    (setf (current-window) *typeout-before-window*)
    (when *typeout-window-rewinding-values*
      (let ((buffer (window-buffer *typeout-window*)))
        (destructuring-bind (typeout-buffer-p
                             not-switchable-buffer-p)
            *typeout-window-rewinding-values*
          (setf (buffer-value buffer 'typeout-buffer-p) typeout-buffer-p)
          (setf (not-switchable-buffer-p buffer) not-switchable-buffer-p))))
    (delete-window *typeout-window*)))

(defun dismiss-typeout-window-2 ()
  (when (and (eq (current-buffer) (window-buffer (current-window)))
             (find 'typeout-mode (buffer-minor-modes (current-buffer))))
    (typeout-mode nil) 
    (quit-window)))

(define-command next-page-or-dismiss-typeout-window () ()
  (unless (deleted-window-p *typeout-window*)
    (setf (current-window) *typeout-window*)
    (move-point (current-point) (window-view-point (current-window)))
    (unless (line-offset (current-point) (window-height (current-window)))
      (dismiss-typeout-window))))

(define-condition dismiss-typeout-window-if-getout (after-executing-command) ())
(defmethod handle-signal ((condition dismiss-typeout-window-if-getout))
  (when (and (not (mode-active-p (window-buffer (current-window)) 'typeout-mode))
             *typeout-window*)
    (dismiss-typeout-window)))

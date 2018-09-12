(in-package :lem)

(export '(*typeout-mode-keymap*
          typeout-mode
          pop-up-typeout-window))

(define-minor-mode typeout-mode
    (:name "typeout"
     :keymap *typeout-mode-keymap*)
  (setf (variable-value 'truncate-lines :buffer (current-buffer)) nil))

(define-key *typeout-mode-keymap* "q" 'dismiss-typeout-window)
(define-key *typeout-mode-keymap* "Space" 'next-page-or-dismiss-typeout-window)
(define-key *typeout-mode-keymap* "Backspace" 'previous-page)
(define-key *typeout-mode-keymap* 'other-window 'dismiss-typeout-window)
(define-key *typeout-mode-keymap* 'execute-command 'undefined-key)

(defvar *typeout-window* nil)
(defvar *typeout-before-window* nil)

#|
(defun pop-up-typeout-window (buffer fn &key focus erase (read-only t))
  (let ((window (display-buffer buffer)))
    (with-current-window window
      (with-buffer-read-only buffer nil
        (when erase
          (erase-buffer buffer))
        (typeout-mode t)
        (when fn
          (save-excursion
            (with-open-stream (out (make-buffer-output-stream (buffer-end-point buffer)))
              (funcall fn out)))))
      (when read-only
        (setf (buffer-read-only-p buffer) t)))
    (when focus
      (setf (current-window) window))
    window))
|#

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

(defun pop-up-typeout-window (buffer function &key focus erase (read-only t))
  (declare (ignore focus))
  ;; !!!!!!!!!!!!!!!
  (setf lem-base::*buffer-list* (delete buffer lem-base::*buffer-list*))
  (when (and *typeout-window*
             (not (eq buffer (window-buffer *typeout-window*))))
    (dismiss-typeout-window)
    (redraw-display*))
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
                  (let ((window (make-floating-window buffer 0 0 (display-width) window-height t)))
                    (setf (window-parameter window 'prohibition-switch-to-buffer) t)
                    (add-hook (window-delete-hook window)
                              'delete-typeout-window-hook)
                    (setf *typeout-window* window
                          *typeout-before-window* (current-window))
                    window)))))
    (setf (variable-value 'modeline-format :buffer buffer) '())
    (setf (buffer-value buffer 'lem::modeline-status-list)
          (list 'typeout-window-modeline))
    (setf (buffer-value buffer 'typeout-buffer-p) t)
    (setf (current-window) window)
    (typeout-mode t)
    (redraw-display*)
    (values)))

(define-command dismiss-typeout-window () ()
  (unless (deleted-window-p *typeout-window*)
    (setf (current-window) *typeout-window*)
    (when (deleted-window-p *typeout-before-window*)
      (setf *typeout-before-window* (first (window-list))))
    (setf (current-window) *typeout-before-window*)
    (let ((buffer (window-buffer *typeout-window*)))
      (when (buffer-value buffer 'typeout-buffer-p)
        (delete-buffer buffer)))
    (delete-window *typeout-window*)))

(define-command next-page-or-dismiss-typeout-window () ()
  (unless (deleted-window-p *typeout-window*)
    (setf (current-window) *typeout-window*)
    (move-point (current-point) (window-view-point (current-window)))
    (unless (line-offset (current-point) (window-height (current-window)))
      (dismiss-typeout-window))))

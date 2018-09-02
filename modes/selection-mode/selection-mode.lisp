(uiop/package:define-package :lem-selection-mode/selection-mode (:use :cl :lem))
(in-package :lem-selection-mode/selection-mode)
;;;don't edit above
(define-minor-mode %selection-mode
    (:keymap *%selection-mode-keymap*
     :name "Sel"))

(define-minor-mode selection-mode
    (:keymap *selection-mode-keymap*
     :global t)
  (if (mode-active-p (current-buffer) 'selection-mode)
      (add-hook *pre-command-hook* 'pre-selection-command-hook)
      (progn (remove-hook *pre-command-hook* 'pre-selection-command-hook)
             (cancel-selection))))

(define-key *selection-mode-keymap* "Shift-Left" 'selection-backward-char)
(define-key *selection-mode-keymap* "Shift-Right" 'selection-forward-char)
(define-key *selection-mode-keymap* "Shift-Up" 'selection-previous-line)
(define-key *selection-mode-keymap* "Shift-Down" 'selection-next-line)
(define-key *selection-mode-keymap* "C-v" 'selection-paste)

(define-key *%selection-mode-keymap* "C-x" 'selection-cut)
(define-key *%selection-mode-keymap* "C-c" 'selection-copy)

(defvar *point* nil)
(defvar *overlay* nil)

(define-attribute selection
  (:light :foreground nil :background "cyan")
  (:dark  :foreground nil :background "cyan"))

(defun call-with-selection (f n)
  (when *overlay*
    (when (not (eql (current-buffer) (overlay-buffer *overlay*)))
      (let ((cb (current-buffer)))
        (setf (current-buffer) (overlay-buffer *overlay*))
        (lem::disable-minor-mode '%selection-mode)
        (setf (current-buffer) cb)))
    (delete-overlay *overlay*)
    (setf *overlay* nil))
  (unless (and (find '%selection-mode (buffer-minor-modes (current-buffer)))
               (eql (ignore-errors
                     (point-buffer *point*))
                    (current-buffer)))
    (when *point*
      (delete-point *point*))
    (lem::enable-minor-mode '%selection-mode)
    (setf *point* (copy-point (buffer-point (current-buffer)))))
  (prog1 
      (funcall f n)
    (setf *overlay* 
          (make-overlay *point* (buffer-point (current-buffer)) 'selection))))

(defun cancel-selection ()
  (when *overlay*
    (delete-overlay *overlay*)
    (setf *overlay* nil))
  (when *point*
    (save-excursion
      (switch-to-buffer (point-buffer *point*))
      (lem::disable-minor-mode '%selection-mode))
    (delete-point *point*)
    (setf *point* nil)))

(define-command selection-next-line (&optional n) ("p")
  (call-with-selection #'next-line n))
(define-command selection-backward-char (&optional n) ("p")
  (call-with-selection #'backward-char n))
(define-command selection-forward-char (&optional n) ("p")
  (call-with-selection #'forward-char n))
(define-command selection-previous-line (&optional n) ("p")
  (call-with-selection #'previous-line n))
(define-command selection-copy (&optional n) ("p")
  (declare (ignore n))
  (message "Copy")
  (lem::copy-to-clipboard (points-to-string (overlay-start *overlay*) (overlay-end *overlay*)))
  (cancel-selection))

(define-command selection-cut (&optional n) ("p")
  (declare (ignore n))
  (message "Cut")
  (lem::copy-to-clipboard (points-to-string (overlay-start *overlay*) (overlay-end *overlay*)))
  (delete-character (overlay-start *overlay*) 
                    (count-characters (overlay-start *overlay*) (overlay-end *overlay*)))
  (cancel-selection))

(define-command selection-paste (&optional n) ("p")
  (declare (ignore n))
  (message "Paste")
  (paste-from-clipboard))

(defun pre-selection-command-hook ()
  (unless (string= (package-name (symbol-package *this-command*)) :lem-selection-mode/selection-mode)
    (cancel-selection)))
;;(defpackage :lem  (:use :cl :lem))

(in-package :lem)

;; structure to track files.
(defstruct fb tab path dir open)

;;==============================================================================
;;
(defparameter *fbar-path* (truename "~/"))
(defparameter *fbar-buffer* nil)
(defparameter *fbar-window* nil)

(defparameter *old-window* nil)
(defparameter *old-buffer* nil)
(defparameter *old-point* nil)

(define-major-mode fbar-mode nil; fundamental-mode
    (:name "fbar" :keymap *fbar-mode-keymap*))


(define-attribute fbar-file
  (:light :foreground "black" )
  (:dark :foreground "white"
	 :background "#333333"))
(define-attribute fbar-dir
  (:light :foreground "blue" :bold-p t )
  (:dark :foreground "sky blue" :bold-p t
	 :background "#333333"))


;;==============================================================================
;;
;; FBAR is invoked with a <C-x f>;
;; dismiss with <Escape>
;; open-close directories with <Return>
;; select files with <Return>

(define-key *global-keymap* "C-x f" 'fbar-on)
(define-key *fbar-mode-keymap* "j" 'next-line)
(define-key *fbar-mode-keymap* "k" 'previous-line)
(define-key *fbar-mode-keymap* "Down" 'next-line)
(define-key *fbar-mode-keymap* "Up" 'previous-line)
(define-key *fbar-mode-keymap* "Return" 'fbar-select)
(define-key *fbar-mode-keymap* "Escape" 'fbar-off)

;;==============================================================================

(defun fbar-insert-entry (pt path dirp tab)
  (flet ((dname ()  (format nil "+ ~A" (car (last (pathname-directory path)))))
	 (fname ()  (format nil "~A.~A" (pathname-name path)
			    (or (pathname-type path) ""))))
    
    (let ((name (if dirp (dname) (fname))))
;;      (format xcb::*q* "...~A ~A~&" pt path)
      (insert-string pt
		     (format nil "~v,,,v<~>~A~&" tab #\space name)
		     :attribute  (if dirp 'fbar-dir  'fbar-file)
		     'type (make-fb :tab tab :path path  :dir dirp :open nil))
;;      (format xcb::*q* "...~A ~A~&" pt path)
      )))

(define-command fbar-select () ()
  (let ((prop (text-property-at (current-point) 'type)))
;;    (format xcb::*q* "prop: ~A~&" prop)
    ;;    (save-excursion)
    (setf (buffer-read-only-p *fbar-buffer*) nil)
    (if (fb-dir prop)
	(save-excursion
	  (if (fb-open prop)
	      ;; if open, close it
	      (let ((ourtab (fb-tab prop)))
		(setf (fb-open prop) nil)
		(next-line 1) (line-start (current-point))
		(loop 
		   for tab = (fb-tab (text-property-at (current-point) 'type))
		   while (> tab ourtab) do
		     (kill-line) (kill-line)))
	      ;; if closed, open it
	      (let ((newtab (+ 2 (fb-tab prop))))
		(setf (fb-open prop) t)
		(character-offset (line-end (current-point)) 1)
		(loop for f in (uiop:subdirectories (fb-path prop)) 
		   for pt = (current-point) do
		     (fbar-insert-entry pt f t newtab))
		(loop for f in (uiop:directory-files (fb-path prop)) do
		     (fbar-insert-entry (current-point) f nil newtab )))))
	;; file!
	(progn
	  (fbar-off)
	  (read-file (fb-path prop))))
    (setf (buffer-read-only-p *fbar-buffer*) t)))


(define-command fbar-on () ()
  (unless *fbar-window*
    (setf *old-window* (current-window)
	  *old-buffer* (current-buffer)
	  *old-point* (copy-point  (current-point) :temporary  ))
    (setf *fbar-window*
	  (lem::make-floating-window
	   *fbar-buffer*
	   0 0 20 (- (window-height (current-window)) 2) nil))
    
    (setf lem::*current-window* *fbar-window*)
    (setf (current-buffer) *fbar-buffer*)
    (redraw-display)))


(define-command fbar-off () ()
  (when *fbar-window*
    (setf (current-window) *old-window*)
    (setf (current-buffer) *old-buffer*)
    (delete-window *fbar-window*)
    (move-point (current-point) *old-point*)
    (switch-to-buffer *old-buffer* t t)
    ;(delete-point *old-point*)
    (setf *fbar-window* nil)
    (redraw-display)))



(defun fbar-init ()
  (let* ((buffer (make-buffer "FBAR" :temporary t
			      :enable-undo-p nil ))
	 (point (buffer-point buffer)))
    (erase-buffer buffer)
    (loop for f in (uiop:subdirectories *fbar-path*) do
	 (fbar-insert-entry point f t 1))
    (loop for f in (uiop:directory-files *fbar-path*) do
	 (fbar-insert-entry point f nil 1))
    (buffer-start point)
    (change-buffer-mode buffer 'fbar-mode)
    (setf *fbar-buffer* buffer)
    (setf (buffer-read-only-p *fbar-buffer*) t)
    ))

(fbar-init)

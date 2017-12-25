(in-package :lem)
;; structure to track files.
(defstruct fb tab path dir open)

(defparameter *fbar-path* (truename "~/"))
(define-major-mode fbar-mode ()
    (:name "fbar"
	   :keymap *fbar-mode-keymap*))


(define-attribute fbar-file
  (:light :foreground "black" )
  (:dark :foreground "white" ))
(define-attribute fbar-dir
  (:light :foreground "blue" :bold-p t )
  (:dark :foreground "sky blue" :bold-p t ))

(defparameter *fbar-window* nil)
(defvar *modeline-el* nil)
(defstruct (fbar-modeline-element (:conc-name el-))
  name)

(defun initialize-fbar-modeline ()
  (setf *modeline-el* (make-fbar-modeline-element))
  (modeline-add-status-list *modeline-el*))

(define-key *fbar-mode-keymap* "g" 'fbar-initial)
(define-key *fbar-mode-keymap* "Return" 'fbar-select)
;;==============================================================================

(defun fbar-insert-entry (path dirp tab)
  (flet ((dname ()  (format nil "+ ~A" (car (last (pathname-directory path)))))
	 (fname ()  (format nil "~A.~A" (pathname-name path)
			    (pathname-type path))))
    (with-point ((pt (window-view-point *fbar-window*)	;(current-point)
		  ))
      (let ((name (if dirp (dname) (fname))))
	(character-offset (line-end pt) 1)
	(insert-string pt
		       (format nil "~v,,,v<~>~A~&" tab #\space name)
		       :attribute  (if dirp 'fbar-dir  'fbar-file)
		       'type (make-fb :tab tab :path path  :dir dirp :open nil)))
      (move-point ;(current-point)
       (window-view-point *fbar-window*) pt
       ))))


(define-command fbar-initial () ()  
  (loop for f in (uiop:subdirectories *fbar-path*) do
       (fbar-insert-entry f t 1))
  (loop for f in (uiop:directory-files *fbar-path*) do
       (fbar-insert-entry f nil 1)))


(define-command fbar-select () ()
  (let ((prop (text-property-at (current-point) 'type)))
    (save-excursion 
      (if (fb-dir prop)
	  (if (fb-open prop)
	      (let ((ourtab (fb-tab prop)))
		(setf (fb-open prop) nil)
		(next-line 1) (line-start (current-point))
		(loop 
		   for tab = (fb-tab (text-property-at (current-point) 'type))
		   while (> tab ourtab) do
		     (kill-line) (kill-line)))
	      (let ((newtab (+ 2 (fb-tab prop))))
		(setf (fb-open prop) t)
		(loop for f in (uiop:subdirectories (fb-path prop)) do
		     (fbar-insert-entry f t newtab))
		(loop for f in (uiop:directory-files (fb-path prop)) do
		     (fbar-insert-entry f nil newtab ))))
	  ;; file!
	  (let ((buffer (get-previous-buffer (current-buffer))))
	    (read-file (fb-path prop)))))))

(defun fbar ()
  (setf *fbar-window*
	(make-floating-window (make-buffer "FBAR" :temporary t
					   :enable-undo-p nil)
			      0 0 20 (window-%height (current-window))
			      nil))
  

  
  )

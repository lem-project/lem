(defpackage :lem.dired
  (:use :cl :lem)
  (:export :dired
           :dired-buffer))
(in-package :lem.dired)

(defvar *header-attribute* (make-attribute "green" nil))
(defvar *file-attribute* (make-attribute nil nil))
(defvar *directory-attribute* (make-attribute "blue" nil :bold-p t))
(defvar *link-attribute* (make-attribute "green" nil))

(defvar *start-point*)

(define-major-mode dired-mode ()
    (:name "dired"
	   :keymap *dired-mode-keymap*))

(define-key *dired-mode-keymap* "q" 'quit-window)
(define-key *dired-mode-keymap* "g" 'dired-update-buffer)
(define-key *dired-mode-keymap* "^" 'dired-up-directory)
(define-key *dired-mode-keymap* "C-m" 'dired-find-file)
(define-key *dired-mode-keymap* "Spc" 'dired-read-file)
(define-key *dired-mode-keymap* "o" 'dired-find-file-other-window)

(define-key *dired-mode-keymap* "n" 'dired-next-line)
(define-key *dired-mode-keymap* "p" 'dired-previous-line)
(define-key *dired-mode-keymap* ">" 'dired-next-directory-line)
(define-key *dired-mode-keymap* "<" 'dired-previous-directory-line)

(define-key *dired-mode-keymap* "m" 'dired-mark-and-next-line)
(define-key *dired-mode-keymap* "u" 'dired-unmark-and-next-line)
(define-key *dired-mode-keymap* "U" 'dired-unmark-and-previous-line)
(define-key *dired-mode-keymap* "t" 'dired-toggle-marks)
(define-key *dired-mode-keymap* "* !" 'dired-unmark-all)
(define-key *dired-mode-keymap* "* %" 'dired-mark-regexp)

(define-key *dired-mode-keymap* "Q" 'dired-query-replace)

(define-key *dired-mode-keymap* "D" 'dired-delete-files)
(define-key *dired-mode-keymap* "C" 'dired-copy-files)
(define-key *dired-mode-keymap* "R" 'dired-rename-files)
(define-key *dired-mode-keymap* "+" 'dired-mkdir)

(defun dired-first-line-p (point)
  (lem::same-line-p *start-point* point))

(defun dired-last-line-p (point)
  (lem::last-line-p point))

(define-command dired-update-buffer () ()
  (save-excursion
    (update (current-buffer))))

(define-command dired-up-directory () ()
  (switch-to-buffer
   (dired-buffer
    (uiop:pathname-parent-directory-pathname
     (buffer-directory)))))

(define-command dired-find-file () ()
  (select-file 'find-file))

(define-command dired-read-file () ()
  (select-file 'read-file))

(define-command dired-find-file-other-window () ()
  (select-file (lambda (file)
                 (pop-to-buffer (find-file-buffer file)))))

(define-command dired-next-line (n) ("p")
  (let ((point (current-point)))
    (lem::line-offset point n)
    (when (point<= point *start-point*)
      (lem::move-point point *start-point*))
    (when (last-line-p point)
      (lem::line-offset point -1))
    (lem::line-start point)))

(define-command dired-previous-line (n) ("p")
  (let ((point (current-point)))
    (lem::line-offset point (- n))
    (when (point<= point *start-point*)
      (lem::move-point point *start-point*))
    (lem::line-start point)))

(define-command dired-next-directory-line (n) ("p")
  (lem::with-point ((cur-point (current-point)))
    (loop
       (when (dired-last-line-p cur-point)
	 (return))
       (lem::line-offset cur-point 1)
       (when (and (eq :directory (lem::text-property-at cur-point 'type))
		  (>= 0 (decf n)))
	 (lem::move-point (current-point) cur-point)
	 (return)))))

(define-command dired-previous-directory-line (n) ("p")
  (lem::with-point ((cur-point (current-point)))
    (loop
       (when (dired-first-line-p cur-point)
	 (return))
       (lem::line-offset cur-point -1)
       (when (and (eq :directory (lem::text-property-at cur-point 'type))
		  (>= 0 (decf n)))
	 (lem::move-point (current-point) cur-point)
	 (return)))))

(define-command dired-mark-and-next-line (n) ("p")
  (loop :repeat n :do
     (mark-current-line t)
     (dired-next-line 1)))

(define-command dired-unmark-and-next-line (n) ("p")
  (loop :repeat n :do
     (mark-current-line nil)
     (dired-next-line 1)))

(define-command dired-unmark-and-previous-line (n) ("p")
  (loop :repeat n :do
     (dired-previous-line 1)
     (mark-current-line nil)))

(define-command dired-toggle-marks () ()
  (mark-lines (constantly t)
              #'not))

(define-command dired-unmark-all () ()
  (mark-lines (constantly t)
              (constantly nil)))

(define-command dired-mark-regexp (regex) ("sRegex: ")
  (mark-lines (lambda (flag)
                (declare (ignore flag))
                (let ((file (get-file)))
                  (and file (ppcre:scan regex (namestring file)))))
              (constantly t)))

(defun dired-query-replace-internal (query-function)
  (destructuring-bind (before after)
      (lem.isearch:read-query-replace-args)
    (dolist (file (selected-files))
      (find-file file)
      (beginning-of-buffer)
      (funcall query-function before after))))

(define-command dired-query-replace () ()
  (dired-query-replace-internal 'lem.isearch:query-replace))

(define-command dired-query-replace-regexp () ()
  (dired-query-replace-internal 'lem.isearch:query-replace-regexp))

(define-command dired-query-replace-symbol () ()
  (dired-query-replace-internal 'lem.isearch:query-replace-symbol))

(defun run-command (string &rest args)
  (let ((error-string
         (with-output-to-string (error-output)
           (uiop:run-program (apply #'format nil string args)
                             :ignore-error-status t
                             :error-output error-output))))
    (when (string/= error-string "")
      (editor-error "~A" error-string))))

(define-command dired-delete-files () ()
  (when (minibuf-y-or-n-p "Really delete files")
    (dolist (file (selected-files))
      (run-command "rm -fr ~A" file)))
  (update-all))

(defun get-dest-directory ()
  (dolist (window (window-list) (buffer-directory))
    (when (and (not (eq window (current-window)))
               (eq 'dired-mode (buffer-major-mode (window-buffer window))))
      (return (buffer-directory (window-buffer window))))))

(define-command dired-copy-files () ()
  (let ((to-pathname (minibuf-read-file "Destination Filename: " (get-dest-directory))))
    (dolist (file (selected-files))
      (run-command "cp -r '~A' '~A'" file to-pathname)))
  (update-all))

(define-command dired-rename-files () ()
  (let ((to-pathname (minibuf-read-file "Destination Filename: " (get-dest-directory))))
    (dolist (file (selected-files))
      (run-command "mv '~A' '~A'" file to-pathname)))
  (update-all))

(define-command dired-mkdir (buffer-name) ("smkdir: ")
  (multiple-value-bind (pathname make-p)
      (ensure-directories-exist
       (uiop:ensure-directory-pathname
        (merge-pathnames buffer-name (buffer-directory))))
    (unless make-p
      (editor-error "failed mkdir: ~A" pathname))
    (message "mkdir: ~A" pathname)
    (update-all)))

(defun select-file (open-file)
  (let ((file (get-file)))
    (when file
      (funcall open-file file))))

(defun mark-current-line (flag)
  (let ((point (current-point)))
    (when (and (point<= *start-point* point)
               (not (last-line-p point)))
      (lem::line-start point)
      (with-buffer-read-only (point-buffer point) nil
        (save-excursion
          (lem::delete-char-at point 1)
          (if flag
              (lem::insert-character point #\*)
              (lem::insert-character point #\space)))))))

(defun mark-lines (test get-flag)
  (save-excursion
    (lem::move-point (current-point) *start-point*)
    (lem::line-offset (current-point) 2)
    (loop
       (beginning-of-line)
       (let ((flag (char= (following-char) #\*)))
	 (when (funcall test flag)
	   (mark-current-line (funcall get-flag flag))))
       (unless (forward-line 1)
	 (return)))))

(defun selected-files ()
  (let ((files '()))
    (save-excursion
      (lem::move-point (current-point) *start-point*)
      (loop
	 (beginning-of-line)
	 (let ((flag (char= (following-char) #\*)))
	   (when flag
	     (lem::line-start (current-point))
	     (push (get-file) files)))
	 (unless (forward-line 1)
	   (return))))
    (if (null files)
        (let ((file (get-file)))
          (when file
            (list file)))
        (nreverse files))))

(defun get-line-property (property-name)
  (let ((point
         (lem::character-offset (lem::line-start (copy-point (current-point)
                                                             :temporary))
                                1)))
    (when point
      (lem::text-property-at point property-name))))

(defun get-file ()
  (get-line-property 'file))

(defun get-type ()
  (get-line-property 'type))

(defun ls-output-string (filename)
  (with-output-to-string (stream)
    (uiop:run-program (format nil "ls -al ~A" filename) :output stream)))

(defun update (buffer)
  (with-buffer-read-only buffer nil
    (erase-buffer buffer)
    (let ((dirname (probe-file (buffer-directory buffer))))
      (lem::with-point ((cur-point (lem::buffer-point buffer) :left-inserting))
        (lem::insert-string cur-point
                               (namestring dirname)
                               :attribute *header-attribute*)
        (lem::insert-character cur-point #\newline 2)
        (let ((output-string (ls-output-string dirname)))
          (lem::insert-string cur-point output-string)
          (lem::buffer-start cur-point)
          (lem::line-offset cur-point 3)
          (setf *start-point* (copy-point cur-point :temporary))
          (loop
	     (let ((string (lem::line-string-at cur-point)))
	       (multiple-value-bind (start end start-groups end-groups)
		   (ppcre:scan "^(\\S*)\\s+(\\d+)\\s+(\\S*)\\s+(\\S*)\\s+(\\d+)\\s+(\\S+\\s+\\S+\\s+\\S+)\\s+(.*?)(?: -> .*)?$"
			       string)
		 (declare (ignorable start end start-groups end-groups))
		 (when start
		   (lem::insert-string cur-point "  ")
		   (let* ((index (1- (length start-groups)))
			  (filename (merge-pathnames
				     (subseq string
					     (aref start-groups index)
					     (aref end-groups index))
				     dirname))
			  (start-file-charpos (+ 2 (aref start-groups index))))
		     (lem::with-point ((start-point (lem::line-start cur-point))
				       (end-point (lem::line-end cur-point)))
		       (case (char string 0)
			 (#\l
			  (lem::put-text-property start-point end-point 'type :link)
			  (lem::character-offset (lem::line-start cur-point) start-file-charpos)
			  (lem::put-text-property cur-point end-point :attribute *link-attribute*))
			 (#\d
			  (lem::put-text-property start-point end-point 'type :directory)
			  (lem::character-offset (lem::line-start cur-point) start-file-charpos)
			  (lem::put-text-property cur-point end-point :attribute *directory-attribute*))
			 (#\-
			  (lem::put-text-property start-point end-point 'type :file)))
		       (lem::put-text-property start-point end-point 'file filename)))
		   (lem::line-offset cur-point 1)
		   (when (lem::end-line-p cur-point)
		     (return)))))))))))

(defun update-all ()
  (dolist (buffer (buffer-list))
    (when (eq 'dired-mode (buffer-major-mode buffer))
      (update buffer)))
  (redraw-display))

(defun dired-buffer (filename)
  (setf filename (uiop:directory-exists-p (expand-file-name (namestring filename))))
  (let ((buffer (get-buffer-create (format nil "DIRED \"~A\"" filename))))
    (change-buffer-mode buffer 'dired-mode)
    (setf (buffer-directory buffer) filename)
    (setf (buffer-read-only-p buffer) t)
    (buffer-disable-undo buffer)
    (update buffer)
    (lem::move-point (lem::buffer-point buffer)
                     *start-point*)
    buffer))

(setf *find-directory-function* 'dired-buffer)

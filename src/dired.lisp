(defpackage :lem.dired
  (:use :cl :lem)
  (:export :dired
           :dired-buffer))
(in-package :lem.dired)

(defvar *header-attribute* (make-attribute "green" nil))
(defvar *file-attribute* (make-attribute nil nil))
(defvar *directory-attribute* (make-attribute "blue" nil :bold-p t))
(defvar *link-attribute* (make-attribute "green" nil))

(defparameter *start-line-number* 4)

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

(defun move-to-file-column ()
  (move-to-column (get-bvar 'start-file-column :default 0)))

(defun move-to-start-point ()
  (setf (current-linum) *start-line-number*)
  (move-to-file-column))

(define-command dired-update-buffer () ()
  (save-excursion
    (update)))

(define-command dired-up-directory () ()
  (switch-to-buffer
   (dired-buffer
    (namestring
     (uiop:pathname-parent-directory-pathname
      (buffer-filename))))))

(define-command dired-find-file () ()
  (select-file 'find-file))

(define-command dired-read-file () ()
  (select-file 'read-file))

(define-command dired-find-file-other-window () ()
  (select-file (lambda (file)
                 (when (one-window-p)
                   (split-window-sensibly (current-window)))
                 (other-window 1)
                 (find-file file))))

(define-command dired-next-line (n) ("p")
  (forward-line n)
  (when (> *start-line-number* (current-linum))
    (setf (current-linum) *start-line-number*))
  (when (end-line-p)
    (forward-line -1))
  (move-to-file-column))

(define-command dired-previous-line (n) ("p")
  (forward-line (- n))
  (setf (current-linum)
        (max *start-line-number* (current-linum)))
  (move-to-file-column))

(define-command dired-next-directory-line (n) ("p")
  (let ((point (current-point)))
    (loop
      (unless (forward-line 1)
        (setf (current-point) point)
        (return))
      (when (and (eq :directory (get-property (current-point) 'type))
                 (>= 0 (decf n)))
        (move-to-file-column)
        (return)))))

(define-command dired-previous-directory-line (n) ("p")
  (let ((point (current-point)))
    (loop
      (unless (forward-line -1)
        (setf (current-point) point)
        (return))
      (when (and (eq :directory (get-property (current-point) 'type))
                 (>= 0 (decf n)))
        (move-to-file-column)
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
                (let ((file (get-property (current-point) 'file)))
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
  (let ((file (get-property (beginning-of-line-point) 'file)))
    (when file
      (funcall open-file file))))

(defun mark-current-line (flag)
  (when (and (<= *start-line-number* (current-linum))
             (not (end-line-p)))
    (save-excursion
      (beginning-of-line)
      (with-buffer-read-only (current-buffer) nil
        (delete-char 1)
        (if flag
            (insert-char #\*)
            (insert-char #\space))))))

(defun mark-lines (test get-flag)
  (save-excursion
    (setf (current-linum) *start-line-number*)
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
      (setf (current-linum) *start-line-number*)
      (loop
        (beginning-of-line)
        (let ((flag (char= (following-char) #\*)))
          (when flag
            (move-to-file-column)
            (push (get-property (current-point) 'file)
                  files)))
        (unless (forward-line 1)
          (return))))
    (if (null files)
        (list (get-property (current-point) 'file))
        (nreverse files))))

(defun ls-output-string (filename)
  (with-output-to-string (stream)
    (uiop:run-program (format nil "ls -al ~A" filename) :output stream)))

(defun update ()
  (with-buffer-read-only (current-buffer) nil
    (buffer-erase)
    (let ((dirname (probe-file (buffer-filename))))
      (insert-string-with-attribute (namestring dirname) *header-attribute*)
      (insert-newline 2)
      (let ((output-string (ls-output-string dirname)))
        (insert-string output-string)
        (beginning-of-buffer)
        (forward-line (1- *start-line-number*))
        (setf (get-bvar 'start-point) (current-point))
        (loop
          (let ((string (current-line-string)))
            (multiple-value-bind (start end start-groups end-groups)
                (ppcre:scan "^(\\S*)\\s+(\\d+)\\s+(\\S*)\\s+(\\S*)\\s+(\\d+)\\s+(\\S+\\s+\\S+\\s+\\S+)\\s+(.*?)(?: -> .*)?$"
                            string)
              (declare (ignorable start end start-groups end-groups))
              (when start
                (save-excursion
                  (beginning-of-line)
                  (insert-string "  "))
                (let* ((index (1- (length start-groups)))
                       (filename (merge-pathnames
                                  (subseq string
                                          (aref start-groups index)
                                          (aref end-groups index))
                                  dirname))
                       (start-file-charpos (+ 2 (aref start-groups index))))
                  (save-excursion
                    (move-to-column start-file-charpos)
                    (setf (get-bvar 'start-file-column) (1+ (current-column))))
                  (case (char string 0)
                    (#\l
                     (put-property (beginning-of-line-point) (end-of-line-point) 'type :link)
                     (shift-position start-file-charpos)
                     (put-attribute (current-point) (end-of-line-point) *link-attribute*))
                    (#\d
                     (put-property (beginning-of-line-point) (end-of-line-point) 'type :directory)
                     (shift-position start-file-charpos)
                     (put-attribute (current-point) (end-of-line-point) *directory-attribute*))
                    (#\-
                     (put-property (beginning-of-line-point) (end-of-line-point) 'type :file)))
                  (put-property (beginning-of-line-point) (end-of-line-point) 'file filename)))
              (unless (forward-line 1)
                (return)))))))))

(defun update-all ()
  (dolist (buffer (buffer-list))
    (when (eq 'dired-mode (buffer-major-mode buffer))
      (with-current-buffer (buffer (make-min-point))
        (update))))
  (redraw-display))

(defun dired-buffer (filename)
  (setf filename (uiop:directory-exists-p (expand-file-name (namestring filename))))
  (let ((buffer (get-buffer-create (format nil "DIRED \"~A\"" filename))))
    (with-current-buffer (buffer (make-min-point))
      (dired-mode)
      (setf (buffer-filename buffer) (namestring filename))
      (setf (buffer-read-only-p buffer) t)
      (buffer-disable-undo buffer)
      (update))
    (let ((prev-buffer (current-buffer)))
      (setf (current-buffer) buffer)
      (move-to-start-point)
      (setf (current-buffer) prev-buffer))
    buffer))

(setf *find-directory-function* 'dired-buffer)

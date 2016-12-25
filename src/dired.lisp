(defpackage :lem.dired
  (:use :cl :lem)
  (:export :dired
           :dired-buffer))
(in-package :lem.dired)

(defvar *header-attribute* (make-attribute "green" nil))
(defvar *file-attribute* (make-attribute nil nil))
(defvar *directory-attribute* (make-attribute "blue" nil :bold-p t))
(defvar *link-attribute* (make-attribute "green" nil))

(defvar *start-marker*)

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

(defun dired-first-line-p (marker)
  (lem::same-line-p *start-marker* marker))

(defun dired-last-line-p (marker)
  (lem::last-line-p marker))

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
  (let ((point (current-marker)))
    (lem::line-offset point n)
    (when (point<= point *start-marker*)
      (lem::move-point point *start-marker*))
    (when (last-line-p point)
      (lem::line-offset point -1))
    (lem::line-start point)))

(define-command dired-previous-line (n) ("p")
  (let ((point (current-marker)))
    (lem::line-offset point (- n))
    (when (point<= point *start-marker*)
      (lem::move-point point *start-marker*))
    (lem::line-start point)))

(define-command dired-next-directory-line (n) ("p")
  (lem::with-marker ((cur-marker (current-marker)))
    (loop
      (when (dired-last-line-p cur-marker)
        (return))
      (lem::line-offset cur-marker 1)
      (when (and (eq :directory (lem::text-property-at cur-marker 'type))
                 (>= 0 (decf n)))
        (lem::move-point (current-marker) cur-marker)
        (return)))))

(define-command dired-previous-directory-line (n) ("p")
  (lem::with-marker ((cur-marker (current-marker)))
    (loop
      (when (dired-first-line-p cur-marker)
        (return))
      (lem::line-offset cur-marker -1)
      (when (and (eq :directory (lem::text-property-at cur-marker 'type))
                 (>= 0 (decf n)))
        (lem::move-point (current-marker) cur-marker)
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
  (let ((point (current-marker)))
    (when (and (point<= *start-marker* point)
               (not (last-line-p point)))
      (lem::line-start point)
      (with-buffer-read-only (marker-buffer point) nil
        (save-excursion
          (lem::delete-char-at point 1)
          (if flag
              (lem::insert-char-at point #\*)
              (lem::insert-char-at point #\space)))))))

(defun mark-lines (test get-flag)
  (save-excursion
    (lem::move-point (current-marker) *start-marker*)
    (lem::line-offset (current-marker) 2)
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
      (lem::move-point (current-marker) *start-marker*)
      (loop
        (beginning-of-line)
        (let ((flag (char= (following-char) #\*)))
          (when flag
            (lem::line-start (current-marker))
            (push (get-file) files)))
        (unless (forward-line 1)
          (return))))
    (if (null files)
        (list (get-file))
        (nreverse files))))

(defun get-line-property (property-name)
  (lem::text-property-at (lem::character-offset (lem::line-start (copy-marker (current-marker)
                                                                              :temporary))
                                                1)
                         property-name))

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
      (lem::with-marker ((cur-marker (lem::buffer-point-marker buffer) :left-inserting))
        (lem::insert-string-at cur-marker
                               (lem.text-property::make-text-property
                                (namestring dirname)
                                :attribute *header-attribute*))
        (lem::insert-char-at cur-marker #\newline 2)
        (let ((output-string (ls-output-string dirname)))
          (lem::insert-string-at cur-marker output-string)
          (lem::buffer-start cur-marker)
          (lem::line-offset cur-marker 3)
          (setf *start-marker* (copy-marker cur-marker :temporary))
          (loop
            (let ((string (lem::line-string-at cur-marker)))
              (multiple-value-bind (start end start-groups end-groups)
                  (ppcre:scan "^(\\S*)\\s+(\\d+)\\s+(\\S*)\\s+(\\S*)\\s+(\\d+)\\s+(\\S+\\s+\\S+\\s+\\S+)\\s+(.*?)(?: -> .*)?$"
                              string)
                (declare (ignorable start end start-groups end-groups))
                (when start
                  (lem::insert-string-at cur-marker "  ")
                  (let* ((index (1- (length start-groups)))
                         (filename (merge-pathnames
                                    (subseq string
                                            (aref start-groups index)
                                            (aref end-groups index))
                                    dirname))
                         (start-file-charpos (+ 2 (aref start-groups index))))
                    (lem::with-marker ((start-marker (lem::line-start cur-marker))
                                       (end-marker (lem::line-end cur-marker)))
                      (case (char string 0)
                        (#\l
                         (lem::put-text-property start-marker end-marker 'type :link)
                         (lem::character-offset (lem::line-start cur-marker) start-file-charpos)
                         (lem::put-text-property cur-marker end-marker :attribute *link-attribute*))
                        (#\d
                         (lem::put-text-property start-marker end-marker 'type :directory)
                         (lem::character-offset (lem::line-start cur-marker) start-file-charpos)
                         (lem::put-text-property cur-marker end-marker :attribute *directory-attribute*))
                        (#\-
                         (lem::put-text-property start-marker end-marker 'type :file)))
                      (lem::put-text-property start-marker end-marker 'file filename)))
                  (lem::line-offset cur-marker 1)
                  (when (lem::end-line-p cur-marker)
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
    (lem::move-point (lem::buffer-point-marker buffer)
                     *start-marker*)
    buffer))

(setf *find-directory-function* 'dired-buffer)

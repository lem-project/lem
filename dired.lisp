(in-package :cl-user)
(defpackage :lem.dired
  (:use :cl :lem)
  (:export
   :dired
   :dired-buffer))
(in-package :lem.dired)

(define-major-mode dired-mode nil
  (:name "dired"
   :keymap *dired-mode-keymap*))

(define-key *dired-mode-keymap* (kbd "n") 'dired-next-line)
(define-key *dired-mode-keymap* (kbd "p") 'dired-previous-line)
(define-key *dired-mode-keymap* (kbd "q") 'dired-quit)

(defun dired-cursor-column ()
  (get-bvar :dired-cursor-column :default 0))

(define-command dired-next-line (&optional (n 1)) ("p")
  (forward-line n)
  (move-to-column (dired-cursor-column)))

(define-command dired-previous-line (&optional (n 1)) ("p")
  (forward-line (- n))
  (move-to-column (dired-cursor-column)))

(define-command dired-quit () ()
  (quit-window))

(defparameter +mark+ #\*)
(defparameter +unmark+ #\space)

(defun symbolic-link-p (pathname)
  (eq :symbolic-link (osicat:file-kind (cl-fad:pathname-as-file pathname))))

(defun goto-start-line ()
  (point-set (get-bvar :start-point))
  (move-to-column (dired-cursor-column)))

(defun date (universal-time)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time universal-time)
    (declare (ignorable second minute hour date month year day daylight-p zone))
    (format nil "~4,'0d/~2,'0d/~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d"
            year month date
            (aref #("Mon" "Tue" "Wed" "Thr" "Fri" "Sat" "Sun") day)
            hour minute second)))

(defun update ()
  (with-buffer-read-only (current-buffer) nil
    (buffer-erase)
    (let ((dirname (buffer-filename))
          (files))
      ;(insert-string (namestring dirname))
      ;(insert-newline 2)
      (setf (get-bvar :start-point) (current-point))
      (dolist (file (cl-fad:list-directory dirname :follow-symlinks nil))
        (push file files)
        (let ((filename (enough-namestring file dirname))
              (symlink-p))
          (when (symbolic-link-p file)
            (setq symlink-p t)
            (setq filename
                  (concatenate 'string
                               filename
                               " -> "
                               (namestring (probe-file file)))))
          (insert-string
           (format nil "  ~a ~a ~23a "
                   (cond (symlink-p "l")
                         ((pathname-name file) "-")
                         (t "d"))
                   (with-open-file (in file)
                     (format nil "~10d" (file-length in)))
                   (date (file-write-date file))))
          (unless (get-bvar :dired-cursor-column)
            (setf (get-bvar :dired-cursor-column)
                  (current-column)))
          (insert-string filename)
          (insert-newline)))
      (setf (get-bvar :dired-files)
            (apply #'vector (reverse files))))
    (goto-start-line)))

(defun update-all ()
  (dolist (buffer (buffer-list))
    (when (eq 'dired-mode (buffer-major-mode buffer))
      (save-excursion
       (set-buffer buffer nil)
       (update)))))

(defun dired-find-directory (dirname)
  (check-switch-minibuffer-window)
  (let ((buffer (get-buffer-create (princ-to-string dirname))))
    (set-buffer buffer)
    (dired-mode)
    (setf (buffer-filename buffer) dirname)
    (setf (buffer-read-only-p buffer) t)
    (update)))

(defun dired-internal (dirname)
  (cond (dirname
         (dired-find-directory dirname))
        (t
         (message "No such directory")
         nil)))

(define-key *global-keymap* (kbd "C-x C-M-d") 'dired)
(define-command dired (dirname) ((list
                                  (cl-fad:directory-exists-p
                                   (minibuf-read-file "Dired: "
                                                      (buffer-directory)
                                                      (buffer-directory)))))
  (dired-internal dirname)
  (goto-start-line))

(defun get-file ()
  (let* ((point (get-bvar :start-point))
         (n (- (current-linum)
               (1- (point-linum point)))))
    (let ((files (get-bvar :dired-files)))
      (when (<= 1 n (length files))
        (aref files (1- n))))))

(defun select-file (open-file-fn)
  (let ((pathname (get-file)))
    (when pathname
      (cond ((cl-fad:directory-exists-p pathname)
             (dired-internal pathname))
            (pathname
             (funcall open-file-fn (namestring pathname)))))))

(define-key *dired-mode-keymap* (kbd "C-m") 'dired-find-file)
(define-command dired-find-file () ()
  (select-file 'find-file))

(define-key *dired-mode-keymap* (kbd "Spc") 'dired-view-file)
(define-command dired-view-file () ()
  (select-file 'read-file))

(define-key *dired-mode-keymap* (kbd "o") 'dired-find-file-other-window)
(define-command dired-find-file-other-window () ()
  (select-file (lambda (file)
                 (when (one-window-p)
                   (split-window-sensibly (current-window)))
                 (other-window 1)
                 (find-file file))))

(define-key *dired-mode-keymap* (kbd "g") 'dired-update-buffer)
(define-command dired-update-buffer () ()
  (update))

(define-key *dired-mode-keymap* (kbd "^") 'dired-up-directory)
(define-command dired-up-directory () ()
  (dired-find-directory
   (namestring
    (cl-fad:pathname-parent-directory
     (buffer-filename)))))

(defun change-flag (char)
  (when (get-file)
    (with-buffer-read-only (current-buffer) nil
      (beginning-of-line)
      (insert-string (string char))
      (delete-char 1 nil)
      (beginning-of-line))))

(defun map-mark (test)
  (save-excursion
   (goto-start-line)
   (do (flag-char)
       ((eobp))
     (let ((mark-char (following-char))
           (file (get-file)))
       (when (and file
                  mark-char
                  (setq flag-char (funcall test (file-namestring file) mark-char)))
         (change-flag flag-char))
       (forward-line 1)))))

(define-key *dired-mode-keymap* (kbd "m") 'dired-mark)
(define-command dired-mark () ()
  (change-flag +mark+)
  (forward-line 1))

(define-key *dired-mode-keymap* (kbd "*") 'dired-mark-with-pattern)
(define-command dired-mark-with-pattern (regex) ("sRegex: ")
  (let ((scanner (ppcre:create-scanner regex)))
    (map-mark #'(lambda (filename mark-char)
                  (declare (ignore mark-char))
                  (when (ppcre:scan scanner filename)
                    +mark+)))))

(define-key *dired-mode-keymap* (kbd "u") 'dired-unmark)
(define-command dired-unmark () ()
  (change-flag +unmark+)
  (forward-line 1))

(define-key *dired-mode-keymap* (kbd "U") 'dired-unmark-prev-line)
(define-command dired-unmark-prev-line () ()
  (forward-line -1)
  (change-flag +unmark+))

(define-key *dired-mode-keymap* (kbd "!") 'dired-unmark-all)
(define-command dired-unmark-all () ()
  (map-mark (constantly +unmark+)))

(define-key *dired-mode-keymap* (kbd "t") 'dired-mark-toggle)
(define-command dired-mark-toggle () ()
  (map-mark #'(lambda (filename mark-char)
                (declare (ignore filename))
                (if (eql +unmark+ mark-char)
                    +mark+
                    +unmark+))))

(defun collect-files ()
  (let ((files))
    (save-excursion
     (goto-start-line)
     (beginning-of-line)
     (do ((file nil nil))
         ((eobp))
       (when (and (eql +mark+ (following-char))
                  (setq file (get-file)))
         (push file files))
       (forward-line 1)))
    (if (null files)
        (let ((file (get-file)))
          (when file
            (list file)))
        (nreverse files))))

(defun walk-directory (fn dirname)
  (labels ((f (dirname)
              (if (symbolic-link-p dirname)
                  (funcall fn dirname)
                  (dolist (file (cl-fad:list-directory dirname :follow-symlinks nil))
                    (cond ((symbolic-link-p file)
                           (funcall fn (cl-fad:pathname-as-file file)))
                          ((osicat:directory-pathname-p file)
                           (f file)
                           (funcall fn file))
                          (t
                           (funcall fn file)))))))
    (f dirname)
    (funcall fn (pathname (probe-file dirname)))
    (values)))

(defun delete-directory-and-files (dirname)
  (walk-directory #'(lambda (file)
                      (cond ((symbolic-link-p file)
                             (delete-file file))
                            ((cl-fad:directory-exists-p file)
                             (osicat:delete-directory file))
                            (t
                             (delete-file file))))
                  dirname))

(defun delete-files (delete-files)
  (dolist (file delete-files)
    (if (cl-fad:directory-exists-p file)
        (delete-directory-and-files file)
        (delete-file file))))

(define-key *dired-mode-keymap* (kbd "D") 'dired-delete-files)
(define-command dired-delete-files () ()
  (let ((delete-files
         (collect-files)))
    (when (minibuf-y-or-n-p "Really delete files")
      (delete-files delete-files)
      (update))))

(defun pathname-directory-exists-p (pathname)
  (cl-fad:file-exists-p
   (if (not (cl-fad:directory-pathname-p pathname))
       (directory-namestring pathname)
       pathname)))

(defun file-pathname-p (pathname)
  (not (cl-fad:directory-pathname-p pathname)))

(defun subdirectory-p (to-pathname from-pathname)
  (let ((to-dir (pathname-directory to-pathname))
        (from-dir (pathname-directory from-pathname)))
    (assert (eq :absolute (car to-dir)))
    (assert (eq :absolute (car from-dir)))
    (and (<= (length from-dir)
             (length to-dir))
         (loop
           :for from-elt :in (cdr from-dir)
           :for to-elt :in (cdr to-dir)
           :when (not (equal from-elt to-elt))
           :do (return nil)
           :finally (return t)))))

(defun copy-file->file (from-pathname to-pathname)
  (let* ((size 2000)
         (seq (make-array size :element-type '(unsigned-byte 8))))
    (with-open-file (in from-pathname
                        :direction :input
                        :element-type '(unsigned-byte 8))
      (with-open-file (out to-pathname
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :element-type '(unsigned-byte 8))
        (loop
          (let ((result (read-sequence seq in)))
            (write-sequence seq out :end result)
            (when (< result size)
              (return))))))))

(defun copy-file->dir (from-pathname to-pathname)
  (copy-file->file from-pathname
                   (merge-pathnames (file-namestring from-pathname)
                                    to-pathname)))

(defun copy-symlink->file (from-pathname to-pathname)
  (osicat:make-link to-pathname :target from-pathname))

(defun copy-symlink->dir (from-pathname to-pathname)
  (copy-symlink->file from-pathname
                      (merge-pathnames (file-namestring
                                        (cl-fad:pathname-as-file
                                         from-pathname))
                                       to-pathname)))

(defun copy-dir->dir (from-pathname to-pathname)
  (let ((to-dir-pathname
         (if (cl-fad:file-exists-p to-pathname)
             (cl-fad:pathname-as-directory
              (merge-pathnames (file-namestring
                                (cl-fad:pathname-as-file
                                 from-pathname))
                               to-pathname))
             to-pathname)))
    (when (subdirectory-p to-pathname from-pathname)
      (editor-error "Cannot copy `~a' into its subdirectory `~a'"
                    from-pathname to-pathname))
    (multiple-value-bind (to-dir-pathname made-directory-p)
        (ensure-directories-exist to-dir-pathname)
      (assert made-directory-p)
      (dolist (from
               (cl-fad:list-directory from-pathname
                                      :follow-symlinks nil))
        (copy-file from to-dir-pathname)))))

(defun copy-file (from-pathname to-pathname)
  (cond ((file-pathname-p from-pathname)
         (let ((symlink-p (not (equal from-pathname (probe-file from-pathname)))))
           (cond ((file-pathname-p to-pathname)
                  (when (and (cl-fad:file-exists-p to-pathname)
                             (not (minibuf-y-or-n-p
                                   (format nil "Overwrite `~a'?"
                                           to-pathname))))
                    (return-from copy-file))
                  (if symlink-p
                      (copy-symlink->file from-pathname to-pathname)
                      (copy-file->file from-pathname to-pathname)))
                 (t
                  (if symlink-p
                      (copy-symlink->dir from-pathname to-pathname)
                      (copy-file->dir from-pathname to-pathname))))))
        (t
         (copy-dir->dir from-pathname
                        (cl-fad:pathname-as-directory
                         to-pathname)))))

(defun copy-files (copy-files to-pathname)
  (let ((res (cl-fad:directory-exists-p to-pathname)))
    (when res
      (setq to-pathname res)))
  (let ((num-copy-files (length copy-files)))
    (cond ((or (and (< 1 num-copy-files)
                    (not (cl-fad:directory-pathname-p to-pathname)))
               (and (= 1 num-copy-files)
                    (cl-fad:directory-exists-p (car copy-files))
                    (not (or (not (probe-file to-pathname))
                             (cl-fad:directory-exists-p to-pathname)))))
           (editor-error "Target must be a directory"))
          ((and (not (and (= 1 num-copy-files)
                          (cl-fad:directory-pathname-p (car copy-files))))
                (not (pathname-directory-exists-p to-pathname)))
           (editor-error "No such file or directory"))))
  (dolist (from-file copy-files)
    (copy-file from-file to-pathname))
  t)

(defun get-dest-directory ()
  (dolist (window (window-list) (buffer-directory))
    (when (and (not (eq window (current-window)))
               (eq 'dired-mode (buffer-major-mode (window-buffer window))))
      (return (buffer-directory (window-buffer window))))))

(define-key *dired-mode-keymap* (kbd "C") 'dired-copy-files)
(define-command dired-copy-files () ()
  (let ((copy-files
         (collect-files))
        (to-pathname
         (minibuf-read-file "Destination Filename: "
                            (get-dest-directory))))
    (copy-files copy-files to-pathname)
    (update-all)))

(define-key *dired-mode-keymap* (kbd "R") 'dired-rename-files)
(define-command dired-rename-files () ()
  (let ((copy-files
         (collect-files))
        (to-pathname
         (minibuf-read-file "Destination Filename: "
                            (get-dest-directory))))
    (and (copy-files copy-files to-pathname)
         (delete-files copy-files))
    (update-all)))

(define-key *dired-mode-keymap* (kbd "+") 'dired-mkdir)
(define-command dired-mkdir (buffer-name) ("smkdir:")
  (multiple-value-bind (pathname make-p)
      (ensure-directories-exist
       (uiop:ensure-directory-pathname
        (merge-pathnames buffer-name (buffer-directory))))
    (unless make-p
      (editor-error "failed mkdir: ~A" pathname))
    (message "mkdir: ~A" pathname)
    (update)))

(defun dired-buffer (filename)
  (save-excursion
    (dired filename)
    (current-buffer)))

(in-package :lem)

(export '(completion
          completion-test
          completion-hypheen
          completion-file
          completion-buffer-name
          start-completion))

(defun completion-test (x y)
  (and (<= (length x) (length y))
       (string= x y :end2 (length x))))

(defun %comp-split (string separator)
  (let ((list nil) (words 0) (end (length string)))
    (when (zerop end) (return-from %comp-split nil))
    (flet ((separatorp (char) (find char separator))
           (done () (return-from %comp-split (cons (subseq string 0 end) list))))
      (loop :for start = (position-if #'separatorp string :end end :from-end t) :do
	 (when (null start) (done))
	 (push (subseq string (1+ start) end) list)
	 (push (string (aref string start)) list)
	 (incf words)
	 (setf end start)))))

(defun logand-strings (strings)
  (let* ((str (car strings))
         (len (length str)))
    (dolist (s (cdr strings))
      (let ((res (mismatch str s :end1 len)))
        (when res
          (setq len res))))
    (subseq str 0 len)))

(defun completion (name list &key (test #'search) separator key)
  (let ((strings
         (remove-if-not (if separator
                            (let* ((parts1 (%comp-split name separator))
                                   (parts1-length (length parts1)))
                              (lambda (elt)
                                (when key
                                  (setf elt (funcall key elt)))
                                (let* ((parts2 (%comp-split elt separator))
                                       (parts2-length (length parts2)))
                                  (and (<= parts1-length parts2-length)
                                       (loop
					  :for p1 :in parts1
					  :for p2 :in parts2
					  :unless (funcall test p1 p2)
					  :do (return nil)
					  :finally (return t))))))
                            (lambda (elt)
                              (funcall test name elt)))
                        list)))
    (cond
      ((null strings) nil)
      ((null (cdr strings))
       (values (car strings)
	       strings))
      (t
       (values (logand-strings strings) strings)))))

(defun completion-hypheen (name list &key key)
  (completion name list :test #'completion-test :separator "-" :key key))

(defun completion-file (str)
  (setf str (expand-file-name str))
  (let* ((dirname (directory-namestring str))
         (files (mapcar #'namestring (cl-fad:list-directory dirname))))
    (let ((strings
           (loop
	      :for pathname :in (or (directory str) (list str))
	      :for str := (namestring pathname)
	      :append
	      (multiple-value-bind (andstr strings)
		  (completion (enough-namestring str dirname)
			      files
			      :test #'completion-test
			      :separator "-."
			      :key #'(lambda (path)
				       (enough-namestring path dirname)))
		(when andstr strings)))))
      (values (logand-strings strings) strings))))

(defun completion-buffer-name (str)
  (completion str (mapcar #'buffer-name (buffer-list))))

(defvar *completion-mode-keymap* (make-keymap 'completion-self-insert))
(defvar *completion-window* nil)
(defvar *completion-overlay* nil)
(defvar *completion-overlay-attribute* (make-attribute "blue" nil :reverse-p t))

(define-minor-mode completion-mode
    (:name "completion"
	   :keymap *completion-mode-keymap*))

(defun completion-update-overlay ()
  (when *completion-overlay*
    (delete-overlay *completion-overlay*))
  (setf *completion-overlay*
        (make-overlay (line-start (copy-point (current-point) :temporary))
                      (line-end (copy-point (current-point) :temporary))
                      *completion-overlay-attribute*)))

(define-key *completion-mode-keymap* (kbd "C-n") 'completion-next-line)
(define-key *completion-mode-keymap* (kbd "M-n") 'completion-next-line)
(define-key *completion-mode-keymap* (kbd "C-i") 'completion-next-line)
(define-command completion-next-line (n) ("p")
  (with-current-window *completion-window*
    (if (last-line-p (current-point))
        (buffer-start (current-point))
        (line-offset (current-point) n))
    (completion-update-overlay)))

(define-key *completion-mode-keymap* (kbd "C-p") 'completion-previous-line)
(define-key *completion-mode-keymap* (kbd "M-p") 'completion-previous-line)
(define-command completion-previous-line (n) ("p")
  (with-current-window *completion-window*
    (if (first-line-p (current-point))
        (end-of-buffer)
        (forward-line (- n)))
    (completion-update-overlay)))

(define-key *completion-mode-keymap* (kbd "C-v") 'completion-next-page)
(define-command completion-next-page () ()
  (with-current-window *completion-window*
    (unless (next-page)
      (end-of-buffer))
    (completion-update-overlay)))

(define-key *completion-mode-keymap* (kbd "M-v") 'completion-previous-page)
(define-command completion-previous-page () ()
  (with-current-window *completion-window*
    (unless (prev-page)
      (beginning-of-buffer))
    (completion-update-overlay)))

(define-key *completion-mode-keymap* (kbd "M->") 'completion-end-of-buffer)
(define-command completion-end-of-buffer () ()
  (with-current-window *completion-window*
    (end-of-buffer)
    (completion-update-overlay)))

(define-key *completion-mode-keymap* (kbd "M-<") 'completion-beginning-of-buffer)
(define-command completion-beginning-of-buffer () ()
  (with-current-window *completion-window*
    (beginning-of-buffer)
    (completion-update-overlay)))

(defvar *completion-last-string* nil)
(defvar *completion-last-function* nil)

(defun completion-end ()
  (completion-mode nil)
  (delete-completion-window)
  (setf *completion-last-string* nil)
  (setf *completion-last-function* nil)
  t)

(define-key *completion-mode-keymap* (kbd "C-m") 'completion-select)
(define-command completion-select () ()
  (let ((str
         (line-string-at
          (buffer-point
           (window-buffer *completion-window*)))))
    (delete-character (current-point) (- (length *completion-last-string*)) nil)
    (setf *completion-last-string* str)
    (insert-string (current-point) str)
    (completion-end))
  t)

(define-key *completion-mode-keymap* (kbd "C-h") 'completion-delete-previous-char)
(define-key *completion-mode-keymap* (kbd "[backspace]") 'completion-delete-previous-char)
(define-command completion-delete-previous-char (n) ("p")
  (delete-character (current-point) (- n) nil)
  (update-completion *completion-last-function*
		     (subseq *completion-last-string* 0 (- (length *completion-last-string*) n))))

(define-command completion-self-insert (n) ("p")
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (insert-character (current-point) c n)
             (update-completion *completion-last-function*
				(concatenate 'string
					     *completion-last-string*
					     (string c))))
          (t (unread-key-sequence (last-read-key-sequence))
             (completion-end)))))

(defun update-completion (comp-f str)
  (setf *completion-last-function* comp-f)
  (setf *completion-last-string* str)
  (multiple-value-bind (result strings) (funcall comp-f str)
    (cond (strings
           (let ((buffer (get-buffer-create "*Completions*")))
             (setf *completion-window*
                   (with-pop-up-typeout-window (out buffer :erase t)
                     (format out "~{~A~^~%~}" strings)))
             (set-window-delete-hook (lambda () (setf *completion-window* nil))
                                     *completion-window*)
             (setf (get-bvar :completion-buffer-p :buffer buffer) t)
             (with-current-window *completion-window*
               (completion-update-overlay))))
          ((null result)
           (completion-end)))
    (if result
        (values result t (and (consp strings) (null (cdr strings))))
        (values str nil))))

(defun start-completion (comp-f str)
  (completion-mode t)
  (multiple-value-bind (str result confirm-p)
      (update-completion comp-f str)
    (declare (ignore result))
    (when confirm-p
      (delete-character (current-point) (- (length *completion-last-string*)) nil)
      (insert-string (current-point) str)
      (completion-end)))
  t)

(defun delete-completion-window ()
  (when (and (window-p *completion-window*)
             (not (deleted-window-p *completion-window*)))
    (with-current-window *completion-window*
      (when (get-bvar :completion-buffer-p
                      :buffer (window-buffer *completion-window*))
        (quit-window)))
    (setf *completion-window* nil)))

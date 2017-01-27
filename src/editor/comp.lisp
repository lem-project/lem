(in-package :lem)

(export '(completion
          completion-test
          completion-hypheen
          completion-file
          completion-buffer-name
          run-completion))

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

(defun completion-file (str directory)
  (setf str (expand-file-name str directory))
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


(defstruct completion-item
  (label "" :read-only t :type string)
  (detail "" :read-only t :type string)
  (start nil :read-only t :type (or null point))
  (end nil :read-only t :type (or null point))
  (apply-fn nil :read-only t :type (or null function)))

(defvar *completion-mode-keymap* (make-keymap 'completion-self-insert))
(define-minor-mode completion-mode
    (:name "completion"
     :keymap *completion-mode-keymap*))

(define-key *completion-mode-keymap* "C-n" 'completion-next-line)
(define-key *completion-mode-keymap* "M-n" 'completion-next-line)
(define-key *completion-mode-keymap* "C-i" 'completion-next-line)
(define-key *completion-mode-keymap* "C-p" 'completion-previous-line)
(define-key *completion-mode-keymap* "M-p" 'completion-previous-line)
(define-key *completion-mode-keymap* "C-v" 'completion-next-page)
(define-key *completion-mode-keymap* "M-v" 'completion-previous-page)
(define-key *completion-mode-keymap* "M->" 'completion-end-of-buffer)
(define-key *completion-mode-keymap* "M-<" 'completion-beginning-of-buffer)
(define-key *completion-mode-keymap* "C-m" 'completion-select)

(defvar *completion-overlay* nil)
(defvar *completion-overlay-attribute* (make-attribute "blue" nil :reverse-p t))

(defun completion-buffer ()
  (get-buffer "*Completion*"))

(defun completion-buffer-point ()
  (let ((buffer (completion-buffer)))
    (when buffer
      (buffer-point buffer))))

(defun completion-window ()
  (let ((buffer (completion-buffer)))
    (when buffer
      (first (get-buffer-windows buffer)))))

(defun update-completion-overlay (point)
  (when *completion-overlay*
    (delete-overlay *completion-overlay*))
  (when point
    (with-point ((start point)
                 (end point))
      (setf *completion-overlay*
            (make-overlay (line-start start)
                          (line-end end)
                          *completion-overlay-attribute*)))))

(defun completion-end ()
  (completion-mode nil)
  (let ((window (completion-window)))
    (with-current-window window
      (quit-window window))))

(define-command completion-self-insert () ()
  (unread-key-sequence (last-read-key-sequence))
  (completion-end))

(define-command completion-next-line () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (line-offset point 1)
    (update-completion-overlay point)))

(define-command completion-previous-line () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (line-offset point -1)
    (update-completion-overlay point)))

(define-command completion-next-page () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (with-current-window (first (get-buffer-windows (point-buffer point)))
      (next-page))
    (update-completion-overlay point)))

(define-command completion-previous-page () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (with-current-window (first (get-buffer-windows (point-buffer point)))
      (prev-page))
    (update-completion-overlay point)))

(define-command completion-end-of-buffer () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (buffer-end point)
    (update-completion-overlay point)))

(define-command completion-beginning-of-buffer () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (buffer-start point)
    (update-completion-overlay point)))

(define-command completion-select () ()
  (let* ((completion-point (completion-buffer-point))
         (item (when completion-point
                 (text-property-at completion-point :item))))
    (completion-insert (current-point) item)
    (completion-end)))

(defun start-completion-mode (buffer)
  (completion-mode t)
  (update-completion-overlay (buffer-point buffer)))

(defun completion-insert (point item)
  (cond ((completion-item-apply-fn item)
         (funcall (completion-item-apply-fn item)
                  point))
        ((and (completion-item-start item)
              (completion-item-end item))
         (move-point point (completion-item-start item))
         (delete-between-points (completion-item-start item)
                                (completion-item-end item))
         (insert-string point (completion-item-label item)))))

(defun run-completion (items &optional (start-completion-mode t))
  (if (uiop:length=n-p items 1)
      (completion-insert (current-point) (first items))
      (let ((buffer (get-buffer-create "*Completion*")))
        (erase-buffer buffer)
        (display-buffer buffer)
        (let ((point (buffer-point buffer)))
          (dolist (item items)
            (insert-string point (completion-item-label item))
            (insert-string point " ")
            (insert-string point (completion-item-detail item))
            (put-text-property (line-start (copy-point point :temporary)) point :item item)
            (insert-character point #\newline))
          (buffer-start point))
        (when start-completion-mode
          (start-completion-mode buffer)))))

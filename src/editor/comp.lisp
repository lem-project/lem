(in-package :lem)

(export '(completion
          completion-test
          completion-hypheen
          completion-file
          completion-buffer-name
          make-completion-item
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
(define-key *completion-mode-keymap* "M->" 'completion-end-of-buffer)
(define-key *completion-mode-keymap* "M-<" 'completion-beginning-of-buffer)
(define-key *completion-mode-keymap* "C-m" 'completion-select)
(define-key *completion-mode-keymap* "Spc" 'completion-insert-space-and-cancel)

(defvar *completion-overlay* nil)
(defvar *completion-overlay-attribute* (make-attribute "blue" nil :reverse-p t))
(defvar *completion-attribute* (make-attribute nil nil :reverse-p t))
(defvar *completion-window* nil)
(defvar *completion-restart-function* nil)

(defun completion-buffer ()
  (get-buffer "*Completion*"))

(defun completion-buffer-point ()
  (let ((buffer (completion-buffer)))
    (when buffer
      (buffer-point buffer))))

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
  (when *completion-overlay*
    (delete-overlay *completion-overlay*))
  (completion-mode nil)
  (delete-window *completion-window*)
  (let ((buffer (completion-buffer)))
    (when buffer
      (delete-buffer buffer)))
  (redraw-display t))

(define-command completion-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (insert-character (current-point) c)
             (completion-end)
             (when *completion-restart-function*
               (funcall *completion-restart-function*)))
          (t (unread-key-sequence (last-read-key-sequence))
             (completion-end)))))

(define-command completion-next-line () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (unless (line-offset point 1)
      (buffer-start point))
    (window-see *completion-window*)
    (update-completion-overlay point)))

(define-command completion-previous-line () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (unless (line-offset point -1)
      (buffer-end point))
    (window-see *completion-window*)
    (update-completion-overlay point)))

(define-command completion-end-of-buffer () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (buffer-end point)
    (window-see *completion-window*)
    (update-completion-overlay point)))

(define-command completion-beginning-of-buffer () ()
  (alexandria:when-let ((point (completion-buffer-point)))
    (buffer-start point)
    (window-see *completion-window*)
    (update-completion-overlay point)))

(define-command completion-select () ()
  (let* ((completion-point (completion-buffer-point))
         (item (when completion-point
                 (text-property-at completion-point :item))))
    (completion-insert (current-point) item)
    (completion-end)))

(define-command completion-insert-space-and-cancel () ()
  (insert-character (current-point) #\space)
  (completion-end))

(defun start-completion-mode (buffer restart-function)
  (setf *completion-restart-function* restart-function)
  (completion-mode t)
  (update-completion-overlay (buffer-point buffer)))

(defun completion-insert (point item)
  (when item
    (cond ((completion-item-apply-fn item)
           (funcall (completion-item-apply-fn item)
                    point))
          ((and (completion-item-start item)
                (completion-item-end item))
           (move-point point (completion-item-start item))
           (delete-between-points (completion-item-start item)
                                  (completion-item-end item))
           (insert-string point (completion-item-label item))))))

(defun create-completion-buffer (items back-attribute)
  (let ((buffer (get-buffer-create "*Completion*")))
    (erase-buffer buffer)
    (let ((point (buffer-point buffer))
          (max-column 0))
      (loop :for rest-items :on items
            :for item := (car rest-items)
            :do
            (insert-string point (completion-item-label item))
            (insert-string point " ")
            (insert-string point (completion-item-detail item))
            (setf max-column (max max-column (point-column point)))
            (with-point ((start (line-start (copy-point point :temporary))))
              (put-text-property start point :item item))
            (when (cdr rest-items)
              (insert-character point #\newline)))
      (buffer-start point)
      (loop
        (move-to-column point max-column t)
        (when back-attribute
          (with-point ((start point)
                       (end point))
            (put-text-property (line-start start)
                               (line-end end)
                               :attribute *completion-attribute*)))
        (line-offset point 1)
        (when (end-buffer-p point)
          (return)))
      (buffer-start point)
      (values buffer max-column))))

(defun run-completion (items &key restart-function (auto-insert t))
  (if (and auto-insert (uiop:length=n-p items 1))
      (completion-insert (current-point) (first items))
      (multiple-value-bind (buffer max-column)
          (create-completion-buffer items *completion-attribute*)
        (setf *completion-window*
              (balloon (current-window)
                       buffer
                       (+ 2 max-column)
                       (min (length items)
                            (display-height))))
        (start-completion-mode buffer restart-function)))
  t)

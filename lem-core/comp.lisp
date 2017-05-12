(in-package :lem)

(export '(completion-attribute
          non-focus-completion-attribute
          completion
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
    strings))

(defun completion-hypheen (name list &key key)
  (completion name list :test #'completion-test :separator "-" :key key))

(defun completion-file (str directory)
  (setf str (expand-file-name str directory))
  (let* ((dirname (directory-namestring str))
         (files (mapcar #'namestring (append (uiop:directory-files dirname)
                                             (uiop:subdirectories dirname)))))
    (let ((strings
           (loop
             :for pathname :in (or (directory str) (list str))
             :for str := (namestring pathname)
             :append
             (completion (enough-namestring str dirname)
                         files
                         :test #'completion-test
                         :separator "-."
                         :key #'(lambda (path)
                                  (enough-namestring path dirname))))))
      strings)))

(defun completion-buffer-name (str)
  (completion str (mapcar #'buffer-name (buffer-list))))


(defstruct completion-item
  (label "" :read-only t :type string)
  (detail "" :read-only t :type string)
  (start nil :read-only t :type (or null point))
  (end nil :read-only t :type (or null point))
  (apply-fn nil :read-only t :type (or null function)))

(defvar *completion-mode-keymap* (make-keymap :name '*completion-mode-keymap*
                                              :undef-hook 'completion-self-insert))
(define-minor-mode completion-mode
    (:name "completion"
     :keymap *completion-mode-keymap*))

(define-key *completion-mode-keymap* "C-n"    'completion-next-line)
(define-key *completion-mode-keymap* "[down]" 'completion-next-line)
(define-key *completion-mode-keymap* "M-n"    'completion-next-line)
(define-key *completion-mode-keymap* "C-i"    'completion-next-line)
(define-key *completion-mode-keymap* "C-p"    'completion-previous-line)
(define-key *completion-mode-keymap* "[up]"   'completion-previous-line)
(define-key *completion-mode-keymap* "M-p"    'completion-previous-line)
(define-key *completion-mode-keymap* "M->"    'completion-end-of-buffer)
(define-key *completion-mode-keymap* "M-<"    'completion-beginning-of-buffer)
(define-key *completion-mode-keymap* "C-m"    'completion-select)
(define-key *completion-mode-keymap* "Spc"    'completion-insert-space-and-cancel)
(define-key *completion-mode-keymap* "[backspace]" 'completion-delete-prevous-char)
(define-key *completion-mode-keymap* "C-h" 'completion-delete-prevous-char)

(define-attribute completion-attribute
  (t :foreground "blue" :background "white" :reverse-p t))
(define-attribute non-focus-completion-attribute
  (t :reverse-p t))

(defvar *completion-overlay* nil)
(defvar *completion-window* nil)
(defvar *completion-buffer* nil)
(defvar *completion-restart-function* nil)

(defun completion-buffer ()
  *completion-buffer*)

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
                          'completion-attribute)))))

(defun completion-end ()
  (when *completion-overlay*
    (delete-overlay *completion-overlay*))
  (completion-mode nil)
  (delete-window *completion-window*)
  (let ((buffer (completion-buffer)))
    (when buffer
      (delete-buffer buffer)
      (setf *completion-buffer* nil)))
  (redraw-display t))

(defun completion-again ()
  (completion-end)
  (when *completion-restart-function*
    (funcall *completion-restart-function*)))

(define-command completion-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (insert-character (current-point) c)
             (completion-again))
          (t (unread-key-sequence (last-read-key-sequence))
             (completion-end)))))

(define-command completion-delete-prevous-char (n) ("p")
  (delete-previous-char n)
  (completion-again))

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
  (let ((buffer (or (completion-buffer)
                    (make-buffer "*Completion*" :enable-undo-p nil :temporary t))))
    (setf *completion-buffer* buffer)
    (erase-buffer buffer)
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (let ((point (buffer-point buffer))
          (max-column 0)
          (label-end-column
           (reduce (lambda (max item)
                     (max max (1+ (string-width (completion-item-label item)))))
                   items
                   :initial-value 0)))
      (loop :for rest-items :on items
            :for item := (car rest-items)
            :do
            (insert-string point (completion-item-label item))
            (move-to-column point label-end-column t)
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
                               :attribute 'non-focus-completion-attribute)))
        (line-offset point 1)
        (when (end-buffer-p point)
          (return)))
      (buffer-start point)
      (values buffer max-column))))

(defun run-completion (items &key restart-function (auto-insert t))
  (cond
    ((null items))
    ((and auto-insert (uiop:length=n-p items 1))
     (completion-insert (current-point) (first items)))
    (t
     (multiple-value-bind (buffer max-column)
         (create-completion-buffer items 'non-focus-completion-attribute)
       (setf *completion-window*
             (balloon (current-window)
                      buffer
                      (+ 1 max-column)
                      (min 20 (length items))))
       (start-completion-mode buffer restart-function))))
  t)

(in-package :cl-user)
(defpackage :lem.isearch
  (:use :cl :lem)
  (:export
   :*isearch-keymap*
   :isearch-mode
   :isearch-forward
   :isearch-backward
   :isearch-forward-regexp
   :isearch-backward-regexp
   :isearch-forward-symbol
   :isearch-backward-symbol
   :isearch-forward-symbol-at-point
   :isearch-abort
   :isearch-delete-char
   :isearch-raw-insert
   :isearch-end
   :isearch-next
   :isearch-prev
   :isearch-yank
   :isearch-self-insert
   :read-query-replace-args
   :query-replace
   :query-replace-regexp
   :query-replace-symbol))
(in-package :lem.isearch)

(defvar *isearch-keymap* (make-keymap 'isearch-self-insert))
(defvar *isearch-prompt*)
(defvar *isearch-string*)
(defvar *isearch-prev-string* "")
(defvar *isearch-start-point*)
(defvar *isearch-search-function*)
(defvar *isearch-search-forward-function*)
(defvar *isearch-search-backward-function*)
(defvar *isearch-highlight-overlays* nil)

(define-minor-mode isearch-mode
    (:name "isearch"
	   :keymap *isearch-keymap*))

(define-attribute isearch-highlight-attribute
  (t :reverse-p t))

(define-attribute isearch-highlight-active-attribute
  (t :foreground "cyan" :reverse-p t))

(defun isearch-update-display ()
  (isearch-update-minibuffer)
  (isearch-update-buffer (current-point)))

(defun isearch-update-minibuffer ()
  (message-without-log "~A~A" *isearch-prompt* *isearch-string*))

(define-key *global-keymap* (kbd "C-s") 'isearch-forward)
(define-command isearch-forward () ()
  (isearch-start
   "ISearch: "
   (lambda (point str)
     (search-forward (or (character-offset point (- (length str)))
                         point)
                     str))
   #'search-forward
   #'search-backward
   ""))

(define-key *global-keymap* (kbd "C-r") 'isearch-backward)
(define-command isearch-backward () ()
  (isearch-start
   "ISearch: "
   (lambda (point str)
     (search-backward (or (character-offset point (length str))
                          point)
                      str))
   #'search-forward
   #'search-backward
   ""))

(define-key *global-keymap* (kbd "C-M-s") 'isearch-forward-regexp)
(define-command isearch-forward-regexp () ()
  (isearch-start "ISearch Regexp: "
                 #'search-forward-regexp
                 #'search-forward-regexp
                 #'search-backward-regexp
                 ""))

(define-key *global-keymap* (kbd "C-M-r") 'isearch-backward-regexp)
(define-command isearch-backward-regexp () ()
  (isearch-start "ISearch Regexp: "
                 #'search-backward-regexp
                 #'search-forward-regexp
                 #'search-backward-regexp
                 ""))

(define-key *global-keymap* (kbd "M-s _") 'isearch-forward-symbol)
(define-command isearch-forward-symbol () ()
  (isearch-start "ISearch Symbol: "
                 #'search-forward-symbol
                 #'search-forward-symbol
                 #'search-backward-symbol
                 ""))

(define-key *global-keymap* (kbd "M-s M-_") 'isearch-backward-symbol)
(define-command isearch-backward-symbol () ()
  (isearch-start "ISearch Symbol: "
                 #'search-backward-symbol
                 #'search-forward-symbol
                 #'search-backward-symbol
                 ""))

(define-key *global-keymap* (kbd "M-s .") 'isearch-forward-symbol-at-point)
(define-command isearch-forward-symbol-at-point () ()
  (let ((point (current-point)))
    (skip-chars-forward point #'syntax-symbol-char-p)
    (skip-chars-backward point #'syntax-symbol-char-p t)
    (skip-chars-backward point #'syntax-symbol-char-p)
    (with-point ((start point))
      (skip-chars-forward point #'syntax-symbol-char-p)
      (with-point ((end point))
        (isearch-start "ISearch Symbol: "
                       #'search-forward-symbol
                       #'search-forward-symbol
                       #'search-backward-symbol
                       (points-to-string start end))))))

(defun isearch-start (prompt
                      search-func
                      search-forward-function
                      search-backward-function
                      initial-string)
  (isearch-mode t)
  (setq *isearch-prompt* prompt)
  (setq *isearch-string* initial-string)
  (setq *isearch-search-function* search-func)
  ;; isearch中にバッファを変更する機能をつけるとここの:temporaryは変えないと駄目
  (setq *isearch-start-point* (copy-point (current-point) :temporary))
  (setq *isearch-search-forward-function* search-forward-function)
  (setq *isearch-search-backward-function* search-backward-function)
  (isearch-update-display)
  t)

(define-key *isearch-keymap* (kbd "C-g") 'isearch-abort)
(define-command isearch-abort () ()
  (move-point (current-point) *isearch-start-point*)
  (isearch-reset-buffer)
  t)

(define-key *isearch-keymap* (kbd "C-h") 'isearch-delete-char)
(define-key *isearch-keymap* (kbd "[backspace]") 'isearch-delete-char)
(define-key *isearch-keymap* (kbd "[del]") 'isearch-delete-char)
(define-command isearch-delete-char () ()
  (when (plusp (length *isearch-string*))
    (setq *isearch-string*
          (subseq *isearch-string*
                  0
                  (1- (length *isearch-string*))))
    (isearch-update-display)))

(define-key *isearch-keymap* (kbd "C-q") 'isearch-raw-insert)
(define-command isearch-raw-insert () ()
  (isearch-add-char (read-key)))

(define-key *isearch-keymap* (kbd "C-j") 'isearch-end)
(define-key *isearch-keymap* (kbd "C-m") 'isearch-end)
(define-command isearch-end () ()
  (isearch-reset-buffer)
  (setq *isearch-prev-string* *isearch-string*)
  (isearch-mode nil)
  t)

(define-key *isearch-keymap* (kbd "C-s") 'isearch-next)
(define-command isearch-next () ()
  (when (string= "" *isearch-string*)
    (setq *isearch-string* *isearch-prev-string*))
  (funcall *isearch-search-forward-function* (current-point) *isearch-string*)
  (isearch-update-display))

(define-key *isearch-keymap* (kbd "C-r") 'isearch-prev)
(define-command isearch-prev () ()
  (when (string= "" *isearch-string*)
    (setq *isearch-string* *isearch-prev-string*))
  (funcall *isearch-search-backward-function* (current-point) *isearch-string*)
  (isearch-update-display))

(define-key *isearch-keymap* (kbd "C-y") 'isearch-yank)
(define-command isearch-yank () ()
  (let ((str (kill-ring-first-string)))
    (when str
      (setq *isearch-string* str)
      (isearch-update-display))))

(defun isearch-reset-buffer ()
  (mapc #'delete-overlay *isearch-highlight-overlays*)
  (setq *isearch-highlight-overlays* nil))

(defun isearch-update-buffer (point &optional (search-string *isearch-string*))
  (isearch-reset-buffer)
  (unless (equal search-string "")
    (window-see (current-window))
    (with-point ((cur-point (window-view-point (current-window)))
                 (limit-point (or (line-offset
                                   (copy-point (window-view-point (current-window))
                                               :temporary)
                                   (window-height (current-window)))
                                  (buffers-end (window-buffer (current-window))))))
      (loop (with-point ((prev-point cur-point))
              (unless (funcall *isearch-search-forward-function*
                               cur-point
                               search-string
                               limit-point)
                (return))
              (let ((start-point (with-point ((temp-point cur-point :temporary))
                                   (when (funcall *isearch-search-backward-function*
                                                  temp-point
                                                  search-string
                                                  prev-point)
                                     temp-point))))
                (when (or (null start-point) (point= start-point cur-point))
                  ;; 正規表現の^や(?=text)が終わらないので中断する
                  (return))
                (push (make-overlay start-point
                                    (copy-point cur-point :temporary)
                                    (if (and (point<= start-point point)
                                             (point<= point cur-point))
                                        'isearch-highlight-active-attribute
                                        'isearch-highlight-attribute))
                      *isearch-highlight-overlays*)))))))

(defun isearch-add-char (c)
  (setq *isearch-string*
        (concatenate 'string
                     *isearch-string*
                     (string c)))
  (isearch-update-display)
  (with-point ((start-point (current-point)))
    (unless (funcall *isearch-search-function* (current-point) *isearch-string*)
      (move-point (current-point) start-point)))
  t)

(define-command isearch-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (isearch-add-char c))
          (t (isearch-update-display)
             (unread-key-sequence (last-read-key-sequence))
             (isearch-end)))))

(defvar *replace-before-string* nil)
(defvar *replace-after-string* nil)

(defun read-query-replace-args ()
  (let ((before)
        (after))
    (setq before
          (prompt-for-string
           (if *replace-before-string*
               (format nil "Before (~a with ~a): "
                       *replace-before-string*
                       *replace-after-string*)
               "Before: ")))
    (when (equal "" before)
      (cond (*replace-before-string*
             (setq before *replace-before-string*)
             (setq after *replace-after-string*)
             (return-from read-query-replace-args
               (list before after)))
            (t
             (message "Before string is empty")
             (return-from read-query-replace-args
               (list nil nil)))))
    (setq after (prompt-for-string "After: "))
    (setq *replace-before-string* before)
    (setq *replace-after-string* after)
    (list before after)))

(defun query-replace-internal-body (cur-point goal-point before after)
  (let ((pass-through nil))
    (loop
       (when (or (not (funcall *isearch-search-forward-function* cur-point before))
		 (and goal-point (point< goal-point cur-point)))
	 (when goal-point
	   (move-point (current-point) goal-point))
	 (return))
       (with-point ((end cur-point :right-inserting))
	 (isearch-update-buffer cur-point before)
	 (funcall *isearch-search-backward-function* cur-point before)
	 (with-point ((start cur-point :right-inserting))
	   (loop :for c := (unless pass-through
			     (minibuf-read-char (format nil "Replace ~s with ~s" before after)))
	      :do (cond
		    ((or pass-through (char= c #\y))
		     (delete-between-points start end)
		     (insert-string cur-point after)
		     (return))
		    ((char= c #\n)
		     (move-point cur-point end)
		     (return))
		    ((char= c #\!)
		     (setf pass-through t)))))))))

(defun query-replace-internal (before after search-forward-function search-backward-function)
  (unwind-protect
       (let ((*isearch-search-forward-function* search-forward-function)
	     (*isearch-search-backward-function* search-backward-function)
	     (buffer (current-buffer)))
	 (when (and before after)
	   (if (buffer-mark-p buffer)
	       (with-point ((mark-point (buffer-mark buffer) :right-inserting))
		 (if (point< mark-point (buffer-point buffer))
		     (query-replace-internal-body mark-point
						  (buffer-point buffer)
						  before after)
		     (query-replace-internal-body (buffer-point buffer)
						  mark-point
						  before after)))
	       (query-replace-internal-body (buffer-point buffer)
					    nil
					    before after))))
    (isearch-reset-buffer)))

(define-key *global-keymap* (kbd "M-%") 'query-replace)
(define-command query-replace (before after)
    ((read-query-replace-args))
  (query-replace-internal before
                          after
                          #'search-forward
                          #'search-backward))

(define-command query-replace-regexp (before after)
    ((read-query-replace-args))
  (query-replace-internal before
                          after
                          #'search-forward-regexp
                          #'search-backward-regexp))

(define-command query-replace-symbol (before after)
    ((read-query-replace-args))
  (query-replace-internal before
                          after
                          #'search-forward-symbol
                          #'search-backward-symbol))

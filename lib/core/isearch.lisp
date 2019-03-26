(defpackage :lem.isearch
  (:use :cl :lem)
  (:export :*isearch-keymap*
           :isearch-highlight-attribute
           :isearch-highlight-active-attribute
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
           :isearch-replace-highlight
           :isearch-next-highlight
           :isearch-prev-highlight
           :isearch-toggle-highlighting
           :read-query-replace-args
           :isearch-toggle-highlighting
           :query-replace
           :query-replace-regexp
           :query-replace-symbol)
  #+sbcl
  (:lock t))
(in-package :lem.isearch)

(defvar *isearch-keymap* (make-keymap :name '*isearch-keymap*
                                      :undef-hook 'isearch-self-insert))
(defvar *isearch-prompt*)
(defvar *isearch-string*)
(defvar *isearch-prev-string* "")
(defvar *isearch-start-point*)
(defvar *isearch-search-function*)
(defvar *isearch-search-forward-function*)
(defvar *isearch-search-backward-function*)

(define-attribute isearch-highlight-attribute
  (t :foreground "black" :background "gray"))

(define-attribute isearch-highlight-active-attribute
  (t :foreground "black" :background "cyan"))

(define-editor-variable isearch-next-last nil)
(define-editor-variable isearch-prev-last nil)

(define-minor-mode isearch-mode
    (:name "isearch"
     :keymap *isearch-keymap*
     :disable-hook 'disable-hook))

(define-key *global-keymap* "C-s" 'isearch-forward)
(define-key *global-keymap* "C-r" 'isearch-backward)
(define-key *global-keymap* "C-M-s" 'isearch-forward-regexp)
(define-key *global-keymap* "C-M-r" 'isearch-backward-regexp)
(define-key *global-keymap* "M-s _" 'isearch-forward-symbol)
(define-key *global-keymap* "M-s M-_" 'isearch-backward-symbol)
(define-key *global-keymap* "M-s ." 'isearch-forward-symbol-at-point)
(define-key *isearch-keymap* "C-g" 'isearch-abort)
(define-key *isearch-keymap* "C-h" 'isearch-delete-char)
(define-key *isearch-keymap* "Backspace" 'isearch-delete-char)
(define-key *isearch-keymap* "Delete" 'isearch-delete-char)
(define-key *isearch-keymap* "C-q" 'isearch-raw-insert)
(define-key *isearch-keymap* "C-j" 'isearch-finish)
(define-key *isearch-keymap* "Return" 'isearch-finish)
(define-key *isearch-keymap* "C-s" 'isearch-next)
(define-key *isearch-keymap* "C-r" 'isearch-prev)
(define-key *isearch-keymap* "C-y" 'isearch-yank)
(define-key *global-keymap* "F2" 'isearch-replace-highlight)
(define-key *global-keymap* "M-s M-n" 'isearch-next-highlight)
(define-key *global-keymap* "M-s n" 'isearch-next-highlight)
(define-key *global-keymap* "M-s M-p" 'isearch-prev-highlight)
(define-key *global-keymap* "M-s p" 'isearch-prev-highlight)
(define-key *global-keymap* "F3" 'isearch-next-highlight)
(define-key *global-keymap* "Shift-F3" 'isearch-prev-highlight)

(defun disable-hook ()
  (setf (variable-value 'isearch-next-last :buffer) nil)
  (setf (variable-value 'isearch-prev-last :buffer) nil))


(defun isearch-overlays (buffer)
  (buffer-value buffer 'isearch-overlays))

(defun isearch-reset-overlays (buffer)
  (mapc #'delete-overlay (buffer-value buffer 'isearch-overlays))
  (setf (buffer-value buffer 'isearch-overlays) '()))

(defun isearch-add-overlay (buffer overlay)
  (push overlay (buffer-value buffer 'isearch-overlays)))

(defun isearch-sort-overlays (buffer)
  (setf (buffer-value buffer 'isearch-overlays)
        (sort (buffer-value buffer 'isearch-overlays) #'point< :key #'overlay-start)))

(defun isearch-visible-overlays (buffer)
  (not (null (buffer-value buffer 'isearch-overlays))))

(defun isearch-next-overlay-point (point)
  (dolist (ov (buffer-value point 'isearch-overlays))
    (when (point< point (overlay-start ov))
      (return (overlay-start ov)))))

(defun isearch-prev-overlay-point (point)
  (let ((prev))
    (dolist (ov (buffer-value point 'isearch-overlays)
                (when prev
                  (overlay-start prev)))
      (when (point<= point (overlay-start ov))
        (return (overlay-start prev)))
      (setf prev ov))))


(defun isearch-update-buffer (&optional (point (current-point))
                                        (search-string *isearch-string*))
  (let ((buffer (point-buffer point)))
    (isearch-reset-overlays buffer)
    (unless (equal search-string "")
      (dolist (window (get-buffer-windows buffer))
        (with-point ((curr (window-view-point window))
                     (limit (window-view-point window)))
          (unless (line-offset limit (window-height window))
            (buffer-end limit))
          (loop :with prev
                :do (when (and prev (point= prev curr)) (return))
                    (setf prev (copy-point curr :temporary))
                    (unless (funcall *isearch-search-forward-function*
                                     curr search-string limit)
                      (return))
                    (with-point ((before curr))
                      (unless (funcall *isearch-search-backward-function*
                                       before search-string prev)
                        (return))
                      (when (point= before curr)
                        (return))
                      (isearch-add-overlay buffer
                                           (make-overlay
                                            before curr
                                            (if (and (point<= before point)
                                                     (point<= point curr))
                                                'isearch-highlight-active-attribute
                                                'isearch-highlight-attribute)))))))
      (isearch-sort-overlays buffer))))

(defun isearch-update-display ()
  (isearch-update-minibuffer)
  (window-see (current-window))
  (isearch-update-buffer))

(defun isearch-update-minibuffer ()
  (message-without-log "~A~A~A"
                       (if (or (variable-value 'isearch-next-last :buffer)
                               (variable-value 'isearch-prev-last :buffer))
                           "Failing "
                           "")
                       *isearch-prompt*
                       *isearch-string*))

(defun make-add-char-callback (search-function)
  (lambda (point string)
    (alexandria:when-let (p (funcall search-function
                                     (copy-point *isearch-start-point* :temporary)
                                     string))
      (move-point point p))))

(define-command isearch-forward (&optional prompt) ((list nil))
  (isearch-start
   (or prompt "ISearch: ")
   (make-add-char-callback #'search-forward)
   #'search-forward
   #'search-backward
   ""))

(define-command isearch-backward (&optional prompt) ((list nil))
  (isearch-start
   (or prompt "ISearch: ")
   (make-add-char-callback #'search-backward)
   #'search-forward
   #'search-backward
   ""))

(define-command isearch-forward-regexp (&optional prompt) ((list nil))
  (isearch-start (or prompt "ISearch Regexp: ")
                 (make-add-char-callback #'search-forward-regexp)
                 #'search-forward-regexp
                 #'search-backward-regexp
                 ""))

(define-command isearch-backward-regexp (&optional prompt) ((list nil))
  (isearch-start (or prompt "ISearch Regexp: ")
                 (make-add-char-callback #'search-backward-regexp)
                 #'search-forward-regexp
                 #'search-backward-regexp
                 ""))

(define-command isearch-forward-symbol (&optional prompt) ((list nil))
  (isearch-start (or prompt "ISearch Symbol: ")
                 #'search-forward-symbol
                 #'search-forward-symbol
                 #'search-backward-symbol
                 ""))

(define-command isearch-backward-symbol (&optional prompt) ((list nil))
  (isearch-start (or prompt "ISearch Symbol: ")
                 #'search-backward-symbol
                 #'search-forward-symbol
                 #'search-backward-symbol
                 ""))

(define-command isearch-forward-symbol-at-point () ()
  (let ((point (current-point)))
    (skip-chars-forward point #'syntax-symbol-char-p)
    (skip-chars-backward point (complement #'syntax-symbol-char-p))
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
  (unless (minibuffer-window-active-p)
    (run-hooks *set-location-hook* (current-point))
    (isearch-mode t)
    (setq *isearch-prompt* prompt)
    (setq *isearch-string* initial-string)
    (setq *isearch-search-function* search-func)
    (setq *isearch-start-point* (copy-point (current-point) :temporary))
    (setq *isearch-search-forward-function* search-forward-function)
    (setq *isearch-search-backward-function* search-backward-function)
    (isearch-update-display)
    t))

(define-command isearch-abort () ()
  (move-point (current-point) *isearch-start-point*)
  (isearch-reset-overlays (current-buffer))
  (isearch-end)
  t)

(define-command isearch-delete-char () ()
  (when (plusp (length *isearch-string*))
    (setq *isearch-string*
          (subseq *isearch-string*
                  0
                  (1- (length *isearch-string*))))
    (isearch-update-display)))

(define-command isearch-raw-insert () ()
  (alexandria:when-let ((char (key-to-char (read-key))))
    (isearch-add-char char)))

(defun isearch-end ()
  (isearch-reset-overlays (current-buffer))
  (setq *isearch-prev-string* *isearch-string*)
  (buffer-unbound (current-buffer) 'isearch-redisplay-string)
  (remove-hook (variable-value 'after-change-functions :buffer)
               'isearch-change-buffer-hook)
  (isearch-mode nil)
  t)

(defun isearch-redisplay-inactive (buffer)
  (alexandria:when-let ((string (buffer-value buffer 'isearch-redisplay-string)))
    (isearch-update-buffer (buffer-start-point buffer) string)))

(defun isearch-scroll-hook (window)
  (isearch-redisplay-inactive (window-buffer window)))

(defun isearch-change-buffer-hook (start &rest args)
  (declare (ignore args))
  (isearch-redisplay-inactive (point-buffer start)))

(defun isearch-add-hooks ()
  (add-hook *window-scroll-functions*
            'isearch-scroll-hook)
  (add-hook (variable-value 'after-change-functions :buffer)
            'isearch-change-buffer-hook))

(define-command isearch-finish () ()
  (setf (buffer-value (current-buffer) 'isearch-redisplay-string) *isearch-string*)
  (setq *isearch-prev-string* *isearch-string*)
  (isearch-add-hooks)
  (isearch-redisplay-inactive (current-buffer))
  (isearch-mode nil))

(define-command isearch-next () ()
  (when (boundp '*isearch-string*)
    (when (string= "" *isearch-string*)
      (setq *isearch-string* *isearch-prev-string*))
    (setf (variable-value 'isearch-prev-last :buffer) nil)
    (cond ((variable-value 'isearch-next-last :buffer)
           (setf (variable-value 'isearch-next-last :buffer) nil)
           (with-point ((p (current-point)))
             (buffer-start p)
             (when (funcall *isearch-search-forward-function* p *isearch-string*)
               (move-point (current-point) p))))
          ((not (funcall *isearch-search-forward-function* (current-point) *isearch-string*))
           (setf (variable-value 'isearch-next-last :buffer) t)))
    (isearch-update-display)))

(define-command isearch-prev () ()
  (when (boundp '*isearch-string*)
    (when (string= "" *isearch-string*)
      (setq *isearch-string* *isearch-prev-string*))
    (setf (variable-value 'isearch-next-last :buffer) nil)
    (cond ((variable-value 'isearch-prev-last :buffer)
           (setf (variable-value 'isearch-prev-last :buffer) nil)
           (with-point ((p (current-point)))
             (buffer-end p)
             (when (funcall *isearch-search-backward-function* p *isearch-string*)
               (move-point (current-point) p))))
          ((not (funcall *isearch-search-backward-function* (current-point) *isearch-string*))
           (setf (variable-value 'isearch-prev-last :buffer) t)))
    (isearch-update-display)))

(define-command isearch-yank () ()
  (let ((str (kill-ring-first-string)))
    (when str
      (setq *isearch-string* str)
      (isearch-update-display))))

(defun isearch-add-char (c)
  (setq *isearch-string*
        (concatenate 'string
                     *isearch-string*
                     (string c)))
  (with-point ((start-point (current-point)))
    (unless (funcall *isearch-search-function* (current-point) *isearch-string*)
      (move-point (current-point) start-point)))
  (isearch-update-display)
  t)

(define-command isearch-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (isearch-add-char c))
          (t (isearch-update-display)
             (unread-key-sequence (last-read-key-sequence))
             (isearch-end)))))

(define-command isearch-replace-highlight () ()
  (let ((buffer (current-buffer)))
    (let ((old-string (buffer-value buffer 'isearch-redisplay-string)))
      (unless old-string
        (return-from isearch-replace-highlight))
      (let ((new-string (prompt-for-string "Replace: " old-string)))
        (save-excursion
          (unless (buffer-mark-p buffer) (buffer-start (current-point)))
          (query-replace-internal old-string
                                  new-string
                                  *isearch-search-forward-function*
                                  *isearch-search-backward-function*
                                  :query nil))))))

(define-command isearch-next-highlight (n) ("p")
  (alexandria:when-let ((string (buffer-value (current-buffer) 'isearch-redisplay-string)))
    (let ((search-fn (if (plusp n)
                         *isearch-search-forward-function*
                         *isearch-search-backward-function*)))
      (dotimes (_ (abs n))
        (funcall search-fn (current-point) string)))))

(define-command isearch-prev-highlight (n) ("p")
  (isearch-next-highlight (- n)))

(define-command isearch-toggle-highlighting () ()
  (cond
    ((isearch-overlays (current-buffer))
     (isearch-end))
    ((boundp '*isearch-string*)
     (isearch-update-buffer))))


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

(defun query-replace-internal-body (cur-point goal-point before after query count)
  (let ((pass-through (not query)))
    (loop
      :repeat (or count most-positive-fixnum)
      :do (when (or (not (funcall *isearch-search-forward-function* cur-point before))
                    (and goal-point (point< goal-point cur-point)))
            (when goal-point
              (move-point (current-point) goal-point))
            (return))
          (with-point ((end cur-point :right-inserting))
            (isearch-update-buffer cur-point before)
            (funcall *isearch-search-backward-function* cur-point before)
            (with-point ((start cur-point :right-inserting))
              (loop :for c := (unless pass-through
                                (prompt-for-character (format nil "Replace ~s with ~s" before after)))
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

(defun query-replace-internal (before after search-forward-function search-backward-function
                               &key query (start nil start-p) (end nil end-p) count)
  (let ((buffer (current-buffer)))
    (unwind-protect
         (let ((*isearch-search-forward-function* search-forward-function)
               (*isearch-search-backward-function* search-backward-function))
           (when (and before after)
             (cond ((or start-p end-p)
                    (with-point ((s (or start (buffer-start-point (current-buffer))))
                                 (e (or end (buffer-end-point (current-buffer)))))
                      (query-replace-internal-body s e before after query count)))
                   ((buffer-mark-p buffer)
                    (with-point ((mark-point (buffer-mark buffer) :right-inserting))
                      (cond ((point< mark-point (buffer-point buffer))
                             (query-replace-internal-body mark-point
                                                          (buffer-point buffer)
                                                          before after query count))
                            (t
                             (query-replace-internal-body (buffer-point buffer)
                                                          mark-point
                                                          before after query count)))))
                   (t (query-replace-internal-body (buffer-point buffer)
                                                   nil before after query count)))))
      (isearch-reset-overlays buffer))))

(define-key *global-keymap* "M-%" 'query-replace)

(define-command query-replace (before after)
    ((read-query-replace-args))
  (query-replace-internal before
                          after
                          #'search-forward
                          #'search-backward
                          :query t))

(define-command query-replace-regexp (before after)
    ((read-query-replace-args))
  (query-replace-internal before
                          after
                          #'search-forward-regexp
                          #'search-backward-regexp
                          :query t))

(define-command query-replace-symbol (before after)
    ((read-query-replace-args))
  (query-replace-internal before
                          after
                          #'search-forward-symbol
                          #'search-backward-symbol
                          :query t))

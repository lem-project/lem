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

(defvar *isearch-highlight-attribute* (make-attribute nil nil :reverse-p t))
(defvar *isearch-highlight-active-attribute* (make-attribute "cyan" nil :reverse-p t))

(defun isearch-update-display ()
  (isearch-update-minibuf)
  (isearch-update-buffer (current-marker)))

(defun isearch-update-minibuf ()
  (message "~a~a"
           *isearch-prompt*
           *isearch-string*))

(define-key *global-keymap* (kbd "C-s") 'isearch-forward)
(define-command isearch-forward () ()
  (isearch-start
   "ISearch: "
   (lambda (marker str)
     (search-forward (or (lem::character-offset marker (- (length str)))
                         marker)
                     str))
   #'search-forward
   #'search-backward
   ""))

(define-key *global-keymap* (kbd "C-r") 'isearch-backward)
(define-command isearch-backward () ()
  (isearch-start
   "ISearch:"
   (lambda (marker str)
     (search-backward (or (lem::character-offset marker (length str))
                          marker)
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

(define-key *global-keymap* (kbd "C-x C-M-s") 'isearch-forward-symbol)
(define-command isearch-forward-symbol () ()
  (isearch-start "ISearch Symbol: "
                 #'search-forward-symbol
                 #'search-forward-symbol
                 #'search-backward-symbol
                 ""))

(define-key *global-keymap* (kbd "C-x C-M-r") 'isearch-backward-symbol)
(define-command isearch-backward-symbol () ()
  (isearch-start "ISearch Symbol: "
                 #'search-backward-symbol
                 #'search-forward-symbol
                 #'search-backward-symbol
                 ""))

(define-key *global-keymap* (kbd "C-x .") 'isearch-forward-symbol-at-point)
(define-command isearch-forward-symbol-at-point () ()
  (let ((point (current-marker)))
    (skip-chars-forward point #'syntax-symbol-char-p)
    (skip-chars-backward point #'syntax-symbol-char-p t)
    (skip-chars-backward point #'syntax-symbol-char-p)
    (lem::with-marker ((start point))
      (skip-chars-forward point #'syntax-symbol-char-p)
      (lem::with-marker ((end point))
        (isearch-start "ISearch Symbol: "
                       #'search-forward-symbol
                       #'search-forward-symbol
                       #'search-backward-symbol
                       (lem::points-to-string start end))))))

(defun isearch-start (prompt
                      search-func
                      search-forward-function
                      search-backward-function
                      initial-string)
  (isearch-mode t)
  (setq *isearch-prompt* prompt)
  (setq *isearch-string* initial-string)
  (setq *isearch-search-function* search-func)
  (setq *isearch-start-point* (copy-marker (current-marker) :temporary))
  (setq *isearch-search-forward-function* search-forward-function)
  (setq *isearch-search-backward-function* search-backward-function)
  (isearch-update-display)
  t)

(define-key *isearch-keymap* (kbd "C-g") 'isearch-abort)
(define-command isearch-abort () ()
  (lem::move-point (current-marker) *isearch-start-point*)
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
  (funcall *isearch-search-forward-function* (current-marker) *isearch-string*)
  (isearch-update-display))

(define-key *isearch-keymap* (kbd "C-r") 'isearch-prev)
(define-command isearch-prev () ()
  (when (string= "" *isearch-string*)
    (setq *isearch-string* *isearch-prev-string*))
  (funcall *isearch-search-backward-function* (current-marker) *isearch-string*)
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

(defun isearch-update-buffer (marker &optional (search-string *isearch-string*))
  (isearch-reset-buffer)
  (unless (equal search-string "")
    (window-see (current-window))
    (lem::with-marker ((cur-marker (lem::window-view-marker (current-window)))
                       (limit-marker (or (lem::line-offset
                                          (copy-marker (lem::window-view-marker (current-window))
                                                       :temporary)
                                          (window-height (current-window)))
                                         (lem::buffers-end (window-buffer (current-window))))))
      (loop :while (funcall *isearch-search-forward-function*
                            cur-marker
                            search-string
                            limit-marker)
            :do (let ((start-marker (lem::with-marker ((temp-marker cur-marker :temporary))
                                      (funcall *isearch-search-backward-function*
                                               temp-marker
                                               search-string)
                                      temp-marker)))
                  (push (make-overlay start-marker
                                      (copy-marker cur-marker :temporary)
                                      (if (and (point<= start-marker marker)
                                               (point<= marker cur-marker))
                                          *isearch-highlight-active-attribute*
                                          *isearch-highlight-attribute*))
                        *isearch-highlight-overlays*))))))

(defun isearch-add-char (c)
  (setq *isearch-string*
        (concatenate 'string
                     *isearch-string*
                     (string c)))
  (isearch-update-display)
  (lem::with-marker ((start-marker (current-marker)))
    (unless (funcall *isearch-search-function* (current-marker) *isearch-string*)
      (lem::move-point (current-marker) start-marker)))
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
          (minibuf-read-string
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
    (setq after (minibuf-read-string "After: "))
    (setq *replace-before-string* before)
    (setq *replace-after-string* after)
    (list before after)))

(defun query-replace-internal-body (cur-marker goal-marker before after)
  (let ((pass-through nil))
    (loop
      (when (or (not (funcall *isearch-search-forward-function* cur-marker before))
                (and goal-marker (point< goal-marker cur-marker)))
        (when goal-marker
          (lem::move-point (current-marker) goal-marker))
        (return))
      (lem::with-marker ((end cur-marker))
        (isearch-update-buffer cur-marker before)
        (funcall *isearch-search-backward-function* cur-marker before)
        (lem::with-marker ((start cur-marker))
          (loop :for c := (unless pass-through
                            (minibuf-read-char (format nil "Replace ~s with ~s" before after)))
                :do (cond
                      ((or pass-through (char= c #\y))
                       (lem::delete-between-points start end)
                       (lem::insert-string-at cur-marker after)
                       (return))
                      ((char= c #\n)
                       (lem::move-point cur-marker end)
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
              (lem::with-marker ((mark-marker (lem::buffer-mark-marker buffer) :right-inserting))
                (if (point< mark-marker (lem::buffer-point-marker buffer))
                    (query-replace-internal-body mark-marker
                                                 (lem::buffer-point-marker buffer)
                                                 before after)
                    (query-replace-internal-body (lem::buffer-point-marker buffer)
                                                 mark-marker
                                                 before after)))
              (query-replace-internal-body (lem::buffer-point-marker buffer)
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

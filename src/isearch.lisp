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
  (isearch-update-buffer))

(defun isearch-update-minibuf ()
  (message "~a~a"
           *isearch-prompt*
           *isearch-string*))

(define-key *global-keymap* (kbd "C-s") 'isearch-forward)
(define-command isearch-forward () ()
  (isearch-start
   "ISearch: "
   #'(lambda (str)
       (shift-position (- (length str)))
       (search-forward str))
   #'search-forward
   #'search-backward
   ""))

(define-key *global-keymap* (kbd "C-r") 'isearch-backward)
(define-command isearch-backward () ()
  (isearch-start
   "ISearch:"
   #'(lambda (str)
       (shift-position (length str))
       (search-backward str))
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
  (skip-chars-forward #'syntax-symbol-char-p)
  (skip-chars-backward #'syntax-symbol-char-p t)
  (skip-chars-backward #'syntax-symbol-char-p)
  (let ((start (current-point)))
    (skip-chars-forward #'syntax-symbol-char-p)
    (let ((end (current-point)))
      (isearch-start "ISearch Symbol: "
                     #'search-forward-symbol
                     #'search-forward-symbol
                     #'search-backward-symbol
                     (region-string start end)))))

(defun isearch-start (prompt
                      search-func
                      search-forward-function
                      search-backward-function
                      initial-string)
  (isearch-mode t)
  (setq *isearch-prompt* prompt)
  (setq *isearch-string* initial-string)
  (setq *isearch-search-function* search-func)
  (setq *isearch-start-point* (current-point))
  (setq *isearch-search-forward-function* search-forward-function)
  (setq *isearch-search-backward-function* search-backward-function)
  (isearch-update-display)
  t)

(define-key *isearch-keymap* (kbd "C-g") 'isearch-abort)
(define-command isearch-abort () ()
  (point-set *isearch-start-point*)
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
  (funcall *isearch-search-forward-function* *isearch-string*)
  (isearch-update-display))

(define-key *isearch-keymap* (kbd "C-r") 'isearch-prev)
(define-command isearch-prev () ()
  (when (string= "" *isearch-string*)
    (setq *isearch-string* *isearch-prev-string*))
  (funcall *isearch-search-backward-function* *isearch-string*)
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

(defun isearch-update-buffer (&optional (search-string *isearch-string*))
  (isearch-reset-buffer)
  (unless (equal "" search-string)
    (let ((save-point (current-point))
          start-point
          end-point)
      (with-window-range (start end) (current-window)
        (setq start-point (make-point start 0))
        (setq end-point (make-point (1+ end) 0))
        (point-set start-point)
        (do ()
            ((null
              (funcall *isearch-search-forward-function*
                       search-string end-point)))
          (let ((point2 (current-point))
                (point1 (save-excursion
                         (funcall *isearch-search-backward-function*
                                  search-string)
                         (current-point))))
            (push (make-overlay point1 point2
                                (if (and (point<= point1 save-point)
                                         (point<= save-point point2))
                                    *isearch-highlight-active-attribute*
                                    *isearch-highlight-attribute*))
                  *isearch-highlight-overlays*))))
      (point-set save-point))))

(defun isearch-add-char (c)
  (setq *isearch-string*
        (concatenate 'string
                     *isearch-string*
                     (string c)))
  (isearch-update-display)
  (let ((point (current-point)))
    (unless (funcall *isearch-search-function* *isearch-string*)
      (point-set point))
    t))

(define-command isearch-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (isearch-add-char c))
          (t (isearch-update-display)
             (unread-key-sequence (last-read-key-sequence))
             (isearch-end)))))

(defvar *replace-before-string* nil)
(defvar *replace-after-string* nil)

(defun query-replace-before-after ()
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
             (return-from query-replace-before-after
               (values before after)))
            (t
             (message "Before string is empty")
             (return-from query-replace-before-after
               (values nil nil)))))
    (setq after (minibuf-read-string "After: "))
    (setq *replace-before-string* before)
    (setq *replace-after-string* after)
    (values before after)))

(defun query-replace-internal (search-forward-function
                               search-backward-function)
  (unwind-protect
    (let ((*isearch-search-forward-function* search-forward-function)
          (*isearch-search-backward-function* search-backward-function)
          goal-point)
      (multiple-value-bind (before after)
          (query-replace-before-after)
        (when (and before after)
          (when (buffer-mark-p)
            (let ((begin (region-beginning))
                  (end (region-end)))
              (setq goal-point end)
              (point-set begin)))
          (do ((start-point)
               (end-point)
               (pass-through nil))
              ((or (null (funcall search-forward-function before))
                   (and goal-point (point< goal-point (current-point))))
               (when goal-point
                 (point-set goal-point)))
            (setq end-point (current-point))
            (isearch-update-buffer before)
            (funcall search-backward-function before)
            (setq start-point (current-point))
            ;(unless pass-through (redraw-display))
            (loop
              (let ((c (unless pass-through
                         (minibuf-read-char
                          (format nil "Replace ~s with ~s" before after)))))
                (cond
                 ((or pass-through (char= c #\y))
                  (delete-region start-point end-point)
                  (insert-string after)
                  (return))
                 ((char= c #\n)
                  (point-set end-point)
                  (return))
                 ((char= c #\!)
                  (setq pass-through t)))))))
        t))
    (isearch-reset-buffer)))

(define-key *global-keymap* (kbd "M-%") 'query-replace)
(define-command query-replace () ()
  (query-replace-internal #'search-forward #'search-backward))

(define-command query-replace-regexp () ()
  (query-replace-internal #'search-forward-regexp #'search-backward-regexp))

(define-command query-replace-symbol () ()
  (query-replace-internal #'search-forward-symbol #'search-backward-symbol))

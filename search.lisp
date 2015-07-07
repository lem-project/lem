(in-package :lem)

(export '(*isearch-keymap*
          isearch-forward
          isearch-backward
          query-replace))

(defvar *isearch-keymap* (make-keymap "isearch" 'isearch-undef-hook))
(defvar *isearch-string*)
(defvar *isearch-prev-string* "")
(defvar *isearch-start-point*)
(defvar *isearch-tmp-keymap*)
(defvar *isearch-search-function*)

(defun isearch-update-minibuf ()
  (write-message (format nil "ISearch: ~a" *isearch-string*)))

(define-key *global-keymap* "C-s" 'isearch-forward)
(define-command isearch-forward () ()
  (isearch-start
   (lambda (str)
     (prev-char (length str))
     (search-forward-aux str))))

(define-key *global-keymap* "C-r" 'isearch-backward)
(define-command isearch-backward () ()
  (isearch-start
   (lambda (str)
     (next-char (length str))
     (search-backward-aux str))))

(defun isearch-start (search-func)
  (setq *isearch-tmp-keymap* (current-mode-keymap))
  (set-current-mode-keymap *isearch-keymap*)
  (setq *isearch-string* "")
  (isearch-update-minibuf)
  (setq *isearch-search-function* search-func)
  (setq *isearch-start-point* (point))
  t)

(define-key *isearch-keymap* "C-g" 'isearch-abort)
(define-command isearch-abort () ()
  (point-set *isearch-start-point*)
  t)

(define-key *isearch-keymap* "C-h" 'isearch-delete-char)
(define-command isearch-delete-char () ()
  (setq *isearch-string*
    (subseq *isearch-string*
      0
      (1- (length *isearch-string*))))
  (isearch-update-minibuf))

(define-key *isearch-keymap* "C-q" 'isearch-raw-insert)
(define-command isearch-raw-insert () ()
  (isearch-add-char (getch)))

(define-key *isearch-keymap* "C-j" 'isearch-end)
(define-key *isearch-keymap* "C-m" 'isearch-end)
(define-command isearch-end () ()
  (setq *isearch-prev-string* *isearch-string*)
  (set-current-mode-keymap *isearch-tmp-keymap*))

(define-key *isearch-keymap* "C-s" 'isearch-next)
(define-command isearch-next () ()
  (when (string= "" *isearch-string*)
    (setq *isearch-string* *isearch-prev-string*))
  (search-forward-aux *isearch-string*)
  (isearch-update-minibuf))

(define-key *isearch-keymap* "C-r" 'isearch-prev)
(define-command isearch-prev () ()
  (when (string= "" *isearch-string*)
    (setq *isearch-string* *isearch-prev-string*))
  (search-backward-aux *isearch-string*)
  (isearch-update-minibuf))

(define-key *isearch-keymap* "C-y" 'isearch-yank)
(define-command isearch-yank () ()
  (let ((str (caar *kill-ring-yank-ptr*)))
    (when str
      (setq *isearch-string* str)
      (isearch-update-minibuf))))

(defun isearch-add-char (c)
  (setq *isearch-string*
    (concatenate 'string
      *isearch-string*
      (string c)))
  (isearch-update-minibuf)
  (let ((point (point)))
    (unless (funcall *isearch-search-function* *isearch-string*)
      (point-set point))))

(defun isearch-undef-hook (key)
  (let ((c (insertion-key-p key)))
    (if c
      (isearch-add-char c)
      (progn
       (mapc 'ungetch key)
       (isearch-end)))))

(defun search-step (str first-search search step goto-matched-pos endp)
  (let ((point (point))
        (result
         (let ((res (funcall first-search)))
           (if res
             (progn
              (funcall goto-matched-pos res)
              t)
             (do () ((funcall endp))
               (funcall step)
               (let ((res (funcall search)))
                 (when res
                   (funcall goto-matched-pos res)
                   (return t))))))))
    (unless result
      (point-set point))
    result))

(defun search-forward-aux (str)
  (let* ((lines (split-string str #\newline))
         (length (length lines)))
    (flet ((take-string ()
             (join (string #\newline)
               (buffer-take-lines (window-buffer)
                 (window-cur-linum)
                 length))))
      (search-step str
        (lambda ()
          (search str (take-string)
            :start2 (window-cur-col)))
        (lambda ()
          (search str (take-string)))
        (lambda () (next-line 1))
        (lambda (i)
          (beginning-of-line)
          (next-char (+ i (length str))))
        #'eobp))))

(defun search-backward-aux (str)
  (let* ((lines (split-string str #\newline))
         (length (length lines)))
    (flet ((%search (&rest args)
             (let ((linum (- (window-cur-linum) (1- length))))
               (when (< 0 linum)
                 (apply 'search str
                   (join (string #\newline)
                     (buffer-take-lines (window-buffer)
                       linum
                       length))
                   :from-end t
                   args)))))
      (search-step str
        (lambda ()
          (%search :end2 (window-cur-col)))
        (lambda ()
          (%search))
        (lambda () (prev-line 1))
        (lambda (i)
          (prev-line (1- length))
          (beginning-of-line)
          (next-char i))
        #'bobp))))

(define-key *global-keymap* "M-C-r" 'query-replace)
(define-command query-replace (before after) ("sBefore: " "sAfter: ")
  (let ((n (length before))
        (pass-through))
    (do () (nil)
      (unless (search-forward-aux before)
        (return))
      (write-message (format nil "Replace ~s with ~s" before after))
      (prev-char n)
      (unless pass-through (window-update-all))
      (do () (nil)
        (let ((c (unless pass-through (getch))))
          (cond
           ((or pass-through (char= c #\y))
            (buffer-delete-char (window-buffer)
              (window-cur-linum) (window-cur-col) n)
            (insert-string after)
            (return))
           ((char= c #\n)
            (next-char n)
            (return))
           ((char= c #\!)
            (setq pass-through t)))))))
  (clear-message-line)
  t)

(in-package :lem)

(defvar *isearch-keymap* (make-keymap 'isearch-undef-hook))
(defvar *isearch-string*)
(defvar *isearch-start-point*)
(defvar *isearch-tmp-keymap*)
(defvar *isearch-search-function*)

(defun isearch-update-minibuf ()
  (mb-write (format nil "ISearch: ~a" *isearch-string*)))

(define-key *global-keymap* "C-s" 'isearch-forward)
(defcommand isearch-forward () ()
  (isearch-start
   (lambda (str)
     (prev-char (length str))
     (search-forward-aux str))))

(define-key *global-keymap* "C-r" 'isearch-backward)
(defcommand isearch-backward () ()
  (isearch-start
   (lambda (str)
     (next-char (length str))
     (search-backward-aux str))))

(defun isearch-start (search-func)
  (setq *isearch-tmp-keymap* *current-keymap*)
  (setq *current-keymap* *isearch-keymap*)
  (setq *isearch-string* "")
  (isearch-update-minibuf)
  (setq *isearch-search-function* search-func)
  (setq *isearch-start-point* (point))
  t)

(define-key *isearch-keymap* "C-g" 'isearch-abort)
(defcommand isearch-abort () ()
  (point-set *isearch-start-point*)
  t)

(define-key *isearch-keymap* "C-h" 'isearch-delete-char)
(defcommand isearch-delete-char () ()
  (setq *isearch-string*
    (subseq *isearch-string*
      0
      (1- (length *isearch-string*))))
  (isearch-update-minibuf))

(define-key *isearch-keymap* "C-s" 'isearch-next)
(defcommand isearch-next () ()
  (search-forward-aux *isearch-string*)
  (isearch-update-minibuf))

(define-key *isearch-keymap* "C-r" 'isearch-prev)
(defcommand isearch-prev () ()
  (search-backward-aux *isearch-string*)
  (isearch-update-minibuf))

(defun isearch-add-char (c)
  (setq *isearch-string*
    (concatenate 'string
      *isearch-string*
      (string c)))
  (isearch-update-minibuf)
  (let ((point (point)))
    (unless (funcall *isearch-search-function* *isearch-string*)
      (point-set point))))

(defun isearch-undef-hook (keys)
  (let ((c (insertion-key-p keys)))
    (if c
      (isearch-add-char c)
      (progn
       (mapc 'ungetch keys)
       (setq *current-keymap* 
         *isearch-tmp-keymap*)))))

(defun search-step (str first-search search step get-col endp)
  (let ((point (point))
        (result
         (let ((res (funcall first-search str (current-line-string))))
           (if res
             (progn
              (goto-column (funcall get-col res))
              t)
             (do () ((funcall endp))
               (funcall step)
               (let ((res (funcall search str (current-line-string))))
                 (when res
                   (goto-column (funcall get-col res))
                   (return t))))))))
    (unless result
      (point-set point))
    result))

(defun search-forward-aux (str)
  (search-step str
    (lambda (str line)
      (search str line
        :start2 (window-cur-col)))
    #'search
    (lambda () (next-line 1))
    (lambda (i) (+ i (length str)))
    #'eobp))

(define-key *global-keymap* "M-s" 'search-forward)
(defcommand search-forward (str) ("sSearch forward: ")
  (or (search-forward-aux str)
    (progn
     (mb-write "Not found")
     nil)))

(defun search-backward-aux (str)
  (search-step str
    (lambda (str line)
      (search str line
        :end2 (window-cur-col)
        :from-end t))
    (lambda (str line)
      (search str line
        :from-end t))
    (lambda () (prev-line 1))
    #'identity
    #'bobp))

(define-key *global-keymap* "M-r" 'search-backward)
(defcommand search-backward (str) ("sSearch backward: ")
  (or (search-backward-aux str)
    (progn
     (mb-write "Not found")
     nil)))

(define-key *global-keymap* "M-C-r" 'query-replace)
(defcommand query-replace (before after) ("sBefore: " "sAfter: ")
  (let ((n (length before))
        (pass-through))
    (do () (nil)
      (unless (search-forward-aux before)
        (return))
      (mb-write (format nil "Replace ~s with ~s" before after))
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
  (mb-clear)
  t)

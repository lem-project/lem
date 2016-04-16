;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(unmark-buffer
          toggle-read-only
          rename-buffer
          quoted-insert
          newline
          open-line
          delete-next-char
          delete-previous-char
          kill-line
          next-line
          prev-line
          next-char
          prev-char
          move-to-beginning-of-buffer
          move-to-end-of-buffer
          move-to-beginning-of-line
          move-to-end-of-line
          next-page
          prev-page
          entab-line
          detab-line
          newline-and-indent
          next-page-char
          prev-page-char
          delete-blank-lines
          just-one-space
          delete-indentation
          transpose-characters
          back-to-indentation
          undo
          redo
          mark-set
          exchange-point-mark
          filter-buffer
          pipe-command))

(define-key *global-keymap* (kbd "M-~") 'unmark-buffer)
(define-command unmark-buffer () ()
  (buffer-unmark (current-buffer))
  t)

(define-key *global-keymap* (kbd "C-x C-q") 'toggle-read-only)
(define-command toggle-read-only () ()
  (setf (buffer-read-only-p (current-buffer))
        (not (buffer-read-only-p (current-buffer))))
  t)

(define-command rename-buffer (name) ("sRename buffer: ")
  (buffer-rename (current-buffer) name)
  t)

(define-key *global-keymap* (kbd "C-q") 'quoted-insert)
(define-command quoted-insert (&optional (n 1)) ("p")
  (let ((c (getch)))
    (dotimes (_ n t)
      (cond ((char= c C-m)
             (insert-newline 1))
            ((char= c C-d)
             (delete-char 1 nil))
            (t
             (insert-char c 1))))))

(define-key *global-keymap* (kbd "C-m") 'newline)
(define-command newline (&optional (n 1)) ("p")
  (insert-newline n))

(define-key *global-keymap* (kbd "C-o") 'open-line)
(define-command open-line (n) ("p")
  (insert-newline n)
  (shift-position (- n)))

(define-key *global-keymap* (kbd "C-d") 'delete-next-char)
(define-key *global-keymap* (kbd "[dc]") 'delete-next-char)
(define-command delete-next-char (&optional n) ("P")
  (delete-char (or n 1)
               (if n t nil)))

(define-key *global-keymap* (kbd "C-h") 'delete-previous-char)
(define-key *global-keymap* (kbd "[backspace]") 'delete-previous-char)
(define-key *global-keymap* (kbd "[del]") 'delete-previous-char)
(define-command delete-previous-char (&optional n) ("P")
  (delete-char (if n (- n) -1)
               (if n t nil)))

(define-key *global-keymap* (kbd "C-k") 'kill-line)
(define-command kill-line (&optional (n 1)) ("p")
  (kill-region (current-point)
               (dotimes (_ n (current-point))
                 (cond ((eolp)
                        (next-line 1)
                        (beginning-of-line))
                       (t
                        (end-of-line))))))

(defvar *next-line-prev-column* nil)

(define-key *global-keymap* (kbd "C-n") 'next-line)
(define-key *global-keymap* (kbd "[down]") 'next-line)
(define-command next-line (&optional n) ("p")
  (when-interrupted-flag :next-line
    (setq *next-line-prev-column* (current-column)))
  (unless (prog1 (forward-line n)
            (move-to-column *next-line-prev-column*))
    (cond ((plusp n)
           (end-of-buffer)
           (editor-error "End of buffer"))
          (t
           (beginning-of-buffer)
           (editor-error "Beginning of buffer"))))
  t)

(define-key *global-keymap* (kbd "C-p") 'prev-line)
(define-key *global-keymap* (kbd "[up]") 'prev-line)
(define-command prev-line (&optional n) ("p")
  (next-line (- n)))

(define-key *global-keymap* (kbd "C-f") 'next-char)
(define-key *global-keymap* (kbd "[right]") 'next-char)
(define-command next-char (&optional (n 1)) ("p")
  (or (shift-position n)
      (editor-error "End of buffer")))

(define-key *global-keymap* (kbd "C-b") 'prev-char)
(define-key *global-keymap* (kbd "[left]") 'prev-char)
(define-command prev-char (&optional (n 1)) ("p")
  (or (shift-position (- n))
      (editor-error "Beginning of buffer")))

(define-key *global-keymap* (kbd "M-<") 'move-to-beginning-of-buffer)
(define-command move-to-beginning-of-buffer () ()
  (beginning-of-buffer)
  t)

(define-key *global-keymap* (kbd "M->") 'move-to-end-of-buffer)
(define-command move-to-end-of-buffer () ()
  (end-of-buffer)
  t)

(define-key *global-keymap* (kbd "C-a") 'move-to-beginning-of-line)
(define-key *global-keymap* (kbd "[home]") 'move-to-beginning-of-line)
(define-command move-to-beginning-of-line () ()
  (beginning-of-line)
  t)

(define-key *global-keymap* (kbd "C-e") 'move-to-end-of-line)
(define-key *global-keymap* (kbd "[end]") 'move-to-end-of-line)
(define-command move-to-end-of-line () ()
  (end-of-line)
  t)

(define-key *global-keymap* (kbd "C-v") 'next-page)
(define-key *global-keymap* (kbd "[npage]") 'next-page)
(define-command next-page (&optional n) ("P")
  (if n
      (scroll-down n)
      (let ((point (current-point)))
        (cond ((forward-line (1- (window-height)))
               (window-recenter (current-window))
               t)
              ((and (point-set point) nil))
              ((not (eobp))
               (end-of-buffer)
               (window-recenter (current-window))
               t)))))

(define-key *global-keymap* (kbd "M-v") 'prev-page)
(define-key *global-keymap* (kbd "[ppage]") 'prev-page)
(define-command prev-page (&optional n) ("P")
  (if n
      (scroll-up n)
      (let ((point (current-point)))
        (cond ((forward-line (- (1- (window-height))))
               (window-recenter (current-window))
               t)
              ((and (point-set point) nil))
              ((not (bobp))
               (beginning-of-buffer)
               (window-recenter (current-window))
               t)))))

(defun tab-line-aux (n make-space-str)
  (dotimes (_ n t)
    (let ((count (save-excursion
                   (back-to-indentation)
                   (current-column))))
      (multiple-value-bind (div mod)
          (floor count *tab-size*)
        (beginning-of-line)
        (delete-while-whitespaces t nil)
        (insert-string (funcall make-space-str div))
        (insert-char #\space mod)))
    (unless (forward-line 1)
      (return))))

(define-command entab-line (n) ("p")
  (tab-line-aux n
                #'(lambda (n)
                    (make-string n :initial-element #\tab))))

(define-command detab-line (n) ("p")
  (tab-line-aux n
                #'(lambda (n)
                    (make-string (* n *tab-size*) :initial-element #\space))))

(define-key *global-keymap* (kbd "C-j") 'newline-and-indent)
(define-command newline-and-indent (n) ("p")
  (dotimes (_ n t)
    (let ((spaces (region-string (make-point (current-linum) 0)
                                 (save-excursion
                                   (back-to-indentation)
                                   (current-point)))))
      (unless (and (newline)
                   (insert-string spaces))
        (return nil)))))

(define-key *global-keymap* (kbd "C-x ]") 'next-page-char)
(define-command next-page-char (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (when (eql (char-code #\page) (following-char))
      (shift-position 1))
    (unless (search-forward (string #\page))
      (end-of-buffer)
      (return nil))))

(define-key *global-keymap* (kbd "C-x [") 'prev-page-char)
(define-command prev-page-char (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (when (eql (char-code #\page) (preceding-char))
      (shift-position -1))
    (unless (search-backward (string #\page))
      (beginning-of-buffer)
      (return nil))))

(define-key *global-keymap* (kbd "C-x C-o") 'delete-blank-lines)
(define-command delete-blank-lines () ()
  (do ()
      ((not (blank-line-p))
       (forward-line 1))
    (unless (forward-line -1)
      (return)))
  (do () ((eobp))
    (let ((result (blank-line-p)))
      (unless (and result (delete-char result nil))
        (return)))))

(define-key *global-keymap* (kbd "M-Spc") 'just-one-space)
(define-command just-one-space () ()
  (skip-chars-backward '(#\space #\tab))
  (delete-while-whitespaces t nil)
  (insert-char #\space 1)
  t)

(define-key *global-keymap* (kbd "M-^") 'delete-indentation)
(define-command delete-indentation () ()
  (beginning-of-line)
  (let ((point (current-point)))
    (forward-line -1)
    (end-of-line)
    (delete-char (region-count (current-point) point) nil)
    (just-one-space)
    t))

(define-key *global-keymap* (kbd "C-t") 'transpose-characters)
(define-command transpose-characters () ()
  (cond ((bolp))
        ((eolp)
         (let* ((c1 (char-before 1))
                (c2 (char-before 2)))
           (unless (eql c2 #\newline)
             (delete-char -2 nil)
             (insert-char c1 1)
             (insert-char c2 1))))
        (t
         (let* ((c1 (following-char))
                (c2 (preceding-char)))
           (delete-char 1 nil)
           (delete-char -1 nil)
           (insert-char c1 1)
           (insert-char c2 1)))))

(define-key *global-keymap* (kbd "M-m") 'back-to-indentation)
(define-command back-to-indentation () ()
  (beginning-of-line)
  (skip-chars-forward '(#\space #\tab))
  t)

(define-key *global-keymap* (kbd "C-\\") 'undo)
(define-command undo () ()
  (let ((point (buffer-undo (current-buffer))))
    (cond
      (point
       (point-set point)
       t)
      (t
       (editor-error "Undo Error")))))

(define-key *global-keymap* (kbd "C-_") 'redo)
(define-command redo () ()
  (let ((point (buffer-redo (current-buffer))))
    (cond
      (point
       (point-set point)
       t)
      (t
       (editor-error "Redo Error")))))

(define-key *global-keymap* (kbd "C-@") 'mark-set)
(define-command mark-set () ()
  (setf (mark-point) (current-point))
  (minibuf-print "Mark set"))

(define-key *global-keymap* (kbd "C-x C-x") 'exchange-point-mark)
(define-command exchange-point-mark () ()
  (check-marked)
  (let ((mark-point (mark-point)))
    (setf (mark-point) (current-point))
    (point-set mark-point))
  t)

(define-key *global-keymap* (kbd "C-x #") 'filter-buffer)
(define-command filter-buffer (str) ("sFilter buffer: ")
  (let (begin end)
    (cond ((buffer-mark-p)
           (setq begin (region-beginning))
           (setq end (region-end)))
          (t
           (setq begin (point-min))
           (setq end (point-max))))
    (let ((input-string
           (region-string begin end))
          (outstr (make-array '(0)
                              :element-type 'character
                              :fill-pointer t))
          output-value
          error-output-value
          status)
      (with-output-to-string (output outstr)
        (with-input-from-string (input input-string)
          (multiple-value-setq (output-value error-output-value status)
                               (uiop:run-program str
                                                 :input input
                                                 :output output
                                                 :ignore-error-status t))))
      (delete-region begin end)
      (insert-string outstr)
      (point-set begin)
      (minibuf-print (format nil "~D ~A" (write-to-string status) error-output-value))
      (zerop status))))

(define-key *global-keymap* (kbd "C-x @") 'pipe-command)
(define-command pipe-command (str) ("sPipe command: ")
  (info-popup (get-buffer-create "*Command*")
              #'(lambda (out)
                  (uiop:run-program str
                                    :output out
                                    :ignore-error-status t))))

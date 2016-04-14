;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

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
  (prev-char n))

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

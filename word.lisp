;; -*- Mode: Lisp; Package: Lem -*-

(in-package :lem)

(export '(next-word
          prev-word
          delete-word
          backward-delete-word
          case-word-capitalize
          case-word-lower
          case-word-upper
          forward-paragraph
          backward-paragraph
          kill-paragraph))

(defun in-word-p (c)
  (and c (alphanumericp c)))

(defun next-word-aux (fn)
  (do () ((in-word-p (following-char)))
    (unless (funcall fn)
      (return)))
  (do () ((not (in-word-p (following-char))) t)
    (unless (funcall fn)
      (return))))

(defun prev-word-aux (fn)
  (do () ((in-word-p (preceding-char)))
    (unless (funcall fn)
      (return)))
  (do () ((not (in-word-p (preceding-char))) t)
    (unless (funcall fn)
      (return))))

(define-key *global-keymap* (kbd "M-f") 'next-word)
(define-key *global-keymap* (kbd "C-right") 'next-word)
(define-command next-word (n) ("p")
  (if (minusp n)
      (prev-word (- n))
      (dotimes (_ n t)
        (unless (next-word-aux 'next-char)
          (return)))))

(define-key *global-keymap* (kbd "M-b") 'prev-word)
(define-key *global-keymap* (kbd "C-left") 'prev-word)
(define-command prev-word (n) ("p")
  (if (minusp n)
      (next-word (- n))
      (dotimes (_ n t)
        (unless (prev-word-aux 'prev-char)
          (return)))))

(define-key *global-keymap* (kbd "M-d") 'delete-word)
(define-key *global-keymap* (kbd "C-dc") 'delete-word)
(define-command delete-word (n) ("p")
  (if (minusp n)
      (backward-delete-word (- n))
      (dotimes (_ n t)
        (unless (next-word-aux #'(lambda () (delete-char 1)))
          (return)))))

(define-key *global-keymap* (kbd "M-C-h") 'backward-delete-word)
(define-key *global-keymap* (kbd "M-[backspace]") 'backward-delete-word)
(define-command backward-delete-word (n) ("p")
  (if (minusp n)
      (delete-word (- n))
      (let ((*kill-before-p* t))
        (dotimes (_ n t)
          (unless (prev-word-aux #'(lambda () (backward-delete-char 1)))
            (return))))))

(defun case-word-aux (n first-case rest-case)
  (dotimes (_ n t)
    (do () ((in-word-p (following-char)))
      (unless (next-char)
        (return-from case-word-aux nil)))
    (replace-char (funcall first-case (following-char)))
    (do () ((not (in-word-p (following-char))))
      (unless (next-char)
        (return-from case-word-aux nil))
      (when (in-word-p (following-char))
        (replace-char (funcall rest-case (following-char)))))))

(define-key *global-keymap* (kbd "M-c") 'case-word-capitalize)
(define-command case-word-capitalize (&optional (n 1)) ("p")
  (case-word-aux n 'char-upcase 'char-downcase))

(define-key *global-keymap* (kbd "M-l") 'case-word-lower)
(define-command case-word-lower (&optional (n 1)) ("p")
  (case-word-aux n 'char-downcase 'char-downcase))

(define-key *global-keymap* (kbd "M-u") 'case-word-upper)
(define-command case-word-upper (&optional (n 1)) ("p")
  (case-word-aux n 'char-upcase 'char-upcase))

(define-key *global-keymap* (kbd "M-}") 'forward-paragraph)
(define-key *global-keymap* (kbd "C-down") 'forward-paragraph)
(define-command forward-paragraph (&optional (n 1)) ("p")
  (block outer
    (let ((dir (if (minusp n) -1 1)))
      (dotimes (_ (abs n) t)
        (loop :while (blank-line-p) :do
          (unless (next-line dir)
            (return-from outer t)))
        (loop :until (blank-line-p) :do
          (unless (next-line dir)
            (return-from outer t)))))))

(define-key *global-keymap* (kbd "M-{") 'backward-paragraph)
(define-key *global-keymap* (kbd "C-up") 'backward-paragraph)
(define-command backward-paragraph (&optional (n 1)) ("p")
  (forward-paragraph (- n)))

(define-key *global-keymap* (kbd "M-k") 'kill-paragraph)
(define-command kill-paragraph (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (let ((point (point)))
      (unless (and (forward-paragraph)
                   (kill-region point (point)))
        (return)))))

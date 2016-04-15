;; -*- Mode: LISP; Package: LEM -*-

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

(defun word-type (char)
  (when (characterp char)
    (cond ((char<= (code-char 12354) ;#\HIRAGANA_LETTER_A
                   char
                   (code-char 12435) ;#\HIRAGANA_LETTER_N
                   )
           :hiragana)
          ((char<= (code-char 12450) ;#\KATAKANA_LETTER_A
                   char
                   (code-char 12531) ;#\KATAKANA_LETTER_N
                   )
           :katakana)
          ((or (<= #x4E00
                   (char-code char)
                   #x9FFF)
               (find char "仝々〆〇ヶ"))
           :kanji)
          ((alphanumericp char)
           :alphanumeric))))

(defun next-word-aux (fn)
  (do () ((word-type (following-char)))
    (unless (funcall fn)
      (return)))
  (do ((type #1=(word-type (following-char))))
      ((not (eq type #1#)) t)
    (unless (funcall fn)
      (return))))

(defun prev-word-aux (fn)
  (do () ((word-type (preceding-char)))
    (unless (funcall fn)
      (return)))
  (do ((type #1=(word-type (preceding-char))))
      ((not (eq type #1#)) t)
    (unless (funcall fn)
      (return))))

(define-key *global-keymap* (kbd "M-f") 'next-word)
(define-key *global-keymap* (kbd "C-right") 'next-word)
(define-command next-word (n) ("p")
  (if (minusp n)
      (prev-word (- n))
      (dotimes (_ n t)
        (unless (next-word-aux (lambda () (shift-position 1)))
          (return)))))

(define-key *global-keymap* (kbd "M-b") 'prev-word)
(define-key *global-keymap* (kbd "C-left") 'prev-word)
(define-command prev-word (n) ("p")
  (if (minusp n)
      (next-word (- n))
      (dotimes (_ n t)
        (unless (prev-word-aux (lambda () (shift-position -1)))
          (return)))))

(define-key *global-keymap* (kbd "M-d") 'delete-word)
(define-key *global-keymap* (kbd "C-dc") 'delete-word)
(define-command delete-word (n) ("p")
  (if (minusp n)
      (backward-delete-word (- n))
      (let ((begin (current-point))
            (end (progn (next-word n) (current-point))))
        (cond
          ((point= begin end)
           nil)
          (t
           (kill-region begin end)
           t)))))

(define-key *global-keymap* (kbd "M-C-h") 'backward-delete-word)
(define-key *global-keymap* (kbd "M-[backspace]") 'backward-delete-word)
(define-command backward-delete-word (n) ("p")
  (if (minusp n)
      (delete-word (- n))
      (let ((*kill-before-p* t))
        (let ((end (current-point))
              (begin (progn (prev-word n) (current-point))))
          (cond
            ((point= begin end)
             nil)
            (t
             (kill-region begin end)
             t))))))

(defun case-word-aux (n replace-char-p first-case rest-case)
  (dotimes (_ n t)
    (do () ((word-type (following-char)))
      (unless (shift-position 1)
        (return-from case-word-aux nil)))
    (replace-char (funcall first-case (following-char)))
    (do ((type #1=(word-type (following-char))))
        ((not (eq type #1#)))
      (unless (shift-position 1)
        (return-from case-word-aux nil))
      (when (funcall replace-char-p (following-char))
        (replace-char (funcall rest-case (following-char)))))))

(define-key *global-keymap* (kbd "M-c") 'case-word-capitalize)
(define-command case-word-capitalize (&optional (n 1)) ("p")
  (case-word-aux n #'alphanumericp #'char-upcase #'char-downcase))

(define-key *global-keymap* (kbd "M-l") 'case-word-lower)
(define-command case-word-lower (&optional (n 1)) ("p")
  (case-word-aux n #'alphanumericp #'char-downcase #'char-downcase))

(define-key *global-keymap* (kbd "M-u") 'case-word-upper)
(define-command case-word-upper (&optional (n 1)) ("p")
  (case-word-aux n #'alphanumericp #'char-upcase #'char-upcase))

(define-key *global-keymap* (kbd "M-}") 'forward-paragraph)
(define-command forward-paragraph (&optional (n 1)) ("p")
  (block outer
    (let ((dir (if (minusp n) -1 1)))
      (dotimes (_ (abs n) t)
        (loop :while (blank-line-p) :do
          (unless (forward-line dir)
            (return-from outer t)))
        (loop :until (blank-line-p) :do
          (unless (forward-line dir)
            (return-from outer t)))))))

(define-key *global-keymap* (kbd "M-{") 'backward-paragraph)
(define-command backward-paragraph (&optional (n 1)) ("p")
  (forward-paragraph (- n)))

(define-key *global-keymap* (kbd "M-k") 'kill-paragraph)
(define-command kill-paragraph (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (let ((point (current-point)))
      (unless (and (forward-paragraph)
                   (kill-region point (current-point)))
        (return)))))

(in-package :lem)

(export '(next-word
          prev-word
          delete-word
          backward-delete-word
          case-word-capitalize
          case-word-lower
          case-word-upper))

(defun in-word-p (c)
  (and c (alphanumericp c)))

(defun next-word-aux (fn)
  (do () ((in-word-p (following-char)))
    (unless (funcall fn)
      (return)))
  (do () ((not (in-word-p (following-char))))
    (unless (funcall fn)
      (return))))

(defun prev-word-aux (fn)
  (do () ((in-word-p (preceding-char)))
    (unless (funcall fn)
      (return)))
  (do () ((not (in-word-p (preceding-char))))
    (unless (funcall fn)
      (return))))

(define-key *global-keymap* (kbd "M-f") 'next-word)
(define-command next-word (n) ("p")
  (if (minusp n)
    (prev-word (- n))
    (dotimes (_ n t)
      (unless (next-word-aux 'next-char)
        (return)))))

(define-key *global-keymap* (kbd "M-b") 'prev-word)
(define-command prev-word (n) ("p")
  (if (minusp n)
    (next-word (- n))
    (dotimes (_ n t)
      (unless (prev-word-aux 'prev-char)
        (return)))))

(define-key *global-keymap* (kbd "M-d") 'delete-word)
(define-command delete-word (n) ("p")
  (if (minusp n)
    (backward-delete-word (- n))
    (dotimes (_ n t)
      (unless (next-word-aux (lambda () (delete-char 1)))
        (return)))))

(define-key *global-keymap* (kbd "M-C-h") 'backward-delete-word)
(define-command backward-delete-word (n) ("p")
  (if (minusp n)
    (delete-word (- n))
    (let ((*kill-before-p* t))
      (dotimes (_ n t)
        (unless (prev-word-aux (lambda () (backward-delete-char 1)))
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

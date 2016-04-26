(in-package :lem)

(export '(abbrev))

(defun preceding-word ()
  (let ((chars))
    (save-excursion
     (skip-chars-backward
      #'(lambda (c)
          (when (and c (syntax-symbol-char-p c))
            (push c chars))))
     (coerce chars 'string))))

(defun scan-line-words (str)
  (let ((words))
    (do ((i 0 (1+ i)))
        ((>= i (length str)))
      (when (syntax-symbol-char-p (aref str i))
        (push (subseq str i
                      (do ((j i (1+ j)))
                          ((or (>= j (length str))
                               (not (syntax-symbol-char-p (aref str j))))
                           (setq i j)
                           j)))
              words)))
    (nreverse words)))

(defvar *abbrev-words* nil)
(defvar *abbrev-save-word* nil)

(defun scan-buffer-words (buffer word)
  (let ((words))
    (map-buffer-lines
     #'(lambda (str eof-p linum)
         (declare (ignore eof-p linum))
         (dolist (w (remove-if-not #'(lambda (tok)
                                       (and (string/= word tok)
                                            (eql 0 (search word tok))))
                                   (scan-line-words str)))
           (push w words)))
     buffer
     1)
    (nreverse words)))

(defun scan-all-buffer-words (word)
  (remove-duplicates
   (nconc (scan-buffer-words (current-buffer) word)
          (mapcan #'(lambda (buffer)
                      (unless (eq buffer (current-buffer))
                        (scan-buffer-words buffer word)))
                  (buffer-list)))
   :test #'equal))

(define-key *global-keymap* (kbd "M-/") 'abbrev)
(let ((save-words))
  (define-command abbrev () ()
    (let ((first nil))
      (unless (continue-flag :abbrev) (setq first t))
      (if first
          (let ((src-word (preceding-word)))
            (setq *abbrev-save-word* src-word)
            (setq save-words
                  (setq *abbrev-words*
                        (scan-all-buffer-words src-word)))
            (unless save-words
              (setq *abbrev-words* (list src-word))
              (setq save-words *abbrev-words*))
            (delete-char (- (length src-word)) nil)
            (insert-string (pop save-words)))
          (let ((src-word (preceding-word)))
            (unless save-words
              (setq save-words *abbrev-words*))
            (let ((dst-word (pop save-words)))
              (delete-char (- (length src-word)) t)
              (insert-string dst-word)))))))

(in-package :cl-user)
(defpackage :lem.abbrev
  (:use :cl :lem)
  (:export :abbrev))
(in-package :lem.abbrev)

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

(defun scan-buffer-words (buffer word)
  (let ((words))
    (with-open-stream (in (make-buffer-input-stream (lem::buffer-start buffer)))
      (loop :for str := (read-line in nil)
            :while str
            :do (dolist (w (remove-if-not #'(lambda (tok)
                                              (and (string/= word tok)
                                                   (eql 0 (search word tok))))
                                          (scan-line-words str)))
                  (push w words))))
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
(define-command abbrev () ()
  (let ((src-word (preceding-word)))
    (let ((words (scan-all-buffer-words src-word)))
      (start-completion (lambda (str)
                          (completion str words))
                        src-word))))

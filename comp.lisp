(in-package :lem)

(export '(completion
          popup-completion
          delete-completion-window
          abbrev))

(defvar *comp-buffer-name* "*Completion*")
(defvar *completion-window* nil)
(defvar *completion-flag* nil)
(defvar *comp-popup-window* nil)

(defun completion (name list)
  (let ((strings (remove-if-not (lambda (elt)
                                  (and (<= (length name) (length elt))
                                    (string= name elt
                                      :end2 (length name))))
                   list)))
    (cond
     ((null strings) nil)
     ((null (cdr strings)) (car strings))
     (t
      (let* ((str (car strings))
             (len (length str)))
        (dolist (s (cdr strings))
          (let ((res (mismatch str s :end1 len)))
            (when res
              (setq len res))))
        (values (subseq str 0 len) strings))))))

(defun popup-completion (comp-f str)
  (multiple-value-bind (result strings) (funcall comp-f str)
    (when strings
      (setq *completion-flag* t)
      (multiple-value-bind (win newwin-p)
          (popup (get-buffer-create *comp-buffer-name*)
                 (lambda ()
                   (setq *completion-window* *current-window*)
                   (dolist (s strings)
                     (buffer-append-line (window-buffer) s))))
        (declare (ignore win))
        (setq *comp-popup-window* newwin-p))
      (window-update-all))
    (or result str)))

(defun delete-completion-window ()
  (when *completion-flag*
    (setq *completion-flag* nil)
    (dolist (win *window-list*)
      (when (and (eq win *completion-window*)
                 (string= (buffer-name (window-buffer win))
                          *comp-buffer-name*))
        (if *comp-popup-window*
          (delete-window win)
          (let ((*current-window* *completion-window*))
            (set-buffer (car *buffer-list*))))
        (return)))))

(defun preceding-word ()
  (let ((chars))
    (save-excursion
     (skip-chars-backward
      (lambda (c)
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
     (lambda (str eof-p linum)
       (declare (ignore eof-p linum))
       (dolist (w (remove-if-not (lambda (tok)
                                   (eql 0 (search word tok)))
                                 (scan-line-words str)))
         (push w words)))
     buffer
     1)
    (nreverse words)))

(defun scan-all-buffer-words (word)
  (remove-duplicates
   (nconc (scan-buffer-words (window-buffer) word)
          (mapcan (lambda (buffer)
                    (unless (eq buffer (window-buffer))
                      (scan-buffer-words buffer word)))
                  *buffer-list*))
   :test #'equal))

(define-key *global-keymap* "M-/" 'abbrev)
(let ((save-words))
  (define-command abbrev () ()
    (let ((first nil))
      (when-interrupted-flag :abbrev (setq first t))
      (if first
        (let ((src-word (preceding-word)))
          (setq *abbrev-save-word* src-word)
          (setq save-words
                (setq *abbrev-words*
                      (scan-all-buffer-words src-word)))
          (backward-delete-char (length src-word) t)
          (insert-string (pop save-words)))
        (let ((src-word (preceding-word)))
          (unless save-words
            (setq save-words *abbrev-words*))
          (let ((dst-word (pop save-words)))
            (backward-delete-char (length src-word) t)
            (insert-string dst-word)))))))

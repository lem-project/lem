(in-package :lem)

(defvar *grep-vector* nil)
(defvar *grep-index* 0)

(defun grep-parse-line (line)
  (ignore-errors
   (let* ((i (position #\: line))
          (j (position #\: line :start (1+ i)))
          (filename (subseq line 0 i))
          (linum (parse-integer (subseq line (1+ i) j))))
     (when (and (stringp filename) (integerp linum))
       (list filename linum)))))

(defun update-grep-list (lines)
  (let ((list))
    (dolist (line lines)
      (unless (string= "" line)
        (let ((result (grep-parse-line line)))
          (when result
            (push result list)))))
    (setq *grep-vector* (apply 'vector (nreverse list)))))

(define-command grep (str) ("sgrep -nH ")
  (let ((str
         (with-output-to-string (s)
           (shell-command (concatenate 'string "grep -nH " str)
                          :output s))))
    (update-grep-list (split-string str #\newline))
    (let ((buffer (get-buffer-create "*Grep*")))
      (let ((*current-window* (pop-to-buffer buffer)))
        (buffer-erase buffer)
        (insert-string str)
        (beginning-of-buffer)))))

(define-key *global-keymap* "M-n" 'grep-next)
(define-command grep-next (&optional (n 1)) ("p")
  (when (and *grep-vector*
             (< *grep-index* (length *grep-vector*)))
    (destructuring-bind (filename linum)
        (aref *grep-vector* *grep-index*)
      (find-file filename)
      (goto-line linum))
    (when (< (1+ *grep-index*) (length *grep-vector*))
      (incf *grep-index*))
    t))

(define-key *global-keymap* "M-p" 'grep-prev)
(define-command grep-prev (&optional (n 1)) ("p")
  (when (and *grep-vector*
             (< 0 *grep-index*))
    (destructuring-bind (filename linum)
        (aref *grep-vector* *grep-index*)
      (find-file filename)
      (goto-line linum))
    (when (< 0 (1- *grep-index*))
      (decf *grep-index*))
    t))

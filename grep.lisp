(in-package :lem)

(export '(grep
          grep-next
          grep-prev))

(defvar *grep-vector* nil)
(defvar *grep-index* -1)

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
    (setq *grep-vector* (apply 'vector (nreverse list)))
    (setq *grep-index* -1)))

(define-command grep (str) ("sgrep -nH ")
  (let ((str
         (with-output-to-string (s)
           (shell-command (concatenate 'string "grep -nH " str)
                          :output s))))
    (update-grep-list (split-string str #\newline))
    (popup (get-buffer-create "*Grep*")
           #'(lambda (out)
               (princ str out)))))

(define-key *global-keymap* (kbd "M-n") 'grep-next)
(define-command grep-next (&optional (n 1)) ("p")
  (when (and *grep-vector*
             (< (1+ *grep-index*) (length *grep-vector*)))
    (incf *grep-index*)
    (destructuring-bind (filename linum)
        (aref *grep-vector* *grep-index*)
      (find-file filename)
      (goto-line linum))
    t))

(define-key *global-keymap* (kbd "M-p") 'grep-prev)
(define-command grep-prev (&optional (n 1)) ("p")
  (when (and *grep-vector*
             (<= 0 (1- *grep-index*)))
    (decf *grep-index*)
    (destructuring-bind (filename linum)
        (aref *grep-vector* *grep-index*)
      (find-file filename)
      (goto-line linum))
    t))

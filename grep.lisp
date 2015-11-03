;; -*- Mode: Lisp; Package: Lem -*-

(in-package :lem)

(export '(grep
          grep-update
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
       (list filename
             #'(lambda () (goto-line linum t)))))))

(defun grep-parse-lines (lines)
  (remove nil
          (mapcar #'grep-parse-line
                  (remove "" lines :test #'string=))))

(defun update-grep-list (list)
  (setq *grep-vector* (apply 'vector list))
  (setq *grep-index* -1))

(defun grep-update (str)
  (update-grep-list (grep-parse-lines (split-string str #\newline)))
  (info-popup (get-buffer-create "*Grep*")
              #'(lambda (out)
                  (princ str out))
              nil))

(define-command grep (str) ("sgrep -nH ")
  (grep-update
   (with-output-to-string (s)
     (shell-command (concatenate 'string "grep -nH " str)
                    :output s))))

(define-key *global-keymap* (kbd "M-n") 'grep-next)
(define-command grep-next (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (unless (and *grep-vector*
                 (< (1+ *grep-index*) (length *grep-vector*)))
      (return nil))
    (incf *grep-index*)
    (destructuring-bind (filename goto-fn)
        (aref *grep-vector* *grep-index*)
      (find-file filename)
      (funcall goto-fn))))

(define-key *global-keymap* (kbd "M-p") 'grep-prev)
(define-command grep-prev (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (unless (and *grep-vector*
                 (<= 0 (1- *grep-index*)))
      (return))
    (decf *grep-index*)
    (destructuring-bind (filename goto-fn)
        (aref *grep-vector* *grep-index*)
      (find-file filename)
      (funcall goto-fn))))

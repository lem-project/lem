(defpackage :lem-git-gutter/diff-parser
  (:use :cl)
  (:export :parse-hunk-header
           :parse-git-diff))
(in-package :lem-git-gutter/diff-parser)

(defun parse-hunk-header (line)
  "Parse a hunk header line and return (new-start new-count).
   Example: '@@ -10,3 +15,5 @@' -> (values 15 5)"
  (ppcre:register-groups-bind ((#'parse-integer new-start)
                               (#'parse-integer new-count))
      ("@@ -\\d+(?:,\\d+)? \\+(\\d+)(?:,(\\d+))? @@" line)
    (values new-start (or new-count 1))))

(defun parse-git-diff (diff-output)
  "Parse git diff output and return a hash-table mapping line-number to change-type.
   Change types are :added, :modified, or :deleted."
  (let ((changes (make-hash-table))
        (current-line nil)
        (pending-deletes 0))
    (dolist (line (uiop:split-string diff-output :separator '(#\Newline)))
      (cond
        ;; Hunk header
        ((and (>= (length line) 2)
              (string= (subseq line 0 2) "@@"))
         (multiple-value-bind (start count)
             (parse-hunk-header line)
           (declare (ignore count))
           ;; Process any pending deletes before moving to new hunk
           (when (and current-line (> pending-deletes 0))
             (setf (gethash current-line changes) :deleted))
           (setf current-line start
                 pending-deletes 0)))
        ;; Added line
        ((and (plusp (length line))
              (char= (char line 0) #\+)
              (or (< (length line) 2)
                  (not (char= (char line 1) #\+))))
         (when current-line
           (if (> pending-deletes 0)
               ;; Delete followed by add = modification
               (progn
                 (setf (gethash current-line changes) :modified)
                 (decf pending-deletes))
               ;; Pure addition
               (setf (gethash current-line changes) :added))
           (incf current-line)))
        ;; Deleted line
        ((and (plusp (length line))
              (char= (char line 0) #\-)
              (or (< (length line) 2)
                  (not (char= (char line 1) #\-))))
         (incf pending-deletes))
        ;; Context line (shouldn't appear with -U0, but handle anyway)
        ((and (plusp (length line))
              (char= (char line 0) #\Space))
         (when (and current-line (> pending-deletes 0))
           (setf (gethash current-line changes) :deleted)
           (setf pending-deletes 0))
         (when current-line
           (incf current-line)))))
    ;; Handle any remaining pending deletes at end of file
    (when (and current-line (> pending-deletes 0))
      (setf (gethash current-line changes) :deleted))
    changes))

(defpackage :lem-lsp-mode/specification
  (:use :cl)
  (:import-from :cl-ppcre))
(in-package :lem-lsp-mode/specification)

(defun lines-to-string (lines)
  (with-output-to-string (out)
    (dolist (line lines)
      (write-line line out))))

(defun extract-typescript (spec-file)
  (with-open-file (in spec-file)
    (let ((code-list '()))
      (loop :with in-code-p := nil
            :and lines := '()
            :for line := (read-line in nil nil)
            :while line
            :do (let ((line (string-right-trim '(#\Return) line)))
                  (if in-code-p
                      (cond ((ppcre:scan "^```" line)
                             (setf in-code-p nil)
                             (push (lines-to-string (nreverse lines)) code-list))
                            (t
                             (push line lines)))
                      (when (ppcre:scan "^```typescript" line)
                        (setf in-code-p t)
                        (setf lines '())))))
      (nreverse code-list))))

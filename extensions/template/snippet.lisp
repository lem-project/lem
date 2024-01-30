(defpackage #:lem-template/snippet
  (:use :cl :lem)
  (:import-from #:lem-template/render
                #:render-string)
  (:import-from #:lem-template/prompt
                #:prompt-hash-table)
  (:import-from #:alexandria-2
                #:if-let)
  (:export #:*format-after-snippet*
           #:register-snippet
           #:register-snippets
           #:insert-snippet))
(in-package :lem-template/snippet)

(defvar *format-after-snippet* t
  "When enabled, formats buffer after inserting snippet.")

(defvar *mode-snippets* (make-hash-table)
  "Table mapping mode to another table of named snippets.")

(defun register-snippet (&key mode name file string)
  "Register a snippet used in mode."
  (if-let ((snips (gethash mode *mode-snippets*)))
    (if file
        (setf (gethash name snips) (uiop:read-file-string file))
        (setf (gethash name snips) string))
    (progn (setf (gethash mode *mode-snippets*) (make-hash-table :test #'equal))
           (register-snippet :mode mode :name name :file file :string string))))

(defmacro register-snippets (&body snippets)
  "Register multiple templates with `register-template`."
  `(progn ,@(mapcar (lambda (it) `(register-snippet ,@it)) snippets)))

(define-command insert-snippet () ()
  "Select a snippet to insert at point."
  (let* ((buffer (current-buffer))
         (point (current-point))
         (mode (buffer-major-mode buffer)))
    (if-let ((snips (gethash mode *mode-snippets*)))
      (progn
        ;; insert the snippet
        (insert-string point (render-string
                              (prompt-hash-table "Snippet: " snips)
                              `(:buffer ,buffer
                                :path ,(buffer-filename buffer))))
        ;; format the new snippet
        (when *format-after-snippet*
          (write-to-file-without-write-hook buffer (buffer-filename buffer))
          (lem:format-buffer :buffer buffer :auto t)))
      (message "No snippets for mode ~a" mode))))

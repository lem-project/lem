(uiop:define-package :lem-markdown-mode/preview/preview
  (:use :cl :lem))
(in-package :lem-markdown-mode/preview/preview)

(define-command markdown-preview () ()
  "Preview the current Markdown buffer as rendered HTML in an internal buffer."
  (lem-markdown-mode/internal:preview (current-buffer) :html-buffer))

(define-command markdown-preview-external-browser () ()
  "Preview the current Markdown buffer as rendered HTML in an external web browser."
  (lem-markdown-mode/internal:preview (current-buffer) :external-browser))

(defun render (string)
  (let ((3bmd-code-blocks:*code-blocks* t))
    (with-output-to-string (stream)
      (3bmd:parse-string-and-print-to-stream string stream))))

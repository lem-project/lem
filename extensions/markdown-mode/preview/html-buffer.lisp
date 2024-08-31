(uiop:define-package :lem-markdown-mode/preview/html-buffer
  (:use :cl :lem)
  (:import-from :lem-markdown-mode/preview/preview
                :render))
(in-package :lem-markdown-mode/preview/html-buffer)

(defun preview-buffer-name (buffer)
  (format nil
          "*Markdown Preview ~A*"
          (buffer-name buffer)))

(defmethod lem-markdown-mode/internal:preview (buffer (view-type (eql :html-buffer)))
  (let* ((html (render (buffer-text buffer)))
         (html-buffer (lem:make-html-buffer (preview-buffer-name buffer)
                                            html)))
    (pop-to-buffer html-buffer)))

(defmethod lem-markdown-mode/internal:on-save (buffer (view-type (eql :html-buffer)))
  (lem-markdown-mode/internal:preview buffer :html-buffer))

(defmethod lem-markdown-mode/internal:on-kill (buffer (view-type (eql :html-buffer)))
  )

(defmethod lem-markdown-mode/internal:on-change (buffer (view-type (eql :html-buffer)))
  (when (get-buffer (preview-buffer-name buffer))
    (lem-markdown-mode/internal:preview buffer :html-buffer)))

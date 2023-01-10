(in-package :lem-language-server)

(defun buffer-uri (buffer)
  (let ((text-document (buffer-text-document buffer)))
    (cond (text-document
           (text-document-uri text-document))
          (t
           (assert (lem:buffer-temporary-p buffer))
           ;; Temporary buffer are not associated with text-document, so return URI based on filename
           (pathname-to-uri (lem:buffer-filename buffer))))))

(defun point-to-lsp-location (point)
  (let ((uri (buffer-uri (lem:point-buffer point))))
    (lem:with-point ((end point))
      (lem:form-offset end 1)
      (make-instance 'lsp:location
                     :uri uri
                     :range (points-to-lsp-range point end)))))

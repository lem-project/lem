(in-package :lem-language-server)

(defun point-lsp-line-number (point)
  (1- (lem:line-number-at-point point)))

(defun point-to-lsp-position (point)
  (make-instance 'lsp:position
                 :line (1- (lem:line-number-at-point point))
                 :character (lem:point-charpos point)))

(defun points-to-lsp-range (start end)
  (make-instance 'lsp:range
                 :start (point-to-lsp-position start)
                 :end (point-to-lsp-position end)))

(defun move-to-lsp-position (point position)
  (check-type point lem:point)
  (check-type position lsp:position)
  (let ((line (lsp:position-line position))
        (character (lsp:position-character position)))
    (lem:move-to-line point (1+ line))
    (lem:character-offset (lem:line-start point) character)
    point))

(defun text-document-position-params-to-point (params)
  (check-type params lsp:text-document-position-params)
  (let ((text-document-identifier (lsp:text-document-position-params-text-document params))
        (position (lsp:text-document-position-params-position params)))
    (let* ((text-document (find-text-document text-document-identifier))
           (buffer (text-document-buffer text-document)))
      (lem:with-point ((point (lem:buffer-point buffer)))
        (move-to-lsp-position point position)
        point))))

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

(in-package :lem-language-server)

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

(defun point-to-lsp-position (point)
  (make-instance 'lsp:position
                 :line (1- (lem:line-number-at-point point))
                 :character (lem:point-charpos point)))

(defun points-to-lsp-range (start end)
  (make-instance 'lsp:range
                 :start (point-to-lsp-position start)
                 :end (point-to-lsp-position end)))

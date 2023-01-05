(in-package :lem-language-server)

(defun buffer-text-document (buffer)
  (lem:buffer-value buffer 'text-document))

(defun (setf buffer-text-document) (text-document buffer)
  (setf (lem:buffer-value buffer 'text-document) text-document))

(defvar *text-document-table* (make-hash-table :test 'equal))

(defclass text-document ()
  ((uri :initarg :uri :accessor text-document-uri)
   (language-id :initarg :language-id :accessor text-document-language-id)
   (version :initarg :version :accessor text-document-version)
   (buffer :initarg :buffer :accessor text-document-buffer)))

(defmethod print-object ((object text-document) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "uri:~S version:~A" (text-document-uri object) (text-document-version object))))

(defun register-text-document (&rest initargs &key uri language-id version buffer)
  (declare (ignore uri language-id version))
  (let ((text-document (apply #'make-instance 'text-document initargs)))
    (setf (buffer-text-document buffer) text-document)
    (setf (gethash (text-document-uri text-document) *text-document-table*)
          text-document)))

(defun find-text-document (text-document-identifier)
  (check-type text-document-identifier
              lsp:text-document-identifier)
  (gethash (lsp:text-document-identifier-uri text-document-identifier)
           *text-document-table*))

(defun close-text-document (text-document)
  (check-type text-document text-document)
  (lem:delete-buffer (text-document-buffer text-document))
  (remhash (text-document-uri text-document)
           *text-document-table*))

(defun find-text-document-buffer (uri)
  (let ((text-document (gethash uri *text-document-table*)))
    (text-document-buffer text-document)))

(defun edit-text-document (text-document content-change)
  (check-type text-document text-document)
  (check-type content-change lsp:text-document-content-change-event)
  (let* ((buffer (text-document-buffer text-document))
         (point (lem:buffer-point buffer))
         (text (gethash "text" content-change))
         (range (gethash "range" content-change)))
    (cond ((null range)
           (lem:erase-buffer buffer)
           (lem:insert-string point text))
          (t
           (let ((start-position (lsp:range-start range))
                 (end-position (lsp:range-end range)))
             (lem:with-point ((start point)
                              (end point))
               (move-to-lsp-position start start-position)
               (move-to-lsp-position end end-position)
               (lem:delete-between-points start end)
               (lem:insert-string start text)))))))

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

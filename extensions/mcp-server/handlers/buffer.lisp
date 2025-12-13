(in-package :lem-mcp-server)

;;; Buffer Operation Tools

;; buffer_list - Get list of all buffers
(define-mcp-tool "buffer_list" ()
  (:description "Get list of all buffers in the editor"
   :input-schema (("type" . "object")
                  ("properties" . ())
                  ("required" . ())))
  (let ((buffers (buffer-list)))
    (with-output-to-string (s)
      (yason:encode
       (mapcar (lambda (buf)
                 (alist-to-hash-table
                  `(("name" . ,(buffer-name buf))
                    ("filename" . ,(or (buffer-filename buf) :null))
                    ("modified" . ,(if (buffer-modified-p buf) t :false))
                    ("readonly" . ,(if (buffer-read-only-p buf) t :false))
                    ("lines" . ,(buffer-nlines buf)))))
               buffers)
       s))))

;; buffer_get_content - Get buffer content
(define-mcp-tool "buffer_get_content" (buffer-name start-line end-line)
  (:description "Get content of a buffer"
   :input-schema (("type" . "object")
                  ("properties" . (("buffer_name" . (("type" . "string")
                                                    ("description" . "Name of the buffer")))
                                   ("start_line" . (("type" . "integer")
                                                    ("description" . "Starting line (1-indexed, optional)")))
                                   ("end_line" . (("type" . "integer")
                                                  ("description" . "Ending line (1-indexed, inclusive, optional)")))))
                  ("required" . ("buffer_name"))))
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (mcp-error +invalid-params+
                 (format nil "Buffer not found: ~A" buffer-name)))
    (if (or start-line end-line)
        ;; Get specific line range
        (let ((start (or start-line 1))
              (end (or end-line (buffer-nlines buffer))))
          (with-point ((start-point (buffer-point buffer))
                       (end-point (buffer-point buffer)))
            (move-to-line start-point start)
            (line-start start-point)
            (move-to-line end-point end)
            (line-end end-point)
            (points-to-string start-point end-point)))
        ;; Get entire buffer
        (buffer-text buffer))))

;; buffer_create - Create a new buffer
(define-mcp-tool "buffer_create" (name content)
  (:description "Create a new buffer"
   :input-schema (("type" . "object")
                  ("properties" . (("name" . (("type" . "string")
                                              ("description" . "Name for the new buffer")))
                                   ("content" . (("type" . "string")
                                                 ("description" . "Initial content (optional)")))))
                  ("required" . ("name"))))
  (when (get-buffer name)
    (mcp-error +invalid-params+
               (format nil "Buffer already exists: ~A" name)))
  (let ((buffer (make-buffer name)))
    (when content
      (with-point ((point (buffer-point buffer)))
        (insert-string point content)))
    (switch-to-buffer buffer)
    (redraw-display :force t)
    (format nil "Buffer created: ~A" name)))

;; buffer_delete - Delete a buffer
(define-mcp-tool "buffer_delete" (buffer-name)
  (:description "Delete a buffer"
   :input-schema (("type" . "object")
                  ("properties" . (("buffer_name" . (("type" . "string")
                                                    ("description" . "Name of the buffer to delete")))))
                  ("required" . ("buffer_name"))))
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (mcp-error +invalid-params+
                 (format nil "Buffer not found: ~A" buffer-name)))
    (delete-buffer buffer)
    (redraw-display :force t)
    (format nil "Buffer deleted: ~A" buffer-name)))

;; buffer_info - Get detailed buffer information
(define-mcp-tool "buffer_info" (buffer-name)
  (:description "Get detailed information about a buffer"
   :input-schema (("type" . "object")
                  ("properties" . (("buffer_name" . (("type" . "string")
                                                    ("description" . "Name of the buffer")))))
                  ("required" . ("buffer_name"))))
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (mcp-error +invalid-params+
                 (format nil "Buffer not found: ~A" buffer-name)))
    (with-output-to-string (s)
      (yason:encode
       (alist-to-hash-table
        `(("name" . ,(buffer-name buffer))
          ("filename" . ,(or (buffer-filename buffer) :null))
          ("modified" . ,(if (buffer-modified-p buffer) t :false))
          ("readonly" . ,(if (buffer-read-only-p buffer) t :false))
          ("lines" . ,(buffer-nlines buffer))
          ("point" . (("line" . ,(line-number-at-point (buffer-point buffer)))
                      ("column" . ,(point-charpos (buffer-point buffer)))))))
       s))))

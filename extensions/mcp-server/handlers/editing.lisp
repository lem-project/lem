(in-package :lem-mcp-server)

;;; Editing Operation Tools

;; buffer_insert - Insert text at position
(define-mcp-tool "buffer_insert" (buffer-name line column text)
  (:description "Insert text at a specific position in a buffer"
   :input-schema (("type" . "object")
                  ("properties" . (("buffer_name" . (("type" . "string")
                                                    ("description" . "Name of the buffer")))
                                   ("line" . (("type" . "integer")
                                              ("description" . "Line number (1-indexed)")))
                                   ("column" . (("type" . "integer")
                                                ("description" . "Column number (0-indexed)")))
                                   ("text" . (("type" . "string")
                                              ("description" . "Text to insert")))))
                  ("required" . ("buffer_name" "line" "column" "text"))))
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (mcp-error +invalid-params+
                 (format nil "Buffer not found: ~A" buffer-name)))
    (when (buffer-read-only-p buffer)
      (mcp-error +server-error+
                 (format nil "Buffer is read-only: ~A" buffer-name)))
    (with-point ((point (buffer-point buffer)))
      (move-to-line point line)
      (line-start point)
      (character-offset point column)
      (insert-string point text))
    (switch-to-buffer buffer)
    (redraw-display :force t)
    (format nil "Inserted ~A characters at line ~A, column ~A"
            (length text) line column)))

;; buffer_delete_region - Delete text in a region
(define-mcp-tool "buffer_delete_region" (buffer-name start-line start-col end-line end-col)
  (:description "Delete text in a region"
   :input-schema (("type" . "object")
                  ("properties" . (("buffer_name" . (("type" . "string")
                                                    ("description" . "Name of the buffer")))
                                   ("start_line" . (("type" . "integer")
                                                    ("description" . "Starting line (1-indexed)")))
                                   ("start_col" . (("type" . "integer")
                                                   ("description" . "Starting column (0-indexed)")))
                                   ("end_line" . (("type" . "integer")
                                                  ("description" . "Ending line (1-indexed)")))
                                   ("end_col" . (("type" . "integer")
                                                 ("description" . "Ending column (0-indexed)")))))
                  ("required" . ("buffer_name" "start_line" "start_col" "end_line" "end_col"))))
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (mcp-error +invalid-params+
                 (format nil "Buffer not found: ~A" buffer-name)))
    (when (buffer-read-only-p buffer)
      (mcp-error +server-error+
                 (format nil "Buffer is read-only: ~A" buffer-name)))
    (with-point ((start-point (buffer-point buffer))
                 (end-point (buffer-point buffer)))
      (move-to-line start-point start-line)
      (line-start start-point)
      (character-offset start-point start-col)
      (move-to-line end-point end-line)
      (line-start end-point)
      (character-offset end-point end-col)
      (let ((deleted-text (points-to-string start-point end-point)))
        (delete-between-points start-point end-point)
        (switch-to-buffer buffer)
        (redraw-display :force t)
        (format nil "Deleted ~A characters" (length deleted-text))))))

;; buffer_replace - Replace text in a region
(define-mcp-tool "buffer_replace" (buffer-name start-line start-col end-line end-col new-text)
  (:description "Replace text in a region with new text"
   :input-schema (("type" . "object")
                  ("properties" . (("buffer_name" . (("type" . "string")
                                                    ("description" . "Name of the buffer")))
                                   ("start_line" . (("type" . "integer")
                                                    ("description" . "Starting line (1-indexed)")))
                                   ("start_col" . (("type" . "integer")
                                                   ("description" . "Starting column (0-indexed)")))
                                   ("end_line" . (("type" . "integer")
                                                  ("description" . "Ending line (1-indexed)")))
                                   ("end_col" . (("type" . "integer")
                                                 ("description" . "Ending column (0-indexed)")))
                                   ("new_text" . (("type" . "string")
                                                  ("description" . "New text to insert")))))
                  ("required" . ("buffer_name" "start_line" "start_col" "end_line" "end_col" "new_text"))))
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (mcp-error +invalid-params+
                 (format nil "Buffer not found: ~A" buffer-name)))
    (when (buffer-read-only-p buffer)
      (mcp-error +server-error+
                 (format nil "Buffer is read-only: ~A" buffer-name)))
    (with-point ((start-point (buffer-point buffer))
                 (end-point (buffer-point buffer)))
      (move-to-line start-point start-line)
      (line-start start-point)
      (character-offset start-point start-col)
      (move-to-line end-point end-line)
      (line-start end-point)
      (character-offset end-point end-col)
      (let ((deleted-text (points-to-string start-point end-point)))
        (delete-between-points start-point end-point)
        (insert-string start-point new-text)
        (switch-to-buffer buffer)
        (redraw-display :force t)
        (format nil "Replaced ~A characters with ~A characters"
                (length deleted-text) (length new-text))))))

;; buffer_save - Save a buffer to file
(define-mcp-tool "buffer_save" (buffer-name)
  (:description "Save a buffer to its associated file"
   :input-schema (("type" . "object")
                  ("properties" . (("buffer_name" . (("type" . "string")
                                                    ("description" . "Name of the buffer to save")))))
                  ("required" . ("buffer_name"))))
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (mcp-error +invalid-params+
                 (format nil "Buffer not found: ~A" buffer-name)))
    (unless (buffer-filename buffer)
      (mcp-error +invalid-params+
                 (format nil "Buffer has no associated file: ~A" buffer-name)))
    (save-buffer buffer)
    (format nil "Buffer saved: ~A" (buffer-filename buffer))))

;; buffer_set_content - Replace entire buffer content
(define-mcp-tool "buffer_set_content" (buffer-name content)
  (:description "Replace entire buffer content"
   :input-schema (("type" . "object")
                  ("properties" . (("buffer_name" . (("type" . "string")
                                                    ("description" . "Name of the buffer")))
                                   ("content" . (("type" . "string")
                                                 ("description" . "New content for the buffer")))))
                  ("required" . ("buffer_name" "content"))))
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (mcp-error +invalid-params+
                 (format nil "Buffer not found: ~A" buffer-name)))
    (when (buffer-read-only-p buffer)
      (mcp-error +server-error+
                 (format nil "Buffer is read-only: ~A" buffer-name)))
    (with-point ((start (buffer-point buffer))
                 (end (buffer-point buffer)))
      (lem:buffer-start start)
      (lem:buffer-end end)
      (delete-between-points start end)
      (insert-string start content))
    (switch-to-buffer buffer)
    (redraw-display :force t)
    (format nil "Buffer content replaced (~A characters)" (length content))))

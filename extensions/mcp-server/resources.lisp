(in-package :lem-mcp-server)

;;; MCP Resources Interface
;;; Handles resources/list and resources/read methods

;; Helper to determine MIME type from buffer
(defun buffer-mime-type (buffer)
  "Determine MIME type for a buffer based on filename or mode."
  (let ((filename (buffer-filename buffer)))
    (if filename
        (let ((extension (pathname-type filename)))
          (cond
            ((member extension '("lisp" "cl" "asd" "asdf") :test #'string-equal)
             "text/x-common-lisp")
            ((string-equal extension "el")
             "text/x-emacs-lisp")
            ((member extension '("c" "h") :test #'string-equal)
             "text/x-c")
            ((member extension '("cpp" "cc" "cxx" "hpp" "hh") :test #'string-equal)
             "text/x-c++")
            ((string-equal extension "py")
             "text/x-python")
            ((member extension '("js" "mjs") :test #'string-equal)
             "text/javascript")
            ((member extension '("ts" "tsx") :test #'string-equal)
             "text/typescript")
            ((string-equal extension "rb")
             "text/x-ruby")
            ((string-equal extension "rs")
             "text/x-rust")
            ((string-equal extension "go")
             "text/x-go")
            ((string-equal extension "java")
             "text/x-java")
            ((member extension '("md" "markdown") :test #'string-equal)
             "text/markdown")
            ((member extension '("json" "jsonl") :test #'string-equal)
             "application/json")
            ((member extension '("yaml" "yml") :test #'string-equal)
             "text/yaml")
            ((member extension '("xml" "xsl" "xslt") :test #'string-equal)
             "text/xml")
            ((member extension '("html" "htm") :test #'string-equal)
             "text/html")
            ((string-equal extension "css")
             "text/css")
            ((string-equal extension "sh")
             "text/x-shellscript")
            ((string-equal extension "sql")
             "text/x-sql")
            (t "text/plain")))
        "text/plain")))

;; Create resource from buffer
(defun buffer-to-resource (buffer)
  "Create an MCP resource from a buffer."
  (make-instance 'mcp-resource
                 :uri (format nil "buffer://~A" (buffer-name buffer))
                 :name (buffer-name buffer)
                 :description (when (buffer-filename buffer)
                                (format nil "File: ~A" (buffer-filename buffer)))
                 :mime-type (buffer-mime-type buffer)))

;; Parse resource URI
(defun parse-resource-uri (uri)
  "Parse a resource URI and return (scheme . path)."
  (let ((colon-pos (position #\: uri)))
    (when colon-pos
      (let ((scheme (subseq uri 0 colon-pos))
            (path (subseq uri (+ colon-pos 3))))  ; Skip "://"
        (cons scheme path)))))

;; resources/list - List available resources
(define-mcp-request (resources-list-request "resources/list") (params)
  (let ((resources (mapcar #'buffer-to-resource (buffer-list))))
    `(("resources" . ,(mapcar #'resource-to-json resources)))))

;; resources/read - Read a resource
(define-mcp-request (resources-read-request "resources/read") (params)
  (let* ((uri (cdr (assoc "uri" params :test #'string=)))
         (parsed (parse-resource-uri uri)))
    (unless parsed
      (mcp-error +invalid-params+
                 (format nil "Invalid resource URI: ~A" uri)))
    (let ((scheme (car parsed))
          (path (cdr parsed)))
      (cond
        ((string-equal scheme "buffer")
         (let ((buffer (get-buffer path)))
           (unless buffer
             (mcp-error +invalid-params+
                        (format nil "Buffer not found: ~A" path)))
           `(("contents" . (,(list
                              `(("uri" . ,uri)
                                ("mimeType" . ,(buffer-mime-type buffer))
                                ("text" . ,(buffer-text buffer)))))))))
        ((string-equal scheme "file")
         (let ((filepath path))
           (unless (probe-file filepath)
             (mcp-error +invalid-params+
                        (format nil "File not found: ~A" filepath)))
           (let ((content (uiop:read-file-string filepath)))
             `(("contents" . (,(list
                                `(("uri" . ,uri)
                                  ("mimeType" . "text/plain")
                                  ("text" . ,content)))))))))
        (t
         (mcp-error +invalid-params+
                    (format nil "Unsupported URI scheme: ~A" scheme)))))))

;; resources/templates/list - List resource templates (optional)
(define-mcp-request (resources-templates-list-request "resources/templates/list") (params)
  ;; We don't support resource templates for now
  `(("resourceTemplates" . ())))

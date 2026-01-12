(in-package :lem-mcp-server)

;;; Key Input Tools
;;;
;;; Note on internal symbol usage:
;;; - lem-core::parse-keyspec: Parses Emacs-style key notation strings into
;;;   key objects. This is essential for accepting user key input but is not
;;;   exported from lem-core. Consider exporting for public use.

;;; Helper functions

(defun safe-parse-keyspec (keyspec)
  "Parse a key specification string with error handling.
Returns (values keys error-message)."
  (handler-case
      (values (lem-core::parse-keyspec keyspec) nil)
    (editor-error (e)
      (values nil (format nil "~A" e)))
    (error (e)
      (values nil (format nil "Parse error: ~A" e)))))

(defun key-to-string (key)
  "Convert a key object to a human-readable string representation."
  (with-output-to-string (s)
    (when (key-ctrl key) (write-string "C-" s))
    (when (key-meta key) (write-string "M-" s))
    (when (key-super key) (write-string "s-" s))
    (when (key-hyper key) (write-string "H-" s))
    (when (key-shift key) (write-string "Shift-" s))
    (write-string (key-sym key) s)))

(defun keys-to-string-list (keys)
  "Convert a list of key objects to a list of string representations."
  (mapcar #'key-to-string keys))

;;; Main tool definition

(define-mcp-tool "editor_send_keys" (keys wait-for-completion)
  (:description "Send a key sequence to the editor. Uses Emacs-style key notation (e.g., 'C-x C-s' for Ctrl+x Ctrl+s, 'M-x' for Alt+x, 'Return' for Enter)."
   :input-schema (("type" . "object")
                  ("properties" . (("keys" . (("type" . "string")
                                              ("description" . "Key sequence in Emacs notation (e.g., 'C-x C-s', 'M-x forward-char Return', 'C-g')")))
                                   ("wait_for_completion" . (("type" . "boolean")
                                                             ("description" . "Wait for key sequence to be processed before returning. Default: true")))))
                  ("required" . ("keys"))))
  ;; Validate keys parameter
  (unless keys
    (mcp-error +invalid-params+ "Missing required parameter: keys"))
  (when (string= keys "")
    (mcp-error +invalid-params+ "Empty key sequence"))

  ;; Parse the key specification
  (multiple-value-bind (parsed-keys parse-error) (safe-parse-keyspec keys)
    (when parse-error
      (mcp-error +invalid-params+ (format nil "Invalid key sequence '~A': ~A" keys parse-error)))

    (let ((wait (if (null wait-for-completion) t wait-for-completion))
          (key-strings (keys-to-string-list parsed-keys)))
      (if wait
          ;; Execute synchronously
          (handler-case
              (progn
                (execute-key-sequence parsed-keys)
                (redraw-display)
                (with-output-to-string (s)
                  (yason:encode
                   (alist-to-hash-table
                    `(("success" . t)
                      ("keys_sent" . ,key-strings)
                      ("message" . "Key sequence executed")))
                   s)))
            (editor-abort ()
              (with-output-to-string (s)
                (yason:encode
                 (alist-to-hash-table
                  `(("success" . :false)
                    ("keys_sent" . ,key-strings)
                    ("message" . "Operation aborted (C-g)")))
                 s)))
            (editor-error (e)
              (mcp-error +server-error+ (format nil "Editor error: ~A" e)))
            (error (e)
              (mcp-error +server-error+ (format nil "Execution error: ~A" e))))
          ;; Execute asynchronously
          (progn
            (send-event
             (lambda ()
               (ignore-errors
                 (execute-key-sequence parsed-keys)
                 (redraw-display))))
            (with-output-to-string (s)
              (yason:encode
               (alist-to-hash-table
                `(("success" . t)
                  ("keys_sent" . ,key-strings)
                  ("message" . "Key sequence queued for execution")))
               s)))))))

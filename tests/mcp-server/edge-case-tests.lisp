(defpackage :lem-tests/mcp-server/edge-cases
  (:use :cl
        :rove
        :lem-tests/mcp-server/utils))
(in-package :lem-tests/mcp-server/edge-cases)

;;; Edge case and error handling tests for MCP Server
;;; Classical/Detroit school approach: verify error behavior with real components

;;; ========================================
;;; Error Handling - Nonexistent Buffers
;;; ========================================

(deftest error-on-get-nonexistent-buffer
  "Getting content of nonexistent buffer returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let ((result (mcp-tool-call "buffer_get_content"
                    '(("buffer_name" . "nonexistent-buffer")))))
      (ok (result-is-error-p result)
          "Returns error for nonexistent buffer"))))

(deftest error-on-delete-nonexistent-buffer
  "Deleting nonexistent buffer returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let ((result (mcp-tool-call "buffer_delete"
                    '(("buffer_name" . "nonexistent-buffer")))))
      (ok (result-is-error-p result)
          "Returns error for deleting nonexistent buffer"))))

(deftest error-on-info-nonexistent-buffer
  "Getting info of nonexistent buffer returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let ((result (mcp-tool-call "buffer_info"
                    '(("buffer_name" . "nonexistent-buffer")))))
      (ok (result-is-error-p result)
          "Returns error for info on nonexistent buffer"))))

(deftest error-on-insert-nonexistent-buffer
  "Inserting into nonexistent buffer returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let ((result (mcp-tool-call "buffer_insert"
                    '(("buffer_name" . "nonexistent-buffer")
                      ("line" . 1)
                      ("column" . 0)
                      ("text" . "test")))))
      (ok (result-is-error-p result)
          "Returns error for insert into nonexistent buffer"))))

;;; ========================================
;;; Error Handling - Duplicate Creation
;;; ========================================

(deftest error-on-duplicate-buffer-create
  "Creating duplicate buffer returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; First creation should succeed
    (let ((result1 (mcp-tool-call "buffer_create"
                     '(("name" . "duplicate-test")))))
      (ok (not (result-is-error-p result1))
          "First creation succeeds"))

    ;; Second creation should fail
    (let ((result2 (mcp-tool-call "buffer_create"
                     '(("name" . "duplicate-test")))))
      (ok (result-is-error-p result2)
          "Second creation returns error"))))

;;; ========================================
;;; Error Handling - Invalid Resource URI
;;; ========================================

(deftest error-on-invalid-resource-uri
  "Reading invalid resource URI returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (ok (signals (mcp-request "resources/read"
                   '(("uri" . "invalid-uri-without-scheme")))
                 'error)
        "Invalid URI signals error")))

(deftest error-on-unsupported-resource-scheme
  "Reading unsupported URI scheme returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (ok (signals (mcp-request "resources/read"
                   '(("uri" . "ftp://some-server/file")))
                 'error)
        "Unsupported scheme signals error")))

(deftest error-on-nonexistent-buffer-resource
  "Reading nonexistent buffer resource returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (ok (signals (mcp-request "resources/read"
                   '(("uri" . "buffer://nonexistent-buffer")))
                 'error)
        "Nonexistent buffer resource signals error")))

;;; ========================================
;;; Edge Case - Empty Buffer
;;; ========================================

(deftest handles-empty-buffer
  "Empty buffer is handled correctly"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; Create empty buffer
    (let ((result (mcp-tool-call "buffer_create"
                    '(("name" . "empty-test")))))
      (ok (not (result-is-error-p result)) "Empty buffer creation succeeds"))

    ;; Get content should work
    (let* ((result (mcp-tool-call "buffer_get_content"
                     '(("buffer_name" . "empty-test"))))
           (content (extract-text-content result)))
      (ok (not (result-is-error-p result)) "Getting empty content succeeds")
      (ok (string= "" content) "Content is empty string"))))

;;; ========================================
;;; Edge Case - Large Content
;;; ========================================

(deftest handles-large-content
  "Large content is handled correctly"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let ((large-content (make-string 50000 :initial-element #\x)))
      ;; Create buffer with large content
      (let ((result (mcp-tool-call "buffer_create"
                      `(("name" . "large-test")
                        ("content" . ,large-content)))))
        (ok (not (result-is-error-p result)) "Large buffer creation succeeds"))

      ;; Verify content
      (let ((buf (lem:get-buffer "large-test")))
        (ok (= 50000 (length (lem:buffer-text buf)))
            "Large content stored correctly")))))

;;; ========================================
;;; Edge Case - Unicode Content
;;; ========================================

(deftest handles-unicode-content
  "Unicode content is handled correctly"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let ((unicode-content "日本語テスト 🎉 émojis αβγ"))
      ;; Create buffer with unicode
      (mcp-tool-call "buffer_create"
        `(("name" . "unicode-test")
          ("content" . ,unicode-content)))

      ;; Verify content
      (let ((buf (lem:get-buffer "unicode-test")))
        (ok (search "日本語" (lem:buffer-text buf))
            "Japanese characters preserved")
        (ok (search "🎉" (lem:buffer-text buf))
            "Emoji preserved")
        (ok (search "αβγ" (lem:buffer-text buf))
            "Greek characters preserved")))))

;;; ========================================
;;; Edge Case - Special Characters in Buffer Name
;;; ========================================

(deftest handles-special-buffer-names
  "Buffer names with special characters work"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; Create buffer with special name
    (let ((result (mcp-tool-call "buffer_create"
                    '(("name" . "*special-buffer*")
                      ("content" . "test content")))))
      (ok (not (result-is-error-p result))
          "Buffer with special name created"))

    ;; Access it
    (let ((buf (lem:get-buffer "*special-buffer*")))
      (ok buf "Buffer with special name accessible"))))

;;; ========================================
;;; Edge Case - Multiline Insert
;;; ========================================

(deftest handles-multiline-insert
  "Inserting multiline text works correctly"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (mcp-tool-call "buffer_create"
      '(("name" . "multiline-test")
        ("content" . "before
after")))

    ;; Insert multiline text between
    (mcp-tool-call "buffer_insert"
      '(("buffer_name" . "multiline-test")
        ("line" . 2)
        ("column" . 0)
        ("text" . "inserted1
inserted2
")))

    (let* ((buf (lem:get-buffer "multiline-test"))
           (text (lem:buffer-text buf)))
      (ok (search "inserted1" text) "First inserted line present")
      (ok (search "inserted2" text) "Second inserted line present")
      (ok (search "after" text) "Original content preserved"))))

;;; ========================================
;;; Edge Case - Zero-Width Operations
;;; ========================================

(deftest handles-empty-insert
  "Inserting empty string is handled"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (mcp-tool-call "buffer_create"
      '(("name" . "empty-insert-test")
        ("content" . "original")))

    ;; Insert empty string
    (let ((result (mcp-tool-call "buffer_insert"
                    '(("buffer_name" . "empty-insert-test")
                      ("line" . 1)
                      ("column" . 0)
                      ("text" . "")))))
      (ok (not (result-is-error-p result))
          "Empty insert succeeds"))

    (let ((buf (lem:get-buffer "empty-insert-test")))
      (ok (string= "original" (lem:buffer-text buf))
          "Content unchanged after empty insert"))))

(deftest handles-same-position-replace
  "Replacing zero-width region (insert at position)"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (mcp-tool-call "buffer_create"
      '(("name" . "same-pos-test")
        ("content" . "ab")))

    ;; Replace at same start/end (effectively insert)
    (mcp-tool-call "buffer_replace"
      '(("buffer_name" . "same-pos-test")
        ("start_line" . 1)
        ("start_col" . 1)
        ("end_line" . 1)
        ("end_col" . 1)
        ("new_text" . "X")))

    (let ((buf (lem:get-buffer "same-pos-test")))
      (ok (string= "aXb" (lem:buffer-text buf))
          "Zero-width replace acts as insert"))))

;;; ========================================
;;; Unknown Tool Handling
;;; ========================================

(deftest error-on-unknown-tool
  "Calling unknown tool returns error"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (ok (signals (mcp-tool-call "nonexistent_tool" nil)
                 'error)
        "Unknown tool signals error")))

;;; ========================================
;;; Buffer Modified State
;;; ========================================

(deftest buffer-modified-after-edit
  "Buffer shows as modified after editing"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (mcp-tool-call "buffer_create"
      '(("name" . "modified-test")
        ("content" . "initial")))

    ;; Initially not modified (just created)
    (let ((buf (lem:get-buffer "modified-test")))
      ;; After edit should be modified
      (mcp-tool-call "buffer_insert"
        '(("buffer_name" . "modified-test")
          ("line" . 1)
          ("column" . 0)
          ("text" . "X")))
      (ok (lem:buffer-modified-p buf)
          "Buffer marked as modified after edit"))))

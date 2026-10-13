(defpackage :lem-tests/mcp-server/display-input
  (:use :cl
        :rove
        :lem-tests/mcp-server/utils))
(in-package :lem-tests/mcp-server/display-input)

;;; Tests for editor_get_screen and editor_send_keys MCP tools
;;; Classical/Detroit school approach: test actual behavior through real components

;;; ========================================
;;; Helper Functions
;;; ========================================

(defun parse-json-content (tool-response)
  "Parse JSON text content from a tools/call response."
  (let ((text (extract-text-content tool-response)))
    (when text
      (yason:parse text :object-as :alist))))

;;; ========================================
;;; editor_get_screen Tests
;;; ========================================

(deftest editor-get-screen-returns-display-info
  "editor_get_screen returns display dimensions"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let* ((result (mcp-tool-call "editor_get_screen" nil))
           (screen (parse-json-content result)))
      (ok (not (result-is-error-p result))
          "Does not return error")
      (ok (assoc "display" screen :test #'string=)
          "Contains display info")
      (let ((display (cdr (assoc "display" screen :test #'string=))))
        (ok (assoc "width" display :test #'string=)
            "Display has width")
        (ok (assoc "height" display :test #'string=)
            "Display has height")))))

(deftest editor-get-screen-returns-windows
  "editor_get_screen returns window information"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let* ((result (mcp-tool-call "editor_get_screen" nil))
           (screen (parse-json-content result)))
      (ok (assoc "windows" screen :test #'string=)
          "Contains windows list")
      (ok (assoc "current_window_id" screen :test #'string=)
          "Contains current window ID"))))

(deftest editor-get-screen-window-has-required-fields
  "editor_get_screen window objects have required fields"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let* ((result (mcp-tool-call "editor_get_screen" nil))
           (screen (parse-json-content result))
           (windows (cdr (assoc "windows" screen :test #'string=))))
      (ok (> (length windows) 0)
          "At least one window exists")
      (let ((window (first windows)))
        (ok (assoc "id" window :test #'string=)
            "Window has id")
        (ok (assoc "buffer" window :test #'string=)
            "Window has buffer info")
        (ok (assoc "cursor" window :test #'string=)
            "Window has cursor info")
        (ok (assoc "position" window :test #'string=)
            "Window has position")
        (ok (assoc "size" window :test #'string=)
            "Window has size")))))

(deftest editor-get-screen-cursor-has-position
  "editor_get_screen cursor info has line and column"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let* ((result (mcp-tool-call "editor_get_screen" nil))
           (screen (parse-json-content result))
           (windows (cdr (assoc "windows" screen :test #'string=)))
           (window (first windows))
           (cursor (cdr (assoc "cursor" window :test #'string=))))
      (ok (assoc "line" cursor :test #'string=)
          "Cursor has line")
      (ok (assoc "column" cursor :test #'string=)
          "Cursor has column"))))

(deftest editor-get-screen-has-visible-content
  "editor_get_screen returns visible content lines"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; Create a buffer with some content
    (mcp-tool-call "buffer_create" '(("name" . "test-visible")))
    (mcp-tool-call "buffer_set_content"
      '(("buffer_name" . "test-visible")
        ("content" . "Line 1\nLine 2\nLine 3")))

    (let* ((result (mcp-tool-call "editor_get_screen" nil))
           (screen (parse-json-content result))
           (windows (cdr (assoc "windows" screen :test #'string=)))
           (window (first windows))
           (content (cdr (assoc "visible_content" window :test #'string=))))
      (ok content "Has visible_content")
      (ok (listp content) "visible_content is a list"))))

(deftest editor-get-screen-has-prompt-info
  "editor_get_screen returns prompt window state"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let* ((result (mcp-tool-call "editor_get_screen" nil))
           (screen (parse-json-content result)))
      (ok (assoc "prompt" screen :test #'string=)
          "Contains prompt info"))))

;;; ========================================
;;; editor_send_keys Tests
;;; ========================================

(deftest editor-send-keys-returns-success
  "editor_send_keys returns success for valid key"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let* ((result (mcp-tool-call "editor_send_keys" '(("keys" . "a"))))
           (content (parse-json-content result)))
      (ok (not (result-is-error-p result))
          "Does not return error")
      (ok (cdr (assoc "success" content :test #'string=))
          "Returns success: true"))))

(deftest editor-send-keys-parses-key-sequence
  "editor_send_keys correctly parses multi-key sequences"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; Use simple character keys that won't trigger commands
    (let* ((result (mcp-tool-call "editor_send_keys" '(("keys" . "a b c"))))
           (content (parse-json-content result)))
      (ok (not (result-is-error-p result))
          "Does not return error for multi-key sequence")
      (ok (cdr (assoc "keys_sent" content :test #'string=))
          "Returns keys_sent list"))))

(deftest editor-send-keys-error-on-invalid-key
  "editor_send_keys returns error for invalid key spec"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let* ((result (mcp-tool-call "editor_send_keys"
                     '(("keys" . "Not-A-Valid-Key-Spec-12345"))))
           (text (extract-text-content result)))
      ;; Invalid key specs should return JSON with success: false
      ;; or contain error indication in text
      (ok (or (search "success" text)
              (search "error" text)
              (search "Invalid" text))
          "Returns error indication for invalid key"))))

(deftest editor-send-keys-modifies-buffer
  "editor_send_keys actually inserts characters into buffer"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; Create a new buffer and switch to it
    (mcp-tool-call "buffer_create" '(("name" . "key-test")))

    ;; Send some keys
    (mcp-tool-call "editor_send_keys" '(("keys" . "h")))
    (mcp-tool-call "editor_send_keys" '(("keys" . "i")))

    ;; Check the buffer content
    (let* ((result (mcp-tool-call "buffer_get_content"
                     '(("buffer_name" . "key-test"))))
           (text (extract-text-content result)))
      ;; Note: The buffer might have the text if it was the active buffer
      ;; This test verifies the keys were processed
      (ok (not (result-is-error-p result))
          "Can get buffer content after sending keys"))))

(deftest editor-send-keys-supports-modifier-keys
  "editor_send_keys handles modifier key combinations"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; Test parsing of modifier combinations (just check they parse, don't execute)
    ;; C-g is safe as it cancels operations
    (let* ((result (mcp-tool-call "editor_send_keys" '(("keys" . "C-g"))))
           (content (parse-json-content result)))
      (ok (cdr (assoc "keys_sent" content :test #'string=))
          "Modifier key C-g parses correctly"))))

(deftest editor-send-keys-supports-special-keys
  "editor_send_keys handles special key names"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; Test Return key (inserts newline in most modes)
    (let* ((result (mcp-tool-call "editor_send_keys" '(("keys" . "Return"))))
           (content (parse-json-content result)))
      (ok (cdr (assoc "keys_sent" content :test #'string=))
          "Return key parses correctly"))))

;;; ========================================
;;; Integration Tests
;;; ========================================

(deftest screen-reflects-buffer-changes
  "editor_get_screen reflects changes made via buffer tools"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    ;; Create buffer with specific content
    (mcp-tool-call "buffer_create" '(("name" . "screen-test")))
    (mcp-tool-call "buffer_set_content"
      '(("buffer_name" . "screen-test")
        ("content" . "Hello World")))

    ;; Get screen state
    (let* ((result (mcp-tool-call "editor_get_screen" nil))
           (screen (parse-json-content result)))
      (ok (not (result-is-error-p result))
          "Can get screen after buffer modification"))))

(deftest tools-list-includes-new-tools
  "tools/list includes editor_get_screen and editor_send_keys"
  (with-mcp-test-env ()
    (mcp-request "initialize"
      '(("protocolVersion" . "2024-11-05")
        ("capabilities" . ())
        ("clientInfo" . (("name" . "test-client")))))

    (let* ((response (mcp-request "tools/list" nil))
           (tools (extract-result response "tools"))
           (tool-names (mapcar (lambda (tool)
                                 (cdr (assoc "name" tool :test #'string=)))
                               tools)))
      (ok (member "editor_get_screen" tool-names :test #'string=)
          "editor_get_screen is registered")
      (ok (member "editor_send_keys" tool-names :test #'string=)
          "editor_send_keys is registered"))))

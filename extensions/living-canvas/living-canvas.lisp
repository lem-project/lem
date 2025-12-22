(defpackage #:lem-living-canvas
  (:use #:cl #:lem)
  (:import-from #:call-graph
                #:call-graph-nodes
                #:graph-node-source-location
                #:*provider-registry*
                #:find-provider
                #:provider-analyze)
  (:import-from #:lem-living-canvas/micros-cl-provider
                #:analyze-package
                #:analyze-file
                #:analyze-system)
  (:import-from #:lem-living-canvas/language
                #:detect-language
                #:language-for-file)
  (:import-from #:lem-living-canvas/buffer
                #:canvas-buffer
                #:make-canvas-buffer)
  (:import-from #:lem-lisp-mode/internal
                #:check-connection
                #:connected-p)
  (:export #:living-canvas
           #:living-canvas-current-file
           #:living-canvas-system
           #:living-canvas-refresh
           #:living-canvas-current-buffer
           #:living-canvas-reload-providers
           #:living-canvas-diagnose
           #:get-provider-status))
(in-package #:lem-living-canvas)

;;; Variables

(defvar *node-positions* (make-hash-table :test 'equal)
  "Global storage for node positions across sessions")

(defvar *source-location-cache* (make-hash-table :test 'equal)
  "Cache for source locations to avoid repeated file searches")

(defvar *current-highlight-overlay* nil
  "Current overlay used to highlight the selected function in source view.")

(defvar *living-canvas-keymap*
  (lem:make-keymap :name '*living-canvas-keymap*))

;;; Attributes

(lem:define-attribute source-highlight-attribute
  (t :background "#264f78"))

;;; Key Bindings

(lem:define-key *living-canvas-keymap* "g" 'living-canvas-refresh)
(lem:define-key *living-canvas-keymap* "q" 'lem:kill-buffer)

;;; Node Position Storage

(defun save-node-position (node-id x y)
  "Save a node's position for persistence"
  (setf (gethash node-id *node-positions*) (cons x y)))

(defun get-node-position (node-id)
  "Get a saved node position"
  (gethash node-id *node-positions*))

;;; Source Location Cache

(defun cache-source-location (node-id location)
  "Cache a source location for a node"
  (setf (gethash node-id *source-location-cache*) location))

(defun get-cached-source-location (node-id)
  "Get a cached source location"
  (gethash node-id *source-location-cache*))

(defun populate-source-location-cache (graph)
  "Populate the source location cache from a graph"
  (maphash (lambda (node-id node)
             (let ((location (graph-node-source-location node)))
               (when location
                 (cache-source-location node-id location))))
           (call-graph-nodes graph)))

;;; Source Navigation

(defun parse-node-id (node-id)
  "Parse a node ID like 'PACKAGE:SYMBOL' into a symbol"
  (let ((colon-pos (position #\: node-id)))
    (when colon-pos
      (let ((pkg-name (subseq node-id 0 colon-pos))
            (sym-name (subseq node-id (1+ colon-pos))))
        (let ((pkg (find-package pkg-name)))
          (when pkg
            (find-symbol sym-name pkg)))))))

;;; Highlight Overlay for Source Preview

(defun clear-highlight-overlay ()
  "Remove the current highlight overlay if it exists."
  (when *current-highlight-overlay*
    (lem:delete-overlay *current-highlight-overlay*)
    (setf *current-highlight-overlay* nil)))

(defun clear-highlight-on-command ()
  "Hook function to clear highlight overlay before any command."
  (clear-highlight-overlay))

(lem:add-hook lem:*pre-command-hook* 'clear-highlight-on-command)

(defun highlight-sexp-at-point (point)
  "Create an overlay to highlight the sexp starting at POINT.
Returns the created overlay."
  (lem:with-point ((start point)
                   (end point))
    ;; Move to start of line/sexp
    (lem:line-start start)
    ;; Find end of sexp
    (when (lem:form-offset end 1)
      (lem:make-overlay start end 'source-highlight-attribute))))

(defun find-canvas-window ()
  "Find a window displaying a canvas-buffer."
  (dolist (window (lem:window-list))
    (when (typep (lem:window-buffer window)
                 'lem-living-canvas/buffer:canvas-buffer)
      (return window))))

(defun show-node-source-popup (node-id)
  "Show source code in a popup window, keeping focus on canvas.
Uses pop-to-buffer to display the source, then returns focus to canvas."
  (let ((canvas-window (find-canvas-window)))
    (unless canvas-window
      (return-from show-node-source-popup))
    ;; Clear previous highlight
    (clear-highlight-overlay)
    ;; Use cached location (populated when graph is created via micros RPC)
    (let ((location (get-cached-source-location node-id)))
      (when location
        (destructuring-bind (file . line) location
          (when (and file (probe-file file))
            (let ((buffer (lem:find-file-buffer file)))
              ;; Execute with canvas window as current-window for pop-to-buffer
              (lem:with-current-window canvas-window
                (lem:pop-to-buffer buffer))
              ;; Now switch to source window to move cursor and update display
              (let ((source-window (car (lem:get-buffer-windows buffer))))
                (when source-window
                  (lem:with-current-window source-window
                    ;; Move to the specified line
                    (lem:goto-line line)
                    ;; Move to beginning of line (start of defun)
                    (lem:line-start (lem:current-point))
                    ;; Create highlight overlay for the sexp
                    (setf *current-highlight-overlay*
                          (highlight-sexp-at-point (lem:current-point)))
                    ;; Center the view
                    (lem:window-recenter source-window))))
              ;; Return focus to canvas
              (lem:switch-to-window canvas-window)
              ;; Force redraw
              (lem:redraw-display))))))))

(defun jump-to-node-source (node-id)
  "Jump to the source location of a node.
Uses cached source locations from call graph analysis (populated via micros RPC)."
  (let ((cached (get-cached-source-location node-id)))
    (if cached
        (destructuring-bind (file . line) cached
          (if (probe-file file)
              (progn
                (lem:find-file file)
                (lem:goto-line (or line 1))
                (lem:message "Jumped to ~A" node-id))
              (lem:message "Source file not found: ~A" file)))
        (lem:message "No source location cached for ~A" node-id))))

;;; JSON-RPC Method Registration

(defun register-canvas-methods ()
  "Register JSON-RPC methods for canvas interaction.
Called when lem-server is available (WebView frontend)."
  (let ((pkg (find-package :lem-server)))
    (when pkg
      (let ((register-fn (find-symbol "REGISTER-METHOD" pkg)))
        (when (and register-fn (fboundp register-fn))
          ;; Node selected (single click) - show source in popup
          (funcall register-fn "canvas:node-selected"
                   (lambda (args)
                     (let ((node-id (gethash "nodeId" args)))
                       (lem:send-event
                        (lambda ()
                          (show-node-source-popup node-id))))))

          ;; Open source (double click)
          (funcall register-fn "canvas:open-source"
                   (lambda (args)
                     (let ((node-id (gethash "nodeId" args)))
                       (lem:send-event
                        (lambda ()
                          (jump-to-node-source node-id))))))

          ;; Node moved (drag end)
          (funcall register-fn "canvas:node-moved"
                   (lambda (args)
                     (let ((node-id (gethash "nodeId" args))
                           (x (gethash "x" args))
                           (y (gethash "y" args)))
                       (save-node-position node-id x y)))))))))

(lem:add-hook lem:*after-init-hook* 'register-canvas-methods)

;;; User Commands

(lem:define-command living-canvas (package-name) ((:string "Package: "))
  "Display a function call graph for a package as an interactive canvas.
Double-click a node to jump to its source code.
Single-click to see function details.
Drag nodes to rearrange the layout.
Requires a Lisp connection (via micros)."
  (check-connection)
  ;; Package validation happens on the connected runtime via micros
  (let* ((source-buffer (lem:current-buffer))
         (graph (analyze-package (string-upcase package-name)))
         (node-count (hash-table-count (call-graph-nodes graph))))
    (cond
      ((zerop node-count)
       (lem:message "No functions found in package ~A" package-name))
      (t
       ;; Pre-populate cache for fast source navigation
       (populate-source-location-cache graph)
       (let ((canvas-buffer (make-canvas-buffer
                             (format nil "*Canvas: ~A*" package-name)
                             source-buffer
                             graph)))
         (lem:pop-to-buffer canvas-buffer)
         (lem:change-buffer-mode canvas-buffer 'living-canvas-mode)
         (lem:message "Living Canvas: ~D functions in ~A"
                      node-count package-name))))))

(lem:define-command living-canvas-current-file () ()
  "Display a call graph for the current file.
Only shows functions defined in this file.
Requires a Lisp connection (via micros)."
  (check-connection)
  (let* ((buffer (lem:current-buffer))
         (filename (lem:buffer-filename buffer)))
    (unless filename
      (lem:editor-error "Buffer has no associated file"))
    (unless (probe-file filename)
      (lem:editor-error "File not found: ~A" filename))
    (let* ((graph (analyze-file filename))
           (node-count (if graph
                           (hash-table-count (call-graph-nodes graph))
                           0)))
      (cond
        ((zerop node-count)
         (lem:message "No functions found in ~A (file may need to be loaded first)"
                      (file-namestring filename)))
        (t
         (populate-source-location-cache graph)
         (let ((canvas-buffer (make-canvas-buffer
                               (format nil "*Canvas: ~A*" (file-namestring filename))
                               buffer
                               graph)))
           (lem:pop-to-buffer canvas-buffer)
           (lem:change-buffer-mode canvas-buffer 'living-canvas-mode)
           (lem:message "Living Canvas: ~D functions in ~A"
                        node-count (file-namestring filename))))))))

(lem:define-command living-canvas-system (system-name) ((:string "System: "))
  "Display a function call graph for an ASDF system.
Shows all functions defined in the system and their call relationships,
including cross-package calls within the system.
Requires a Lisp connection (via micros)."
  (check-connection)
  ;; System validation happens on the connected runtime via micros
  (let* ((source-buffer (lem:current-buffer))
         (graph (analyze-system system-name))
         (node-count (hash-table-count (call-graph-nodes graph))))
    (cond
      ((zerop node-count)
       (lem:message "No functions found in system ~A" system-name))
      (t
       (populate-source-location-cache graph)
       (let ((canvas-buffer (make-canvas-buffer
                             (format nil "*Canvas: ~A*" system-name)
                             source-buffer
                             graph)))
         (lem:pop-to-buffer canvas-buffer)
         (lem:change-buffer-mode canvas-buffer 'living-canvas-mode)
         (lem:message "Living Canvas: ~D functions in system ~A"
                      node-count system-name))))))

(lem:define-command living-canvas-refresh () ()
  "Refresh the current canvas view"
  (let ((buffer (lem:current-buffer)))
    (cond
      ((typep buffer 'lem-living-canvas/buffer:canvas-buffer)
       (lem-living-canvas/buffer:update-canvas-buffer buffer)
       (lem:message "Canvas refreshed"))
      (t
       (lem:message "Not in a canvas buffer")))))

;;; Multi-Language Support via Provider Registry

(defun analyze-with-provider (source language)
  "Analyze SOURCE using a registered provider for LANGUAGE.

Arguments:
  SOURCE   - The source to analyze (buffer or pathname)
  LANGUAGE - Language keyword (e.g., :python)

Returns:
  A call-graph structure, or nil if no suitable provider is found"
  (let ((provider (find-provider *provider-registry* language source)))
    (cond
      (provider
       (provider-analyze provider source))
      ((eq language :common-lisp)
       ;; Fallback to micros provider for Common Lisp
       (typecase source
         (lem:buffer (analyze-file (lem:buffer-filename source)))
         (pathname (analyze-file source))
         (t nil)))
      (t nil))))

(defun supported-language-p (language)
  "Return T if LANGUAGE has a registered provider or is Common Lisp.

Arguments:
  LANGUAGE - Language keyword (e.g., :python)

Returns:
  T if analysis is available for this language"
  (or (eq language :common-lisp)
      (find-provider *provider-registry* language)))

(defun get-provider-status (language)
  "Get detailed status information about provider availability for LANGUAGE.

Arguments:
  LANGUAGE - Language keyword (e.g., :python)

Returns:
  A string describing the provider status"
  (cond
    ((eq language :common-lisp)
     (if (connected-p)
         "Common Lisp (micros connection active)"
         "Common Lisp (requires micros connection - use M-x slime)"))
    (t
     (let ((providers (call-graph:list-providers *provider-registry* language)))
       (if providers
           (format nil "~A (provider: ~A)"
                   language
                   (call-graph:provider-name (first providers)))
           (format nil "~A (no provider - tree-sitter grammar may not be installed)"
                   language))))))

(lem:define-command living-canvas-current-buffer () ()
  "Display a call graph for the current buffer.

Automatically detects the programming language and selects the
appropriate provider. Supports:
  - Common Lisp (via micros connection)
  - Python, JavaScript, TypeScript (via tree-sitter providers)

The command uses the provider registry to find the best available
provider for the detected language."
  (let* ((buffer (lem:current-buffer))
         (filename (lem:buffer-filename buffer))
         (language (detect-language buffer)))
    (unless filename
      (lem:editor-error "Buffer has no associated file"))
    (unless (probe-file filename)
      (lem:editor-error "File not found: ~A" filename))
    (unless language
      (lem:editor-error "Cannot detect language for ~A" (file-namestring filename)))
    (unless (supported-language-p language)
      (lem:editor-error "No call graph provider available for ~A~%~A"
                        language
                        (get-provider-status language)))

    ;; For Common Lisp, require micros connection
    (when (eq language :common-lisp)
      (check-connection))

    (let* ((graph (analyze-with-provider buffer language))
           (node-count (if graph
                           (hash-table-count (call-graph-nodes graph))
                           0)))
      (cond
        ((zerop node-count)
         (lem:message "No functions found in ~A" (file-namestring filename)))
        (t
         (populate-source-location-cache graph)
         (let ((canvas-buffer (make-canvas-buffer
                               (format nil "*Canvas: ~A*" (file-namestring filename))
                               buffer
                               graph)))
           (lem:pop-to-buffer canvas-buffer)
           (lem:change-buffer-mode canvas-buffer 'living-canvas-mode)
           (lem:message "Living Canvas: ~D functions in ~A (~A)"
                        node-count (file-namestring filename)
                        language)))))))

;;; Provider Management

(lem:define-command living-canvas-reload-providers () ()
  "Reload and re-register all tree-sitter providers.

Use this command if providers failed to register at startup
because tree-sitter grammars were not yet available."
  (let ((count 0))
    ;; Try to register Python provider
    (ignore-errors
      (unless (find-provider *provider-registry* :python)
        (let ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider)))
          (when (slot-value provider 'lem-living-canvas/python::language)
            (call-graph:register-provider *provider-registry* provider '(:python))
            (incf count)))))
    ;; Try to register JavaScript provider
    (ignore-errors
      (unless (find-provider *provider-registry* :javascript)
        (let ((provider (make-instance 'lem-living-canvas/javascript:tree-sitter-js-provider)))
          (when (slot-value provider 'lem-living-canvas/javascript::language)
            (call-graph:register-provider *provider-registry* provider '(:javascript :typescript))
            (incf count)))))
    ;; Try to register Go provider
    (ignore-errors
      (unless (find-provider *provider-registry* :go)
        (let ((provider (make-instance 'lem-living-canvas/go:tree-sitter-go-provider)))
          (when (slot-value provider 'lem-living-canvas/go::language)
            (call-graph:register-provider *provider-registry* provider '(:go))
            (incf count)))))
    (if (zerop count)
        (lem:message "Providers already registered or tree-sitter grammars not available.")
        (lem:message "Registered ~D provider(s). Use M-x living-canvas-diagnose to verify." count))))

;;; Diagnostics

(lem:define-command living-canvas-diagnose () ()
  "Show diagnostic information about Living Canvas providers.

Displays the status of all registered providers and their capabilities.
Useful for troubleshooting when providers are not working."
  (let ((ts-available (and (find-package :tree-sitter)
                           (ignore-errors
                             (funcall (find-symbol "TREE-SITTER-AVAILABLE-P" :tree-sitter)))))
        (providers (call-graph:list-providers *provider-registry*))
        (lisp-connected (connected-p)))
    (lem:with-pop-up-typeout-window (out (lem:make-buffer "*Living Canvas Diagnostics*") :erase t)
      (format out "=== Living Canvas Provider Diagnostics ===~%~%")

      ;; Tree-sitter status
      (format out "Tree-sitter Status:~%")
      (format out "  Available: ~A~%" (if ts-available "Yes" "No"))
      (when ts-available
        (dolist (lang '("python" "javascript" "typescript" "go"))
          (let ((loaded (ignore-errors
                          (funcall (find-symbol "GET-LANGUAGE" :tree-sitter) lang))))
            (format out "  ~A grammar: ~A~%" lang (if loaded "Loaded" "Not loaded")))))
      (format out "~%")

      ;; Common Lisp status
      (format out "Common Lisp Status:~%")
      (format out "  Micros connection: ~A~%" (if lisp-connected "Connected" "Not connected"))
      (format out "~%")

      ;; Registered providers
      (format out "Registered Providers:~%")
      (if providers
          (dolist (provider providers)
            (format out "  ~A (priority: ~A, languages: ~A)~%"
                    (call-graph:provider-name provider)
                    (call-graph:provider-priority provider)
                    (call-graph:provider-languages provider)))
          (format out "  (none)~%"))
      (format out "~%")

      ;; Language support summary
      (format out "Language Support:~%")
      (dolist (lang '(:common-lisp :python :javascript :typescript :go))
        (format out "  ~A: ~A~%" lang (get-provider-status lang))))))

;;; Minor Mode

(lem:define-minor-mode living-canvas-mode
    (:name "LivingCanvas"
     :keymap *living-canvas-keymap*))

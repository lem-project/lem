(defpackage :lem-living-canvas
  (:use :cl :lem)
  (:import-from :lem-living-canvas/call-graph
                #:analyze-package
                #:analyze-file
                #:analyze-buffer
                #:analyze-system
                #:graph-to-cytoscape-json
                #:get-source-location)
  (:import-from :lem-living-canvas/buffer
                #:canvas-buffer
                #:make-canvas-buffer
                #:canvas-buffer-graph
                #:canvas-buffer-source-buffer)
  (:export #:living-canvas
           #:living-canvas-current-file
           #:living-canvas-system
           #:living-canvas-refresh))
(in-package :lem-living-canvas)

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
             (let ((location (lem-living-canvas/call-graph:graph-node-source-location node)))
               (when location
                 (cache-source-location node-id location))))
           (lem-living-canvas/call-graph:call-graph-nodes graph)))

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
    (let ((location (or (get-cached-source-location node-id)
                        (let ((symbol (parse-node-id node-id)))
                          (when symbol
                            (let ((loc (get-source-location symbol)))
                              (when loc
                                (cache-source-location node-id loc)
                                loc)))))))
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
Uses cached source locations from call graph analysis, falling back to
runtime introspection if needed."
  (let ((cached (get-cached-source-location node-id)))
    (if cached
        (destructuring-bind (file . line) cached
          (when (probe-file file)
            (lem:find-file file)
            (lem:goto-line line)
            (lem:message "Jumped to ~A" node-id)))
        (let ((symbol (parse-node-id node-id)))
          (when symbol
            (let ((location (get-source-location symbol)))
              (when location
                (cache-source-location node-id location)
                (destructuring-bind (file . line) location
                  (when (probe-file file)
                    (lem:find-file file)
                    (lem:goto-line line)
                    (lem:message "Jumped to ~A" node-id))))))))))

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
Drag nodes to rearrange the layout."
  (let ((pkg (find-package (string-upcase package-name))))
    (unless pkg
      (lem:editor-error "Package not found: ~A" package-name))
    (let* ((source-buffer (lem:current-buffer))
           (graph (analyze-package pkg))
           (node-count (hash-table-count
                        (lem-living-canvas/call-graph:call-graph-nodes graph))))
      (if (zerop node-count)
          (lem:message "No functions found in package ~A" package-name)
          (progn
            ;; Pre-populate cache for fast source navigation
            (populate-source-location-cache graph)
            (let ((canvas-buffer (make-canvas-buffer
                                  (format nil "*Canvas: ~A*" package-name)
                                  source-buffer
                                  graph)))
              (lem:pop-to-buffer canvas-buffer)
              (lem:change-buffer-mode canvas-buffer 'living-canvas-mode)
              (lem:message "Living Canvas: ~D functions in ~A"
                           node-count package-name)))))))

(lem:define-command living-canvas-current-file () ()
  "Display a call graph for the current file.
Only shows functions defined in this file."
  (let* ((buffer (lem:current-buffer))
         (filename (lem:buffer-filename buffer)))
    (unless filename
      (lem:editor-error "Buffer has no associated file"))
    (unless (probe-file filename)
      (lem:editor-error "File not found: ~A" filename))
    (let* ((graph (lem-living-canvas/call-graph:analyze-file filename))
           (node-count (if graph
                           (hash-table-count
                            (lem-living-canvas/call-graph:call-graph-nodes graph))
                           0)))
      (if (zerop node-count)
          (lem:message "No functions found in ~A (file may need to be loaded first)"
                       (file-namestring filename))
          (progn
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
including cross-package calls within the system."
  (let ((system (asdf:find-system system-name nil)))
    (unless system
      (lem:editor-error "System not found: ~A" system-name))
    (let* ((source-buffer (lem:current-buffer))
           (graph (analyze-system system-name))
           (node-count (hash-table-count
                        (lem-living-canvas/call-graph:call-graph-nodes graph))))
      (if (zerop node-count)
          (lem:message "No functions found in system ~A" system-name)
          (progn
            (populate-source-location-cache graph)
            (let ((canvas-buffer (make-canvas-buffer
                                  (format nil "*Canvas: ~A*" system-name)
                                  source-buffer
                                  graph)))
              (lem:pop-to-buffer canvas-buffer)
              (lem:change-buffer-mode canvas-buffer 'living-canvas-mode)
              (lem:message "Living Canvas: ~D functions in system ~A"
                           node-count system-name)))))))

(lem:define-command living-canvas-lem-core () ()
  "Display a call graph for lem-core package (demo)"
  (living-canvas "LEM-CORE"))

(lem:define-command living-canvas-refresh () ()
  "Refresh the current canvas view"
  (let ((buffer (lem:current-buffer)))
    (if (typep buffer 'lem-living-canvas/buffer:canvas-buffer)
        (progn
          (lem-living-canvas/buffer:update-canvas-buffer buffer)
          (lem:message "Canvas refreshed"))
        (lem:message "Not in a canvas buffer"))))

;;; Minor Mode

(lem:define-minor-mode living-canvas-mode
    (:name "LivingCanvas"
     :keymap *living-canvas-keymap*))

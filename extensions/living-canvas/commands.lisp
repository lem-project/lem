(in-package :lem-living-canvas)

;;; Node Position Storage

(defvar *node-positions* (make-hash-table :test 'equal)
  "Global storage for node positions across sessions")

(defun save-node-position (node-id x y)
  "Save a node's position for persistence"
  (setf (gethash node-id *node-positions*) (cons x y)))

(defun get-node-position (node-id)
  "Get a saved node position"
  (gethash node-id *node-positions*))

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

(defun jump-to-node-source (node-id)
  "Jump to the source location of a node"
  (let ((symbol (parse-node-id node-id)))
    (when symbol
      (let ((location (lem-living-canvas/call-graph::get-source-location symbol)))
        (when location
          (destructuring-bind (file . line) location
            (when (probe-file file)
              ;; Open file and go to line
              (lem:find-file file)
              (lem:goto-line line)
              (lem:message "Jumped to ~A" node-id))))))))

;;; JSON-RPC Method Registration

(defun register-canvas-methods ()
  "Register JSON-RPC methods for canvas interaction.
Called when lem-server is available (WebView frontend)."
  (let ((pkg (find-package :lem-server)))
    (when pkg
      (let ((register-fn (find-symbol "REGISTER-METHOD" pkg)))
        (when (and register-fn (fboundp register-fn))
          ;; Node selected (single click)
          (funcall register-fn "canvas:node-selected"
                   (lambda (args)
                     (let ((node-id (gethash "nodeId" args)))
                       (lem:send-event
                        (lambda ()
                          (lem:message "Selected: ~A" node-id))))))

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

;; Register methods after editor initialization
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
          (let ((canvas-buffer (make-canvas-buffer
                                (format nil "*Canvas: ~A*" package-name)
                                source-buffer
                                graph)))
            (lem:pop-to-buffer canvas-buffer)
            (lem:message "Living Canvas: ~D functions in ~A"
                         node-count package-name))))))

(lem:define-command living-canvas-current-file () ()
  "Display a call graph for all packages defined in the current file"
  (let ((buffer (lem:current-buffer)))
    (if (lem:buffer-filename buffer)
        (let ((pkg (lem:buffer-package buffer)))
          (if pkg
              (living-canvas (package-name pkg))
              (lem:message "No package found for current buffer")))
        (lem:message "Buffer has no associated file"))))

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

;;; Keymap

(defvar *living-canvas-keymap*
  (make-instance 'lem:keymap :name '*living-canvas-keymap*))

(lem:define-key *living-canvas-keymap* "g" 'living-canvas-refresh)
(lem:define-key *living-canvas-keymap* "q" 'lem:kill-buffer)

;;; Minor mode for canvas buffers

(lem:define-minor-mode living-canvas-mode
    (:name "LivingCanvas"
     :keymap *living-canvas-keymap*)
  (setf (lem:variable-value 'lem:truncate-lines :buffer (lem:current-buffer)) nil))

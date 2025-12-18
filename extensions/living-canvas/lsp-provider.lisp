(defpackage #:lem-living-canvas/lsp-provider
  (:use #:cl #:call-graph)
  (:import-from #:alexandria #:when-let)
  (:local-nicknames (:lsp-ch :lem-living-canvas/lsp-call-hierarchy)
                    (:cg-lsp :call-graph-lsp))
  (:export #:lsp-call-hierarchy-provider
           #:collect-file-call-hierarchy
           #:collect-file-call-hierarchy-async
           #:*lsp-analysis-in-progress*
           #:*lsp-analysis-cancel-flag*
           #:*lsp-request-delay-ms*))
(in-package #:lem-living-canvas/lsp-provider)

;;; LSP Call Hierarchy Provider
;;;
;;; This provider uses the LSP Call Hierarchy API to analyze code.
;;; It works with any language server that supports the Call Hierarchy protocol.
;;; The core conversion logic is delegated to call-graph-lsp package.

;;; ============================================================
;;; Lem-specific helper functions
;;; ============================================================

(defun collect-callable-symbols (buffer)
  "Get all callable symbols (functions, methods, constructors) from BUFFER.

Uses textDocument/documentSymbol to get all symbols, then filters to callables.
Returns a list of DocumentSymbol objects."
  (when-let ((symbols (lsp-ch:get-document-symbols buffer)))
    (let ((all-symbols (lsp-ch:flatten-document-symbols symbols)))
      (remove-if-not #'lsp-ch:document-symbol-is-callable-p all-symbols))))

(defvar *lsp-connection-error* nil
  "Flag set when LSP connection error occurs.")

(defvar *max-symbols-to-analyze* nil
  "Maximum number of symbols to analyze. Set to nil for no limit.")

(defvar *lsp-request-delay-ms* 100
  "Delay between LSP requests in milliseconds to avoid overwhelming the server.")

(defvar *lsp-request-max-retries* 3
  "Maximum number of retries for failed LSP requests.")

(defvar *lsp-analysis-in-progress* nil
  "Non-nil when LSP analysis is running in background.")

(defvar *lsp-analysis-cancel-flag* nil
  "Set to T to request cancellation of running analysis.")

(defun prepare-call-hierarchy-items (buffer)
  "Collect CallHierarchyItem objects for all callable symbols in BUFFER.

Returns a list of CallHierarchyItem objects suitable for use with
call-graph-lsp:build-call-graph-from-hierarchy."
  (setf *lsp-connection-error* nil)
  (let* ((items '())
         (all-callable-symbols (collect-callable-symbols buffer))
         (callable-symbols (if (and *max-symbols-to-analyze*
                                    (> (length all-callable-symbols) *max-symbols-to-analyze*))
                               (progn
                                 (lem:message "Limiting to ~D of ~D symbols"
                                              *max-symbols-to-analyze*
                                              (length all-callable-symbols))
                                 (subseq all-callable-symbols 0 *max-symbols-to-analyze*))
                               all-callable-symbols))
         (total (length callable-symbols))
         (current 0)
         (error-count 0))
    (lem:message "Preparing ~D symbols..." total)
    (dolist (doc-symbol callable-symbols)
      ;; Abort if too many errors (likely connection lost)
      (when (> error-count 3)
        (lem:message "Too many LSP errors, aborting...")
        (setf *lsp-connection-error* t)
        (return))
      (incf current)
      (when (zerop (mod current 5))
        (lem:message "Preparing symbols... ~D/~D" current total)
        (lem:redraw-display))
      (handler-case
          (let ((point (lsp-ch:document-symbol-to-position doc-symbol buffer)))
            (when-let ((prepared (lsp-ch:prepare-call-hierarchy buffer point)))
              (push (elt prepared 0) items)
              (setf error-count 0)))  ; Reset on success
        (error (e)
          (incf error-count)
          (when (> error-count 3)
            (lem:message "LSP connection error: ~A" e)))))
    (nreverse items)))

(defun make-safe-incoming-calls-fn ()
  "Create a callback function for fetching incoming calls with error handling."
  (lambda (item)
    (handler-case
        (lsp-ch:get-incoming-calls item)
      (error () nil))))

(defun make-safe-outgoing-calls-fn ()
  "Create a callback function for fetching outgoing calls with error handling."
  (lambda (item)
    (handler-case
        (lsp-ch:get-outgoing-calls item)
      (error () nil))))

;;; ============================================================
;;; Retry and Throttling Support
;;; ============================================================

(defun retry-with-backoff (fn &key (max-retries *lsp-request-max-retries*)
                                   (initial-delay 0.2))
  "Execute FN with exponential backoff on failure.

Retries up to MAX-RETRIES times, with delays of INITIAL-DELAY seconds
that double after each failure. Returns NIL if all retries fail."
  (loop :for attempt :from 1 :to max-retries
        :for delay := initial-delay :then (* delay 2)
        :do (handler-case
                (return (funcall fn))
              (error ()
                (when (= attempt max-retries)
                  (return nil))
                (sleep delay)))))

(defun make-throttled-incoming-calls-fn ()
  "Create a callback that fetches incoming calls with throttling and retry."
  (lambda (item)
    (when *lsp-analysis-cancel-flag*
      (error "Analysis cancelled"))
    (sleep (/ *lsp-request-delay-ms* 1000.0))
    (retry-with-backoff
     (lambda () (lsp-ch:get-incoming-calls item)))))

(defun make-throttled-outgoing-calls-fn ()
  "Create a callback that fetches outgoing calls with throttling and retry."
  (lambda (item)
    (when *lsp-analysis-cancel-flag*
      (error "Analysis cancelled"))
    (sleep (/ *lsp-request-delay-ms* 1000.0))
    (retry-with-backoff
     (lambda () (lsp-ch:get-outgoing-calls item)))))

;;; ============================================================
;;; Collect File Call Hierarchy
;;; ============================================================

(defun collect-file-call-hierarchy (buffer &key (include-incoming nil)
                                                (include-outgoing t))
  "Collect call hierarchy for all callable symbols in BUFFER.

INCLUDE-INCOMING - if T, fetch incoming calls (callers). Default: NIL
INCLUDE-OUTGOING - if T, fetch outgoing calls (callees). Default: T

Returns a call-graph structure containing all functions and their call relationships.
This function uses the LSP Call Hierarchy API via call-graph-lsp."
  (unless (lsp-ch:buffer-has-call-hierarchy-p buffer)
    (return-from collect-file-call-hierarchy (make-call-graph)))
  (let ((items (prepare-call-hierarchy-items buffer))
        (total 0))
    ;; Check for connection errors during preparation
    (when *lsp-connection-error*
      (lem:message "LSP connection lost. Showing partial results.")
      (lem:redraw-display))
    (setf total (length items))
    (when (zerop total)
      (if *lsp-connection-error*
          (lem:editor-error "LSP connection lost during analysis")
          (return-from collect-file-call-hierarchy (make-call-graph))))
    (lem:message "Analyzing ~D symbols..." total)
    ;; Use call-graph-lsp to build the graph
    (cg-lsp:build-call-graph-from-hierarchy
     items
     (make-safe-incoming-calls-fn)
     (make-safe-outgoing-calls-fn)
     :include-incoming include-incoming
     :include-outgoing include-outgoing
     :progress-fn (lambda (current total)
                    (when (zerop (mod current 5))
                      (lem:message "Analyzing calls... ~D/~D" current total)
                      (lem:redraw-display))))))

;;; ============================================================
;;; Async Call Hierarchy Collection
;;; ============================================================

(defun prepare-call-hierarchy-items-throttled (buffer)
  "Prepare call hierarchy items with throttling between requests.

Unlike `prepare-call-hierarchy-items`, this function:
- Has no symbol limit
- Inserts delays between LSP requests
- Supports cancellation via `*lsp-analysis-cancel-flag*`"
  (setf *lsp-connection-error* nil)
  (let* ((items '())
         (callable-symbols (collect-callable-symbols buffer))
         (total (length callable-symbols))
         (current 0)
         (error-count 0))
    (dolist (doc-symbol callable-symbols)
      ;; Check for cancellation
      (when *lsp-analysis-cancel-flag*
        (error "Analysis cancelled"))
      ;; Abort if too many consecutive errors
      (when (> error-count 3)
        (setf *lsp-connection-error* t)
        (return))
      (incf current)
      ;; Progress update every 5 items
      (when (zerop (mod current 5))
        (lem:send-event
         (lambda ()
           (lem:message "Preparing symbols... ~D/~D" current total)
           (lem:redraw-display))))
      ;; Throttle requests
      (sleep (/ *lsp-request-delay-ms* 1000.0))
      ;; Make LSP request with retry
      (let ((result (retry-with-backoff
                     (lambda ()
                       (let ((point (lsp-ch:document-symbol-to-position doc-symbol buffer)))
                         (lsp-ch:prepare-call-hierarchy buffer point))))))
        (cond
          (result
           (push (elt result 0) items)
           (setf error-count 0))
          (t
           (incf error-count)))))
    (nreverse items)))

(defun collect-file-call-hierarchy-throttled (buffer &key include-incoming include-outgoing)
  "Collect call hierarchy with throttled requests and cancellation support.

This is the internal function used by async collection."
  (let ((items (prepare-call-hierarchy-items-throttled buffer)))
    ;; Check for cancellation
    (when *lsp-analysis-cancel-flag*
      (error "Analysis cancelled"))
    ;; Check for connection errors during preparation
    (when *lsp-connection-error*
      (lem:send-event
       (lambda ()
         (lem:message "LSP connection issues. Showing partial results."))))
    (when (zerop (length items))
      (return-from collect-file-call-hierarchy-throttled (make-call-graph)))
    ;; Build graph with throttled callbacks
    (cg-lsp:build-call-graph-from-hierarchy
     items
     (make-throttled-incoming-calls-fn)
     (make-throttled-outgoing-calls-fn)
     :include-incoming include-incoming
     :include-outgoing include-outgoing
     :progress-fn (lambda (current total)
                    (when *lsp-analysis-cancel-flag*
                      (error "Analysis cancelled"))
                    (when (zerop (mod current 5))
                      (lem:send-event
                       (lambda ()
                         (lem:message "Analyzing calls... ~D/~D" current total)
                         (lem:redraw-display))))))))

(defun collect-file-call-hierarchy-async (buffer callback
                                          &key (include-incoming nil)
                                               (include-outgoing t))
  "Analyze BUFFER in a background thread.

CALLBACK is called with (graph error) when complete, via send-event.
The GRAPH argument is a call-graph structure on success, or NIL on error.
The ERROR argument is NIL on success, or the error condition on failure.

Example:
  (collect-file-call-hierarchy-async
   buffer
   (lambda (graph error)
     (if error
         (message \"Failed: ~A\" error)
         (display-graph graph))))"
  (unless (lsp-ch:buffer-has-call-hierarchy-p buffer)
    (funcall callback (make-call-graph) nil)
    (return-from collect-file-call-hierarchy-async))
  (when *lsp-analysis-in-progress*
    (error "Analysis already in progress"))
  (setf *lsp-analysis-in-progress* t
        *lsp-analysis-cancel-flag* nil)
  (bt2:make-thread
   (lambda ()
     (handler-case
         (let ((graph (collect-file-call-hierarchy-throttled
                       buffer
                       :include-incoming include-incoming
                       :include-outgoing include-outgoing)))
           (setf *lsp-analysis-in-progress* nil)
           (lem:send-event (lambda () (funcall callback graph nil))))
       (error (e)
         (setf *lsp-analysis-in-progress* nil)
         (lem:send-event (lambda () (funcall callback nil e))))))
   :name "living-canvas-lsp-worker"))

;;; ============================================================
;;; Task C-2: LSP Provider Class
;;; ============================================================

(defclass lsp-call-hierarchy-provider (call-graph-provider)
  ()
  (:documentation "Call graph provider using LSP Call Hierarchy API.
Works with any language server that supports the Call Hierarchy protocol."))

(defmethod provider-name ((provider lsp-call-hierarchy-provider))
  :lsp-call-hierarchy)

(defmethod provider-priority ((provider lsp-call-hierarchy-provider))
  20)  ; Higher priority than micros provider (10)

(defmethod provider-supports-p ((provider lsp-call-hierarchy-provider) source)
  "Return T if SOURCE is a buffer with LSP call hierarchy support."
  (and (typep source 'lem:buffer)
       (lsp-ch:buffer-has-call-hierarchy-p source)))

(defmethod provider-analyze ((provider lsp-call-hierarchy-provider) source &key)
  "Analyze SOURCE buffer using LSP Call Hierarchy API.
Returns a call-graph structure."
  (if (typep source 'lem:buffer)
      (collect-file-call-hierarchy source)
      (make-call-graph)))

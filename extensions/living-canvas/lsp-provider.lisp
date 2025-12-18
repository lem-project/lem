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
;;; Async Call Hierarchy Collection (Idle Timer Based)
;;; ============================================================
;;;
;;; LSP requests must run on the main thread (not thread-safe).
;;; We use idle timers to process symbols in batches, yielding
;;; between batches to keep the UI responsive.

(defvar *lsp-batch-size* 5
  "Number of items to process per timer tick.")

(defvar *lsp-async-timer* nil
  "Current idle timer for async processing.")

(defstruct lsp-async-state
  "State for incremental LSP analysis."
  buffer
  callback
  include-incoming
  include-outgoing
  ;; Phase 1: Preparing items
  doc-symbols           ; remaining document symbols to process
  prepared-items        ; collected CallHierarchyItem objects
  ;; Phase 2: Building graph (handled by build-call-graph)
  phase                 ; :preparing or :building
  total-symbols
  current-symbol
  error-count)

(defvar *lsp-async-state* nil
  "Current async analysis state.")

(defun lsp-async-cleanup ()
  "Clean up async analysis state."
  (when *lsp-async-timer*
    (lem:stop-timer *lsp-async-timer*)
    (setf *lsp-async-timer* nil))
  (setf *lsp-async-state* nil
        *lsp-analysis-in-progress* nil))

(defun lsp-async-tick ()
  "Process one batch of LSP analysis. Called by idle timer."
  (when (or (null *lsp-async-state*)
            *lsp-analysis-cancel-flag*)
    ;; Cancelled or no state
    (let ((callback (and *lsp-async-state*
                         (lsp-async-state-callback *lsp-async-state*))))
      (lsp-async-cleanup)
      (when callback
        (funcall callback nil (make-condition 'simple-error
                                              :format-control "Analysis cancelled"))))
    (return-from lsp-async-tick))

  (let ((state *lsp-async-state*))
    (case (lsp-async-state-phase state)
      (:preparing
       (lsp-async-prepare-batch state))
      (:building
       (lsp-async-build-graph state)))))

(defun lsp-async-prepare-batch (state)
  "Process a batch of document symbols to prepare CallHierarchyItems."
  (let ((buffer (lsp-async-state-buffer state))
        (remaining (lsp-async-state-doc-symbols state))
        (items (lsp-async-state-prepared-items state))
        (error-count (lsp-async-state-error-count state))
        (processed 0))
    ;; Process up to *lsp-batch-size* symbols
    (loop :while (and remaining (< processed *lsp-batch-size*))
          :do
             ;; Too many consecutive errors - abort
             (when (> error-count 3)
               (setf *lsp-connection-error* t)
               (setf (lsp-async-state-doc-symbols state) nil)
               (return))
             (let ((doc-symbol (pop remaining)))
               (incf processed)
               (incf (lsp-async-state-current-symbol state))
               (handler-case
                   (let ((point (lsp-ch:document-symbol-to-position doc-symbol buffer)))
                     (when-let ((prepared (lsp-ch:prepare-call-hierarchy buffer point)))
                       (push (elt prepared 0) items)
                       (setf error-count 0)))
                 (error ()
                   (incf error-count)))))
    ;; Update state
    (setf (lsp-async-state-doc-symbols state) remaining
          (lsp-async-state-prepared-items state) items
          (lsp-async-state-error-count state) error-count)
    ;; Progress update
    (lem:message "Preparing symbols... ~D/~D"
                 (lsp-async-state-current-symbol state)
                 (lsp-async-state-total-symbols state))
    (lem:redraw-display)
    ;; Check if done with preparation
    (when (null remaining)
      (setf (lsp-async-state-prepared-items state) (nreverse items)
            (lsp-async-state-phase state) :building))))

(defun lsp-async-build-graph (state)
  "Build the call graph from prepared items."
  ;; Stop the timer - we'll do this synchronously but with progress
  (when *lsp-async-timer*
    (lem:stop-timer *lsp-async-timer*)
    (setf *lsp-async-timer* nil))

  (let ((items (lsp-async-state-prepared-items state))
        (callback (lsp-async-state-callback state))
        (include-incoming (lsp-async-state-include-incoming state))
        (include-outgoing (lsp-async-state-include-outgoing state)))
    (handler-case
        (let ((graph
                (if (zerop (length items))
                    (make-call-graph)
                    (cg-lsp:build-call-graph-from-hierarchy
                     items
                     (make-safe-incoming-calls-fn)
                     (make-safe-outgoing-calls-fn)
                     :include-incoming include-incoming
                     :include-outgoing include-outgoing
                     :progress-fn (lambda (current total)
                                    (when *lsp-analysis-cancel-flag*
                                      (error "Analysis cancelled"))
                                    (when (zerop (mod current 5))
                                      (lem:message "Analyzing calls... ~D/~D" current total)
                                      (lem:redraw-display)))))))
          (lsp-async-cleanup)
          (funcall callback graph nil))
      (error (e)
        (lsp-async-cleanup)
        (funcall callback nil e)))))

(defun collect-file-call-hierarchy-async (buffer callback
                                          &key (include-incoming nil)
                                               (include-outgoing t))
  "Analyze BUFFER incrementally using idle timers.

CALLBACK is called with (graph error) when complete.
The GRAPH argument is a call-graph structure on success, or NIL on error.
The ERROR argument is NIL on success, or the error condition on failure.

Analysis runs on the main thread in small batches, yielding between
batches to keep the UI responsive. Use `*lsp-analysis-cancel-flag*`
to cancel.

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

  ;; Get all callable symbols upfront
  (let ((doc-symbols (collect-callable-symbols buffer)))
    (when (zerop (length doc-symbols))
      (funcall callback (make-call-graph) nil)
      (return-from collect-file-call-hierarchy-async))

    ;; Initialize state
    (setf *lsp-analysis-in-progress* t
          *lsp-analysis-cancel-flag* nil
          *lsp-connection-error* nil
          *lsp-async-state* (make-lsp-async-state
                             :buffer buffer
                             :callback callback
                             :include-incoming include-incoming
                             :include-outgoing include-outgoing
                             :doc-symbols doc-symbols
                             :prepared-items '()
                             :phase :preparing
                             :total-symbols (length doc-symbols)
                             :current-symbol 0
                             :error-count 0))

    ;; Start idle timer
    (lem:message "Starting analysis of ~D symbols..." (length doc-symbols))
    (setf *lsp-async-timer*
          (lem:start-timer (lem:make-idle-timer 'lsp-async-tick
                                                :name "lsp-async-analysis")
                           *lsp-request-delay-ms*
                           :repeat t))))

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

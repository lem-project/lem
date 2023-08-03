(defpackage :lem-lisp-mode/v2/eval
  (:use :cl
        :lem)
  (:import-from :lem-lsp-mode
                :register-lsp-method
                :get-buffer-from-text-document-identifier)
  (:import-from :lem-lsp-base/converter
                :convert-to-json
                :convert-from-json)
  (:shadowing-import-from :lem-language-client/request
                          :execute-command)
  (:import-from :lem-language-server
                :start-eval-params
                :start-eval-params-range
                :start-eval-params-id
                :start-eval-params-text-document
                :show-eval-result-params
                :show-eval-result-params-type
                :show-eval-result-params-id
                :show-eval-result-params-text-document
                :show-eval-result-params-message)
  (:export :register-eval-methods))
(in-package :lem-lisp-mode/v2/eval)

(define-key lem-lisp-mode:*lisp-mode-keymap* "M-Return" 'lisp/eval-at-point)

(defun message-type-to-attribute (message-type)
  (alexandria:switch (message-type)
    (lsp:message-type-warning
     (make-attribute :foreground "yellow"
                     :background "dark yellow"))
    (lsp:message-type-error
     (make-attribute :foreground "white"
                     :background "dark red"))
    (t
     (make-attribute :foreground "cyan"
                     :background "dark cyan"))))

(defun get-client (buffer)
  (lem-lsp-mode::workspace-client (lem-lsp-mode::buffer-workspace buffer)))

(define-command lisp/eval-at-point () ()
  (execute-command (get-client (current-buffer))
                   "cl-lsp.eval-last-expression"
                   (convert-to-json
                    (lem-lsp-mode::make-text-document-position-params
                     (current-point)))))

(define-command lisp/clear-eval-results () ()
  (clear-eval-results (current-buffer)))

(define-command lisp/interrupt () ()
  (dolist (spinner (lem/loading-spinner:get-line-spinners (current-point)))
    (execute-command (get-client (current-buffer))
                     "cl-lsp.interrupt"
                     (spinner-eval-id spinner))))

(defun register-eval-methods (workspace)
  (register-lsp-method workspace
                       "lisp/showEvalResult"
                       'show-eval-result)
  (register-lsp-method workspace
                       "lisp/startEval"
                       'start-eval))

(defun fold-one-line-message (message)
  (let ((pos (position #\newline message)))
    (if (not pos)
        message
        (format nil "~A..." (subseq message 0 pos)))))

(defun buffer-eval-result-overlays (buffer)
  (buffer-value buffer 'eval-result-overlays))

(defun (setf buffer-eval-result-overlays) (value buffer)
  (setf (buffer-value buffer 'eval-result-overlays) value))

(defun clear-eval-results (buffer)
  (mapc #'remove-eval-result-overlay
        (buffer-eval-result-overlays buffer)))

(defun spinner-eval-id (spinner)
  (lem/loading-spinner:spinner-value spinner 'eval-id))

(defun register-eval-spinner (buffer id spinner)
  (let ((hash-table
          (or (buffer-value buffer 'eval-loading-spinner)
              (setf (buffer-value buffer 'eval-loading-spinner)
                    (make-hash-table)))))
    (setf (gethash id hash-table) spinner)))

(defun get-eval-spinner (buffer id)
  (gethash id (buffer-value buffer 'eval-loading-spinner)))

(defun remove-eval-spinner (buffer id)
  (remhash id (buffer-value buffer 'eval-loading-spinner)))

(defun start-eval-spinner (start end id)
  (let ((spinner (lem/loading-spinner:start-loading-spinner :region :start start :end end)))
    (setf (lem/loading-spinner:spinner-value spinner 'eval-id) id)
    (register-eval-spinner (point-buffer start) id spinner)
    spinner))

(defun stop-eval-spinner (buffer id)
  (let ((spinner (get-eval-spinner buffer id)))
    (remove-eval-spinner buffer id)
    (lem/loading-spinner:stop-loading-spinner spinner)))

(defun start-eval (params)
  (with-accessors ((range start-eval-params-range)
                   (id start-eval-params-id)
                   (text-document-identifier start-eval-params-text-document))
      (convert-from-json params 'start-eval-params)
    (send-event (lambda ()
                  (let ((buffer (get-buffer-from-text-document-identifier
                                 text-document-identifier)))
                    (when buffer
                      (with-point ((start (buffer-point buffer))
                                   (end (buffer-point buffer)))
                        (lem-lsp-base/utils:destructuring-lsp-range start end range)
                        (remove-eval-result-overlay-between start end)
                        (start-eval-spinner start end id))))))))

(defun show-eval-result (params)
  (with-accessors ((type show-eval-result-params-type)
                   (id show-eval-result-params-id)
                   (text-document-identifier show-eval-result-params-text-document)
                   (message show-eval-result-params-message))
      (convert-from-json params 'show-eval-result-params)
    (send-event (lambda ()
                  (alexandria:when-let* ((buffer (get-buffer-from-text-document-identifier
                                                  text-document-identifier))
                                         (spinner (get-eval-spinner buffer id)))
                    (lem/loading-spinner:with-line-spinner-points (start end spinner)
                      (stop-eval-spinner buffer id)
                      (let ((popup-overlay
                              (make-overlay start
                                            end
                                            (message-type-to-attribute type)))
                            (background-overlay
                              (make-overlay start end (make-attribute :underline t))))
                        (overlay-put popup-overlay 'relation-overlay background-overlay)
                        (overlay-put popup-overlay :display-line-end t)
                        (overlay-put popup-overlay :display-line-end-offset 1)
                        (overlay-put popup-overlay :text (fold-one-line-message message))
                        (overlay-put popup-overlay 'whole-message message)
                        (push popup-overlay (buffer-eval-result-overlays buffer))
                        (add-hook (variable-value 'after-change-functions :buffer buffer)
                                  'remove-touch-overlay))))))))

(defun remove-touch-overlay (start end old-len)
  (declare (ignore old-len))
  (remove-eval-result-overlay-between start end))

(defun remove-eval-result-overlay (overlay)
  (delete-overlay overlay)
  (delete-overlay (overlay-get overlay 'relation-overlay))
  (alexandria:removef (buffer-eval-result-overlays (overlay-buffer overlay))
                      overlay))

(defun remove-eval-result-overlay-between (start end)
  (let ((buffer (point-buffer start)))
    (dolist (ov (buffer-eval-result-overlays buffer))
      (unless (or (point< end (overlay-start ov))
                  (point< (overlay-end ov) start))
        (delete-overlay ov)
        (delete-overlay (overlay-get ov 'relation-overlay))
        (alexandria:removef (buffer-eval-result-overlays buffer)
                            ov)))))

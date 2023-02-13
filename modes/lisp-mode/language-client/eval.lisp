(defpackage :lem-lisp-mode/language-client/eval
  (:use :cl
        :lem)
  (:import-from :lem-lsp-mode
                :register-lsp-method)
  (:import-from :lem-lsp-base/converter
                :convert-to-json
                :convert-from-json)
  (:shadowing-import-from :lem-language-client/request
                          :execute-command)
  (:export :register-eval-methods))
(in-package :lem-lisp-mode/language-client/eval)

(define-key lem-lisp-mode:*lisp-mode-keymap* "M-Return" 'lisp-language-client/eval-at-point)

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

(define-command lisp-language-client/eval-at-point () ()
  (execute-command (get-client (current-buffer))
                   "cl-lsp.eval-last-expression"
                   (convert-to-json
                    (lem-lsp-mode::make-text-document-position-params
                     (current-point)))))

(define-command lisp-language-client/clear-eval-results () ()
  (clear-eval-results (current-buffer)))

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

(defun register-eval-spinner (buffer id spinner)
  (let ((hash-table
          (or (buffer-value buffer 'eval-loading-spinner)
              (setf (buffer-value buffer 'eval-loading-spinner)
                    (make-hash-table)))))
    (setf (gethash id hash-table) spinner)))

(defun stop-eval-spinner (buffer id)
  (let ((spinner (gethash id (buffer-value buffer 'eval-loading-spinner))))
    (remhash id (buffer-value buffer 'eval-loading-spinner))
    (lem.loading-spinner:stop-loading-spinner spinner)))

(defun start-eval (params)
  (let* ((params (convert-from-json params 'lem-language-server::start-eval-params))
         (range (lem-language-server::start-eval-params-range params))
         (id (lem-language-server::start-eval-params-id params))
         (text-document-identifier (lem-language-server::start-eval-params-text-document params))
         (buffer (lem-lsp-mode::get-buffer-from-text-document-identifier text-document-identifier)))
    (send-event (lambda ()
                  (with-point ((start (buffer-point buffer))
                               (end (buffer-point buffer)))
                    (remove-eval-result-overlay-between start end)
                    (lem-lsp-base/utils:destructuring-lsp-range start end range)
                    (let ((spinner (lem.loading-spinner:start-loading-spinner :line :point end)))
                      (register-eval-spinner buffer id spinner)))))))

(defun show-eval-result (params)
  (let* ((params (convert-from-json params 'lem-language-server::show-eval-result-params))
         (type (lem-language-server::show-eval-result-params-type params))
         (range (lem-language-server::show-eval-result-params-range params))
         (id (lem-language-server::show-eval-result-params-id params))
         (text-document-identifier (lem-language-server::show-eval-result-params-text-document params))
         (message (lem-language-server::show-eval-result-params-message params)))
    (send-event (lambda ()
                  (let ((buffer (lem-lsp-mode::get-buffer-from-text-document-identifier text-document-identifier))
                        (attribute (message-type-to-attribute type))
                        (folding-message (fold-one-line-message message)))
                    (with-point ((start (buffer-point buffer))
                                 (end (buffer-point buffer)))
                      (lem-lsp-base/utils:destructuring-lsp-range start end range)
                      (stop-eval-spinner buffer id)
                      (let ((popup-overlay (make-overlay start end attribute))
                            (background-overlay
                              (make-overlay start end (make-attribute :underline-p t))))
                        (overlay-put popup-overlay 'relation-overlay background-overlay)
                        (overlay-put popup-overlay :display-line-end t)
                        (overlay-put popup-overlay :display-line-end-offset 1)
                        (overlay-put popup-overlay :text folding-message)
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

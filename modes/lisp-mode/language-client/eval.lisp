(defpackage :lem-lisp-mode/language-client/eval
  (:use :cl
        :lem)
  (:import-from :lem-lsp-base/converter
                :convert-to-json
                :convert-from-json)
  (:shadowing-import-from :lem-language-client/request
                          :execute-command))
(in-package :lem-lisp-mode/language-client/eval)

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

(defun show-eval-result (params)
  (let* ((params (convert-from-json params
                                    'lem-language-server::show-eval-result-params))
         (type (lem-language-server::show-eval-result-params-type params))
         (range (lem-language-server::show-eval-result-params-range params))
         (attribute (if (= type lsp:message-type-error)
                        (make-attribute :foreground "white"
                                        :background "dark red")
                        (make-attribute :foreground "cyan"
                                        :background "dark cyan")))
         (message (lem-language-server::show-eval-result-params-message params))
         (folding-message (fold-one-line-message message)))
    (send-event (lambda ()
                  (with-point ((start (current-point))
                               (end (current-point)))
                    (lem-lsp-base/utils:destructuring-lsp-range start end range)
                    (remove-overlay-between start end)
                    (let ((popup-overlay (make-overlay start end attribute))
                          (background-overlay
                            (make-overlay start end (make-attribute :underline-p t))))
                      (overlay-put popup-overlay 'relation-overlay background-overlay)
                      (overlay-put popup-overlay :display-line-end t)
                      (overlay-put popup-overlay :display-line-end-offset 1)
                      (overlay-put popup-overlay :text folding-message)
                      (overlay-put popup-overlay 'whole-message message)
                      (push popup-overlay (buffer-eval-result-overlays (current-buffer)))
                      (add-hook (variable-value 'after-change-functions :buffer (current-buffer))
                                'remove-touch-overlay)))))))

(defun remove-touch-overlay (start end old-len)
  (declare (ignore old-len))
  (remove-overlay-between start end))

(defun remove-eval-result-overlay (overlay)
  (delete-overlay overlay)
  (delete-overlay (overlay-get overlay 'relation-overlay))
  (alexandria:removef (buffer-eval-result-overlays (current-buffer))
                      overlay))

(defun remove-overlay-between (start end)
  (dolist (ov (buffer-eval-result-overlays (current-buffer)))
    (unless (or (point< end (overlay-start ov))
                (point< (overlay-end ov) start))
      (delete-overlay ov)
      (delete-overlay (overlay-get ov 'relation-overlay))
      (alexandria:removef (buffer-eval-result-overlays (current-buffer))
                          ov))))

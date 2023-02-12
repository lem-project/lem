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

(defun fold-one-line-message (message)
  (let ((pos (position #\newline message)))
    (if (not pos)
        message
        (format nil "~A..." (subseq message 0 pos)))))

(defun show-eval-result (params)
  (let* ((message (lem-language-server::show-eval-result-params-message
                   (convert-from-json params
                                      'lem-language-server::show-eval-result-params)))
         (folding-message (fold-one-line-message message)))
    (send-event (lambda ()
                  (with-point ((start (current-point))
                               (end (current-point)))
                    (form-offset start -1)
                    (let ((popup-overlay (make-overlay start end
                                                       (make-attribute :foreground "cyan"
                                                                       :background "dark cyan")))
                          (background-overlay
                            (make-overlay start end (make-attribute :underline-p t))))
                      (overlay-put popup-overlay 'relation-overlay background-overlay)
                      (overlay-put popup-overlay :display-line-end t)
                      (overlay-put popup-overlay :display-line-end-offset 1)
                      (overlay-put popup-overlay :text folding-message)
                      (overlay-put popup-overlay 'whole-message message)
                      (push popup-overlay (buffer-value (current-buffer) 'eval-result-overlays))
                      (add-hook (variable-value 'after-change-functions :buffer (current-buffer))
                                'remove-touch-overlay)))))))

(defun remove-touch-overlay (start end old-len)
  (declare (ignore old-len))
  (remove-overlay-between start end))

(defun remove-overlay-between (start end)
  (dolist (ov (buffer-value (current-buffer) 'eval-result-overlays))
    (unless (or (point< end (overlay-start ov))
                (point< (overlay-end ov) start))
      (delete-overlay ov)
      (delete-overlay (overlay-get ov 'relation-overlay))
      (alexandria:removef (buffer-value (current-buffer) 'eval-result-overlays)
                          ov))))

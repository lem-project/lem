(defpackage :lem-lisp-mode/language-client/eval
  (:use :cl))
(in-package :lem-lisp-mode/language-client/eval)

(define-command lisp-evaluate () ()
  (if (buffer-mark-p (current-buffer))
      (let ((start (region-beginning))
            (end (region-end)))
        (declare (ignore start end))
        #+(or)
        (lem-language-client/request::execute-command
         (lem-lsp-mode::workspace-client (lem-lsp-mode::buffer-workspace (current-buffer)))
         "cl-lsp.eval-range"
         ...TODO))
      (lem-language-client/request::execute-command
       (lem-lsp-mode::workspace-client (lem-lsp-mode::buffer-workspace (current-buffer)))
       "cl-lsp.eval-previous-form"
       (lem-lsp-mode::make-text-document-position-params (current-point)))))

(defun micros/eval-result (params)
  (let* ((params (convert-from-json params 'lem-language-server::eval-result-params))
         (text (lem-language-server::eval-result-params-message params)))
    (send-event (lambda ()
                  (display-popup-message text
                                         :style '(:gravity :cursor
                                                  :use-border nil
                                                  :background-color "dark cyan"))))))

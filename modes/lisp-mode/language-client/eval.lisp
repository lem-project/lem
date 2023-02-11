(defpackage :lem-lisp-mode/language-client/eval
  (:use :cl
        :lem)
  (:import-from :lem-lsp-base/converter
                :convert-to-json)
  (:shadowing-import-from :lem-language-client/request
                          :execute-command))
(in-package :lem-lisp-mode/language-client/eval)

(defun get-client (buffer)
  (lem-lsp-mode::workspace-client (lem-lsp-mode::buffer-workspace buffer)))

(define-command lisp-language-client/eval-at-point () ()
  (execute-command (get-client (current-buffer))
                   "cl-lsp.micros.eval-last-expression"
                   (convert-to-json
                    (lem-lsp-mode::make-text-document-position-params
                     (current-point)))))

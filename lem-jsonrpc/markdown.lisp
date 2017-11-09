(defpackage :lem-jsonrpc.markdown
  (:use :cl :lem))
(in-package :lem-jsonrpc.markdown)

(define-major-mode markdown-mode ()
    (:name "markdown"
     :keymap *markdown-mode-keymap*)
  (markdown-update)
  (add-hook (variable-value 'after-change-functions
                            :buffer (current-buffer))
            (lambda (&rest args)
              (declare (ignore args))
              (markdown-update))))

(defun send-markdown-text (text)
  (lem-jsonrpc:notify "markdown-update"
                      (alexandria:plist-hash-table `("text" ,text)
                                                   :test #'equal)))

(define-command markdown-update () ()
  (let ((buffer (current-buffer)))
    (send-markdown-text
     (points-to-string (buffer-start-point buffer)
                       (buffer-end-point buffer)))))

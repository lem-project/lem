(uiop:define-package :lem-intelligence/claude-code
  (:use :cl
        :lem)
  (:local-nicknames (:claude-code :lem-intelligence/lib/claude-code)))
(in-package :lem-intelligence/claude-code)

(define-major-mode claude-code-mode nil
    (:name "Claude Code"
     :keymap *claude-code-keymap*
     :mode-hook *claude-code-mode-hook*)
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer (current-buffer))
        'set-prompt
        (variable-value 'lem/listener-mode:listener-check-input-function :buffer (current-buffer))
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function :buffer (current-buffer))
        'execute-query)
  (lem/listener-mode:start-listener-mode))

(defun handle-kill-buffer (buffer)
  (declare (ignore buffer))
  ;; TODO
  )

(defun set-prompt (point)
  (insert-string point "Claude> "))

(defun execute-query (point prompt)
  (let ((buffer (point-buffer point)))
    (setf (buffer-value buffer 'session)
          (claude-code:query prompt
                             :callback (lambda (response)
                                         (send-event (lambda ()
                                                       (with-point ((point (buffer-point buffer) :left-inserting))
                                                         (buffer-end point)
                                                         (insert-string point 
                                                                        (with-output-to-string (out)
                                                                          (yason:encode response out))))
                                                       (when (equal "result" (gethash "type" response))
                                                         (lem/listener-mode:refresh-prompt buffer))
                                                       (when (get-buffer-windows buffer)
                                                         (redraw-display)))))))))

(define-command claude-code () ()
  (let ((buffer (make-buffer "*Claude Code*")))
    (add-hook (variable-value 'kill-buffer-hook :buffer buffer) 'handle-kill-buffer)
    (lem/listener-mode:listener-start buffer 'claude-code-mode)))

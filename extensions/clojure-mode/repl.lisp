(defpackage :lem-clojure-mode/repl
  (:use :cl :lem :lem-clojure-mode)
  (:import-from :lem-clojure-mode/nrepl-client
                :*nrepl-connection*
                :nrepl-connection-session
                :nrepl-connect
                :nrepl-disconnect
                :nrepl-connected-p
                :check-nrepl-connection
                :nrepl-eval
                :nrepl-interrupt
                :nrepl-response-value
                :nrepl-response-out
                :nrepl-response-err
                :nrepl-response-exception)
  (:export :clojure-repl-mode
           :*clojure-repl-mode-hook*
           :*clojure-repl-mode-keymap*
           :start-clojure-repl
           :clojure-switch-to-repl))

(in-package :lem-clojure-mode/repl)

;;;; REPL Attributes

(define-attribute repl-prompt-attribute
  (t :foreground :base0D :bold t))

(define-attribute repl-result-attribute
  (t :foreground :base0B))

(define-attribute repl-error-attribute
  (t :foreground :base08))

(define-attribute repl-output-attribute
  (t :foreground :base05))

;;;; REPL Mode

(define-major-mode clojure-repl-mode clojure-mode
    (:name "Clojure REPL"
     :keymap *clojure-repl-mode-keymap*
     :mode-hook *clojure-repl-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (lem/listener-mode:start-listener-mode
   (merge-pathnames "history/clojure-repl" (lem-home)))
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function)
        'repl-set-prompt)
  (setf (variable-value 'lem/listener-mode:listener-check-input-function)
        'repl-check-input)
  (setf (variable-value 'lem/listener-mode:listener-execute-function)
        'repl-execute))

(define-key *clojure-repl-mode-keymap* "C-c C-c" 'clojure-repl-interrupt)
(define-key *clojure-repl-mode-keymap* "C-c C-z" 'clojure-switch-to-repl)

;;;; REPL Buffer Management

(defvar *repl-buffer-name* "*clojure-repl*")

(defun repl-buffer ()
  "Get the REPL buffer."
  (get-buffer *repl-buffer-name*))

(defun ensure-repl-buffer ()
  "Ensure the REPL buffer exists and return it."
  (or (repl-buffer)
      (make-buffer *repl-buffer-name*)))

(defun current-namespace ()
  "Get the current namespace for the REPL."
  (or (buffer-value (repl-buffer) 'clojure-repl-namespace)
      "user"))

(defun set-current-namespace (ns)
  "Set the current namespace for the REPL."
  (setf (buffer-value (repl-buffer) 'clojure-repl-namespace) ns))

;;;; REPL Listener Functions

(defun repl-set-prompt (point)
  "Set the REPL prompt at POINT."
  (insert-string point
                 (format nil "~A=> " (current-namespace))
                 :attribute 'repl-prompt-attribute)
  point)

(defun repl-check-input (point)
  "Check if the input at POINT is complete."
  (with-point ((start (lem/listener-mode:input-start-point (repl-buffer))))
    (when (point<= start point)
      (let ((state (parse-partial-sexp start point)))
        (and (not (pps-state-string-or-comment-p state))
             (>= 0 (pps-state-paren-depth state)))))))

(defun repl-execute (point string)
  "Execute STRING in the nREPL server."
  (declare (ignore point))
  (unless (nrepl-connected-p)
    (editor-error "Not connected to nREPL. Use M-x clojure-connect"))
  (let ((buffer (repl-buffer)))
    (nrepl-eval string
                :ns (current-namespace)
                :callback (lambda (response)
                            (repl-handle-response buffer response)))))

(defun repl-handle-response (buffer response)
  "Handle a response from the nREPL server."
  (with-buffer-read-only buffer nil
    (with-point ((point (buffer-end-point buffer)))
      ;; Handle stdout
      (alexandria:when-let ((out (gethash "out" response)))
        (insert-string point out :attribute 'repl-output-attribute))
      ;; Handle stderr
      (alexandria:when-let ((err (gethash "err" response)))
        (insert-string point err :attribute 'repl-error-attribute))
      ;; Handle value
      (alexandria:when-let ((value (gethash "value" response)))
        (insert-string point value :attribute 'repl-result-attribute)
        (insert-character point #\newline))
      ;; Handle namespace change
      (alexandria:when-let ((ns (gethash "ns" response)))
        (set-current-namespace ns))
      ;; Handle exception
      (alexandria:when-let ((ex (gethash "ex" response)))
        (insert-string point (format nil "Exception: ~A~%" ex)
                       :attribute 'repl-error-attribute))
      ;; Handle completion (status contains "done")
      (when (member "done" (gethash "status" response) :test #'equal)
        (lem/listener-mode:refresh-prompt buffer)))))

;;;; REPL Commands

(define-command clojure-repl-interrupt () ()
  "Interrupt the current nREPL evaluation."
  (check-nrepl-connection)
  (nrepl-interrupt *nrepl-connection*)
  (message "Evaluation interrupted"))

(define-command start-clojure-repl (&optional (use-this-window nil)) (:universal-nil)
  "Start the Clojure REPL."
  (unless (nrepl-connected-p)
    (editor-error "Not connected to nREPL. Use M-x clojure-connect first"))
  (let ((buffer (ensure-repl-buffer)))
    (lem/listener-mode:listener-start
     buffer
     'clojure-repl-mode
     :switch-to-buffer-function
     (if use-this-window
         #'switch-to-buffer
         (lambda (buffer)
           (switch-to-window (pop-to-buffer buffer)))))))

(define-command clojure-switch-to-repl () ()
  "Switch to the Clojure REPL buffer."
  (let ((buffer (repl-buffer)))
    (if buffer
        (switch-to-window (pop-to-buffer buffer))
        (start-clojure-repl))))

;;;; Connection Commands

(define-command clojure-connect (host port) ((:string "Host: ")
                                              (:number "Port: "))
  "Connect to an nREPL server."
  (nrepl-connect host port)
  (start-clojure-repl))

(define-command clojure-disconnect () ()
  "Disconnect from the nREPL server."
  (nrepl-disconnect))

(define-command clojure-connect-to-localhost (port) ((:number "Port: "))
  "Connect to an nREPL server on localhost."
  (clojure-connect "127.0.0.1" port))

;;;; Keybindings for clojure-mode

(define-key lem-clojure-mode:*clojure-mode-keymap* "C-c C-z" 'clojure-switch-to-repl)

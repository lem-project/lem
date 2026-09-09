(defpackage :lem-clojure-mode/commands
  (:use :cl :lem :lem-clojure-mode)
  (:import-from :lem-clojure-mode/nrepl-client
                :*nrepl-connection*
                :nrepl-connected-p
                :check-nrepl-connection
                :nrepl-eval
                :nrepl-eval-sync
                :nrepl-response-value
                :nrepl-response-out
                :nrepl-response-err
                :nrepl-response-exception)
  (:export :clojure-eval-last-sexp
           :clojure-eval-defun
           :clojure-eval-region
           :clojure-eval-buffer
           :clojure-load-file
           :clojure-macroexpand-1
           :clojure-macroexpand-all
           :clojure-describe-symbol
           :clojure-describe-symbol-at-point
           :clojure-set-namespace))

(in-package :lem-clojure-mode/commands)

;;;; Evaluation Highlighting

(define-attribute eval-region-attribute
  (t :background :base02))

(defvar *eval-overlay* nil)

(defun highlight-eval-region (start end)
  "Highlight the region being evaluated."
  (when *eval-overlay*
    (delete-overlay *eval-overlay*))
  (setf *eval-overlay*
        (make-overlay start end 'eval-region-attribute)))

(defun clear-eval-highlight ()
  "Clear the evaluation highlight."
  (when *eval-overlay*
    (delete-overlay *eval-overlay*)
    (setf *eval-overlay* nil)))

;;;; Evaluation Result Display

(defun display-eval-result (value &optional error-p)
  "Display evaluation result in the minibuffer."
  (if error-p
      (message "Error: ~A" value)
      (message "=> ~A" value)))

(defun handle-eval-responses (responses)
  "Handle responses from an evaluation."
  (clear-eval-highlight)
  (let ((value (nrepl-response-value responses))
        (err (nrepl-response-err responses))
        (ex (nrepl-response-exception responses))
        (out (nrepl-response-out responses)))
    (when (and out (plusp (length out)))
      (message "~A" out))
    (cond
      (ex (display-eval-result ex t))
      ((plusp (length err)) (display-eval-result err t))
      (value (display-eval-result value))
      (t (message "Evaluation complete")))))

;;;; Form Extraction Utilities

(defun form-string-at-point (point)
  "Get the form at POINT as a string."
  (with-point ((start point)
               (end point))
    (when (and (form-offset start -1)
               (form-offset end 1))
      (points-to-string start end))))

(defun top-level-form-at-point (point)
  "Get the top-level form at POINT."
  (with-point ((start point)
               (end point))
    ;; Move to beginning of top-level form
    (loop :while (backward-up-list start t))
    ;; Move to end
    (form-offset (move-point end start) 1)
    (values start end (points-to-string start end))))

(defun last-sexp-at-point (point)
  "Get the sexp before POINT."
  (with-point ((start point)
               (end point))
    (when (form-offset start -1)
      (values start end (points-to-string start end)))))

;;;; Evaluation Commands

(define-command clojure-eval-last-sexp () ()
  "Evaluate the sexp before the cursor."
  (check-nrepl-connection)
  (multiple-value-bind (start end code)
      (last-sexp-at-point (current-point))
    (when code
      (highlight-eval-region start end)
      (let ((ns (clojure-current-namespace)))
        (let ((responses (nrepl-eval-sync code :ns ns)))
          (handle-eval-responses responses))))))

(define-command clojure-eval-defun () ()
  "Evaluate the top-level form at point."
  (check-nrepl-connection)
  (multiple-value-bind (start end code)
      (top-level-form-at-point (current-point))
    (when code
      (highlight-eval-region start end)
      (let ((ns (clojure-current-namespace)))
        (let ((responses (nrepl-eval-sync code :ns ns)))
          (handle-eval-responses responses))))))

(define-command clojure-eval-region (start end) (:region)
  "Evaluate the selected region."
  (check-nrepl-connection)
  (let ((code (points-to-string start end))
        (ns (clojure-current-namespace)))
    (highlight-eval-region start end)
    (let ((responses (nrepl-eval-sync code :ns ns)))
      (handle-eval-responses responses))))

(define-command clojure-eval-buffer () ()
  "Evaluate the entire buffer."
  (check-nrepl-connection)
  (let ((code (buffer-text (current-buffer)))
        (ns (clojure-current-namespace)))
    (let ((responses (nrepl-eval-sync code :ns ns :timeout 60)))
      (handle-eval-responses responses))))

(define-command clojure-load-file () ()
  "Load the current file into the nREPL server."
  (check-nrepl-connection)
  (let ((filename (buffer-filename (current-buffer))))
    (unless filename
      (editor-error "Buffer has no associated file"))
    (save-current-buffer)
    (let* ((code (format nil "(clojure.core/load-file ~S)" filename))
           (responses (nrepl-eval-sync code :timeout 60)))
      (handle-eval-responses responses))))

;;;; Macro Expansion

(define-command clojure-macroexpand-1 () ()
  "Macroexpand the form at point once."
  (check-nrepl-connection)
  (let ((form (form-string-at-point (current-point))))
    (when form
      (let* ((code (format nil "(clojure.core/macroexpand-1 '~A)" form))
             (ns (clojure-current-namespace))
             (responses (nrepl-eval-sync code :ns ns)))
        (let ((result (nrepl-response-value responses)))
          (if result
              (show-macroexpand-result result)
              (handle-eval-responses responses)))))))

(define-command clojure-macroexpand-all () ()
  "Fully macroexpand the form at point."
  (check-nrepl-connection)
  (let ((form (form-string-at-point (current-point))))
    (when form
      (let* ((code (format nil "(clojure.walk/macroexpand-all '~A)" form))
             (ns (clojure-current-namespace))
             (responses (nrepl-eval-sync code :ns ns)))
        (let ((result (nrepl-response-value responses)))
          (if result
              (show-macroexpand-result result)
              (handle-eval-responses responses)))))))

(defun show-macroexpand-result (result)
  "Display macroexpand result in a popup buffer."
  (let ((buffer (make-buffer "*clojure-macroexpand*")))
    (change-buffer-mode buffer 'clojure-mode)
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (insert-string (buffer-point buffer) result)
      (indent-points (buffer-start-point buffer)
                     (buffer-end-point buffer)))
    (with-pop-up-typeout-window (s buffer)
      (declare (ignore s)))))

;;;; Documentation

(define-command clojure-describe-symbol (symbol-name)
    ((:string "Describe symbol: "))
  "Show documentation for SYMBOL-NAME."
  (check-nrepl-connection)
  (let* ((code (format nil "(clojure.repl/doc ~A)" symbol-name))
         (ns (clojure-current-namespace))
         (responses (nrepl-eval-sync code :ns ns)))
    (let ((out (nrepl-response-out responses)))
      (if (and out (plusp (length out)))
          (let ((buffer (make-buffer "*clojure-doc*")))
            (with-buffer-read-only buffer nil
              (erase-buffer buffer)
              (insert-string (buffer-point buffer) out))
            (with-pop-up-typeout-window (s buffer)
              (declare (ignore s))))
          (message "No documentation found for ~A" symbol-name)))))

(define-command clojure-describe-symbol-at-point () ()
  "Show documentation for the symbol at point."
  (let ((symbol (symbol-string-at-point (current-point))))
    (if symbol
        (clojure-describe-symbol symbol)
        (call-command 'clojure-describe-symbol 1))))

;;;; Namespace

(define-command clojure-set-namespace (ns) ((:string "Namespace: "))
  "Set the current namespace."
  (check-nrepl-connection)
  (let* ((code (format nil "(in-ns '~A)" ns))
         (responses (nrepl-eval-sync code)))
    (let ((err (nrepl-response-err responses)))
      (if (and err (plusp (length err)))
          (message "Error: ~A" err)
          (message "Switched to namespace: ~A" ns)))))

(define-command clojure-set-namespace-from-buffer () ()
  "Set the namespace from the current buffer's ns form."
  (let ((ns (clojure-current-namespace)))
    (if ns
        (clojure-set-namespace ns)
        (message "No namespace found in buffer"))))

;;;; Add keybindings to clojure-mode

(define-keys lem-clojure-mode:*clojure-mode-keymap*
  ("C-x C-e"   'clojure-eval-last-sexp)
  ("C-c C-e"   'clojure-eval-last-sexp)
  ("C-c C-c"   'clojure-eval-defun)
  ("C-c C-r"   'clojure-eval-region)
  ("C-c C-k"   'clojure-load-file)
  ("C-c C-l"   'clojure-load-file)
  ("C-c Return" 'clojure-macroexpand-1)
  ("C-c M-m"   'clojure-macroexpand-all)
  ("C-c C-d d" 'clojure-describe-symbol-at-point)
  ("C-c C-d C-d" 'clojure-describe-symbol-at-point)
  ("C-c M-n"   'clojure-set-namespace-from-buffer))

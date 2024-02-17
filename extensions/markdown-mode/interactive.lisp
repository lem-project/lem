(defpackage :lem-markdown-mode/interactive
  (:use :cl :lem)
  (:import-from #:alexandria :if-let :when-let :curry))
(in-package :lem-markdown-mode/interactive)

(defvar *block-evaluators* (make-hash-table :test #'equal)
  "Dispatch table for block evaluators per language.")

(defmacro register-block-evaluator (language (string callback) &body body)
  "Convenience macro to register block evaluators, wraps setf."
  `(setf (gethash ,language *block-evaluators*)
         (lambda (,string ,callback)
           ,@body)))

(defmacro with-constant-position (&body body)
  "This allows you to move around the buffer point without worry."
  `(let* ((buf (current-buffer))
          (p (copy-point (buffer-point buf))))
     (prog1 ,@body
       (move-point (buffer-point buf) p))))

(defmacro when-markdown-mode (&body body)
  "Ensure the major mode is markdown-mode and alert the user if not."
  `(if (eq 'lem-markdown-mode:markdown-mode
           (buffer-major-mode (current-buffer)))
       (progn ,@body)
       (message "Not in markdown mode.")))

(defun pop-up-buffer (name text)
  "Create a popup with name containing text."
  (let ((buffer (make-buffer name)))
    (erase-buffer buffer)
    (with-buffer-read-only buffer nil
      (insert-string (buffer-point buffer) text)
      (with-pop-up-typeout-window (s buffer)
        (declare (ignore s))))))

(defun block-fence-lang (fence)
  "Get language from a block fence string."
  (let ((str (coerce (cdddr (coerce fence 'list)) 'string)))
    (unless (str:emptyp str)
      str)))

(defun block-at-point (point)
  "Ugly hack to get the string in a code block at point."
  (search-backward-regexp point "```")
  (when-let ((lang (block-fence-lang (str:trim (line-string (current-point))))))
    (search-forward (current-point) (format nil "~%"))
    (let ((start (copy-point (current-point))))
      (search-forward-regexp (current-point) "```")
      (search-backward (current-point) (format nil "~%"))
      (let ((end (current-point)))
        (values lang (points-to-string start end))))))

(define-command kill-block-eval-result () ()
  "Searches for a result block below the current code block, and kills it."
  (when-markdown-mode
    (with-constant-position
      (when (block-at-point (current-point))
        (search-forward-regexp (current-point) "```")
        (line-offset (current-point) 2)
        (when (equal "result" (block-fence-lang (line-string (current-point))))
          (loop :while (not (equal "```" (line-string (current-point))))
                :do (kill-whole-line)
                :do (line-offset (current-point) 1))
          (kill-whole-line)
          (kill-whole-line))))))

(defun pop-up-eval-result (result)
  "Display results of evaluation in a popup buffer."
  (pop-up-buffer "*literate*" (format nil "~a" result)))

(defun insert-eval-result (point result)
  "Insert results of evaluation in a code block."
  (with-constant-position
    (kill-block-eval-result)
    (block-at-point point)
    (search-forward-regexp point "```")
    (insert-string point (format nil "~%~%```result~%~a~%```" result)))
  (redraw-display))

(defun eval-block-internal (handler)
  "Evaluate code block and apply handler to result."
  (with-constant-position
    (multiple-value-bind (lang block) (block-at-point (current-point))
      (when lang
        (if-let ((evaluator (gethash lang *block-evaluators*)))
          (funcall evaluator block handler)
          (message "No evaluator registered for ~a." lang))))))

(define-command eval-block () ()
  "Evaluate current markdown code block and display results in popup."
  (when-markdown-mode
    (eval-block-internal #'pop-up-eval-result)))

(define-command eval-block-and-insert () ()
  "Evaluate current markdown code block and display results in popup."
  (when-markdown-mode
    (eval-block-internal (curry #'insert-eval-result (current-point)))))

;;
;; Default evaluators:
;;

(register-block-evaluator "bash" (string callback)
  "Register evaluator for Bash blocks."
  (bt:make-thread
   (lambda ()
     (funcall callback (uiop:run-program string :output :string)))))

(register-block-evaluator "lisp" (string callback)
  "Register evaluator for Lisp blocks."
  (lem-lisp-mode:check-connection)
  (lem-lisp-mode:lisp-eval-async
   (read-from-string
    (format nil "(progn ~a)" string))
   (lambda (result)
     (funcall callback result))))

;;
;; Keybindings
;;

(define-keys lem-markdown-mode::*markdown-mode-keymap*
  ("C-c C-e" 'eval-block)
  ("C-c C-c" 'eval-block-and-insert)
  ("C-c C-d" 'kill-block-eval-result))

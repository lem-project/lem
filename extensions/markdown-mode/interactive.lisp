(defpackage :lem-markdown-mode/interactive
  (:use :cl :lem)
  (:import-from #:alexandria :if-let :when-let :curry)
  (:export :register-block-evaluator))
(in-package :lem-markdown-mode/interactive)

(define-keys lem-markdown-mode::*markdown-mode-keymap*
  ("C-c C-e" 'eval-block)
  ("C-c C-c" 'eval-block-and-insert)
  ("C-c C-d" 'kill-block-eval-result))

(defvar *block-evaluators* (make-hash-table :test #'equal)
  "Dispatch table for block evaluators per language.")

(defmacro register-block-evaluator (language (string callback) &body body)
  "Convenience macro to register block evaluators, wraps setf."
  `(setf (gethash ,language *block-evaluators*)
         (lambda (,string ,callback)
           ,@body)))

(defmacro with-constant-position ((point) &body body)
  "This allows you to move around the point without worry."
  `(let ((tmp (copy-point ,point)))
     (prog1 (progn ,@body)
       (move-point ,point tmp))))

(defmacro when-markdown-mode (&body body)
  "Ensure the major mode is markdown-mode and alert the user if not."
  `(if (eq 'lem-markdown-mode:markdown-mode
           (buffer-major-mode (current-buffer)))
       (progn ,@body)
       (message "Not in markdown mode.")))

(defun pop-up-buffer (name text)
  "Create a pop-up with name containing text."
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
  "Get the language of a code block and its contents."
  (search-backward-regexp point "```")
  (when-let ((lang (block-fence-lang (str:trim (line-string point)))))
    (search-forward point (format nil "~%"))
    (let ((start (copy-point point)))
      (search-forward-regexp point "```")
      (search-backward point (format nil "~%"))
      (let ((end point))
        (values lang (points-to-string start end))))))

(define-command kill-block-eval-result (&optional (point (current-point))) ()
  "Searches for a result block below the current code block, and kills it."
  (when-markdown-mode
    (with-constant-position (point)
      (when (block-at-point point)
        (search-forward-regexp point "```")
        (line-offset point 2)
        (when (equal "result" (block-fence-lang (line-string point)))
          (loop :while (not (equal "```" (line-string point)))
                :do (kill-whole-line)
                :do (line-offset point 1))
          (kill-whole-line)
          (kill-whole-line))))))

(defun pop-up-eval-result (point result)
  "Display results of evaluation in a pop-up buffer."
  (declare (ignore point))
  (pop-up-buffer "*result*" (format nil "~a" result)))

(defun insert-eval-result (point result)
  "Insert results of evaluation in a code block."
  (block-at-point point)
  (search-forward-regexp point "```")
  (insert-string point (format nil "~%~%```result~%~a~%```" result))
  (message "Block evaluated."))

(defun eval-block-internal (point handler)
  "Evaluate code block and apply handler to result."
  (multiple-value-bind (lang block) (block-at-point point)
    (when lang
      (if-let ((evaluator (gethash lang *block-evaluators*)))
        (funcall evaluator block (curry handler point))
        (message "No evaluator registered for ~a." lang)))))

(define-command eval-block () ()
  "Evaluate current markdown code block and display results in pop-up."
  (when-markdown-mode
    (eval-block-internal (copy-point (current-point)) #'pop-up-eval-result)))

(define-command eval-block-and-insert () ()
  "Evaluate current markdown code block and display results in pop-up."
  (when-markdown-mode
    (kill-block-eval-result)
    (eval-block-internal (copy-point (current-point)) #'insert-eval-result)))

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
   `(eval (read-from-string ,(format nil "(progn ~a)" string)))
   callback))

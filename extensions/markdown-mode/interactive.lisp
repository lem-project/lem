(defpackage :lem-markdown-mode/interactive
  (:use :cl :lem)
  (:import-from #:alexandria :if-let :when-let))
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

(defun pop-up-eval-result (result)
  "Display results of evaluation."
  (pop-up-buffer "*literate*" (format nil "~a" result)))

(defun insert-eval-result (result)
  (search-forward-regexp (current-point) "```")
  (insert-string (current-point)
                 (format nil "~%~%```result~%~a~%```" result))
  (redraw-display))

(defun eval-block-internal (handler)
  (with-constant-position
    (multiple-value-bind (lang block) (block-at-point (current-point))
      (when lang
        (if-let ((evaluator (gethash lang *block-evaluators*)))
          (funcall evaluator block handler)
          (message "No evaluator registered for ~a" lang))))))

(define-command eval-block () ()
  "Evaluate current markdown code block and display results in popup."
  (eval-block-internal #'pop-up-eval-result))

(define-command eval-block-and-insert () ()
  "Evaluate current markdown code block and display results in popup."
  (eval-block-internal #'insert-eval-result))

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

(define-keys lem-markdown-mode::*markdown-mode-keymap*
  ("C-c C-c" 'eval-block)
  ("C-c C-e" 'eval-block-and-insert))

(in-package :lem-base)

(export '(syntax-string-attribute
          syntax-comment-attribute
          syntax-keyword-attribute
          syntax-constant-attribute
          syntax-function-name-attribute
          syntax-variable-attribute
          syntax-type-attribute
          *global-syntax-highlight*
          before-syntax-scan-hook
          after-syntax-scan-hook
          enable-syntax-highlight
          enable-syntax-highlight-p
          syntax-scan-region))

(define-editor-variable before-syntax-scan-hook nil)
(define-editor-variable after-syntax-scan-hook nil)
(define-editor-variable enable-syntax-highlight nil)

(defvar *global-syntax-highlight* t)

(defun enable-syntax-highlight-p (buffer)
  (and *global-syntax-highlight*
       (variable-value 'enable-syntax-highlight :buffer buffer)))

(defun current-syntax-parser ()
  (syntax-table-parser (current-syntax)))

(defclass syntax-parser ()
  ())

(defgeneric %syntax-scan-region (parser start end))

(defvar *recursive-syntax-scan* nil)

(defun syntax-scan-region (start end)
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (unless *recursive-syntax-scan*
    (let ((*recursive-syntax-scan* t))
      (run-hooks (variable-value 'before-syntax-scan-hook :global t) start end)
      (run-hooks (variable-value 'before-syntax-scan-hook :buffer start) start end)
      (without-interrupts
        (let ((buffer (point-buffer start)))
          (when (enable-syntax-highlight-p buffer)
            (let ((*current-syntax*
                    (buffer-syntax-table buffer)))
              (with-point ((start start)
                           (end end))
                (line-start start)
                (line-end end)
                (%syntax-scan-region (syntax-table-parser *current-syntax*) start end))))))
      (run-hooks (variable-value 'after-syntax-scan-hook :global t) start end)
      (run-hooks (variable-value 'after-syntax-scan-hook :buffer start) start end))))

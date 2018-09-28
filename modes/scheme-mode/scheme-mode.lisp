(in-package :lem-scheme-mode)

(define-major-mode scheme-mode language-mode
    (:name "scheme"
     :keymap *scheme-mode-keymap*
     :syntax-table lem-scheme-syntax:*syntax-table*
     :mode-hook *scheme-mode-hook*)
  (setf (variable-value 'beginning-of-defun-function) 'scheme-beginning-of-defun)
  (setf (variable-value 'end-of-defun-function) 'scheme-end-of-defun)
  (setf (variable-value 'indent-tabs-mode) nil)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'calc-indent)
  (setf (variable-value 'line-comment) ";")
  (setf (variable-value 'insertion-line-comment) ";; ")
  (set-syntax-parser lem-scheme-syntax:*syntax-table* (make-tmlanguage-scheme)))

(define-key *scheme-mode-keymap* "C-M-q" 'scheme-indent-sexp)
(define-key *scheme-mode-keymap* "C-c C-e" 'scheme-eval-last-expression)
(define-key *scheme-mode-keymap* "C-c C-r" 'scheme-eval-region)

(defparameter *scheme-run-command* "gosh -i")

(defun calc-indent (point)
  (lem-scheme-syntax:calc-indent point))

(defun scheme-beginning-of-defun (point n)
  (lem-scheme-syntax:beginning-of-defun point (- n)))

(defun scheme-end-of-defun (point n)
  (if (minusp n)
      (scheme-beginning-of-defun point (- n))
      (dotimes (_ n)
        (with-point ((p point))
          (cond ((and (lem-scheme-syntax:beginning-of-defun p -1)
                      (point<= p point)
                      (or (form-offset p 1)
                          (progn
                            (move-point point p)
                            (return)))
                      (point< point p))
                 (move-point point p)
                 (skip-whitespace-forward point t)
                 (when (end-line-p point)
                   (character-offset point 1)))
                (t
                 (form-offset point 1)
                 (skip-whitespace-forward point t)
                 (when (end-line-p point)
                   (character-offset point 1))))))))

(define-command scheme-indent-sexp () ()
  (with-point ((end (current-point) :right-inserting))
    (when (form-offset end 1)
      (indent-region (current-point) end))))

(defvar *scheme-process* nil)

(defun scheme-output-callback (string)
  (let ((buffer (lem-process::process-buffer *scheme-process*)))
    (with-current-window (pop-to-buffer buffer)
      (buffer-end (buffer-point buffer)))
    (redraw-display)))

(defun scheme-run-process ()
  (unless *scheme-process*
    (setf *scheme-process* (lem-process:run-process
                            *scheme-run-command* '()
                            :name "*scheme-output*"
                            :output-callback #'scheme-output-callback))
    (lem-base::add-buffer (lem-process::process-buffer *scheme-process*))))

(defun scheme-send-input (string)
  (lem-process::process-send-input *scheme-process* string))

(define-command scheme-eval-last-expression (p) ("P")
  (with-point ((start (current-point))
               (end   (current-point)))
    (form-offset start -1)
    (scheme-run-process)
    (scheme-send-input (format nil "~A~%" (points-to-string start end)))))

(define-command scheme-eval-region (start end) ("r")
  (scheme-run-process)
  (scheme-send-input (format nil "~A~%" (points-to-string start end))))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *scheme-process*
              (lem-process:delete-process *scheme-process*))))

(define-command scheme-scratch () ()
  (let ((buffer (make-buffer "*tmp*")))
    (change-buffer-mode buffer 'scheme-mode)
    (switch-to-buffer buffer)))

(pushnew (cons "\\.scm$" 'scheme-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "\\.sld$" 'scheme-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "\\.rkt$" 'scheme-mode) *auto-mode-alist* :test #'equal)

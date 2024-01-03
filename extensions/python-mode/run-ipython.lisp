(defpackage :lem-python-mode.run-ipython
  (:use :cl :lem :lem-python-mode)
  (:export :*ipython-run-command*
           :run-ipython))

(in-package :lem-python-mode.run-ipython)

(defvar *ipython-flags* '("--simple-prompt" "--colors=NoColor"))

(defvar *ipython-run-command* "ipython3")

(defvar *process* nil)

(define-major-mode run-ipython-mode ()
    (:name "Python"
     :keymap *run-ipython-mode-keymap*
     :syntax-table lem-python-mode::*python-syntax-table*)
  (reset-listener-variables (current-buffer))
  (lem/listener-mode:start-listener-mode))

(define-key lem-python-mode::*python-mode-keymap* "C-c C-r" 'ipython-eval-region)

(defun reset-listener-variables (buffer)
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer buffer)
        #'identity
        (variable-value 'lem/listener-mode:listener-check-input-function :buffer buffer)
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function :buffer buffer)
        'execute-input))

(defun execute-input (point string)
  (declare (ignore point))
  (unless (alive-process-p)
    (editor-error "IPython process doesn't exist."))
  (ipm/impl:process-send-line string *process*))

(defun alive-process-p ()
  (and *process*
       (ipm/impl:process-alive-p *process*)))

(defun repl-buffer-exists-p ()
  (get-buffer "*ipython*"))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer "*ipython*")))
    (unless (eq (buffer-major-mode buffer) 'run-ipython-mode)
      (change-buffer-mode buffer 'run-ipython-mode))
    buffer))

(defun output-callback (string)
  (when (not (str:emptyp string))
    (let* ((already-exists (repl-buffer-exists-p))
           (buffer (get-repl-buffer))
           (p (buffer-point buffer)))
      (buffer-end p)

      (insert-string p string)
      (when (ppcre:scan "^(In \\[[0-9]+\\]: )|(   ...: )" (line-string p))
        (lem/listener-mode:refresh-prompt buffer nil))
      (unless already-exists
        (switch-to-window (pop-to-buffer buffer)))
      (alexandria:when-let (window (first (get-buffer-windows buffer)))
        (with-current-window window
          (buffer-end p)
          (window-see window)))
      (redraw-display))))

(defun run-ipython-internal ()
  (unless (and (alive-process-p) (repl-buffer-exists-p))
    (when *process*
      (lem/run-process:destroy-process *process*))
    (setf *process*
          (lem/run-process:run-process
           (str:join #\Space (concatenate 'List
                                          (list *ipython-run-command*)
                                          *ipython-flags*))
           :name "run-ipython"
           :output-callback 'output-callback))))

(define-command ipython-eval-region (start end) ("r")
  (unless (ipm/impl:process-alive-p *process* )
    (editor-error "IPython process doesn't exist."))
  (let* ((buffer (get-repl-buffer))
         (p (buffer-point buffer)))
    (buffer-end p)
    (insert-string p (format nil "~%"))
    (ipm/impl:process-send-line (points-to-string start end) *process*)
    (lem/listener-mode:refresh-prompt buffer nil)))

(define-command run-ipython () ()
  (run-ipython-internal))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *process*
              (ignore-errors (lem/run-process:destroy-process *process*)))))

(defpackage :lem.prompt-window
  (:use :cl :lem)
  (:import-from :alexandria
                :when-let))
(in-package :lem.prompt-window)

(define-condition execute ()
  ((input
    :initarg :input
    :reader execute-input)))

(define-condition abort-prompt ()
  ())

(defclass prompt-window-parameters ()
  ((completion-function
    :initarg :completion-function
    :reader completion-function)
   (existing-test-function
    :initarg :existing-test-function
    :reader existing-test-function)
   (called-window
    :initarg :called-window
    :reader called-window)))

(defclass prompt-window (floating-window prompt-window-parameters)
  ((start-point
    :accessor prompt-window-start-charpos))
  (:default-initargs
   :border 1))

(defclass prompt-buffer (buffer)
  ((prompt-string
    :initarg :prompt-string
    :reader prompt-buffer-prompt-string)
   (initial-string
    :initarg :initial-string
    :reader prompt-buffer-initial-string)))

(define-major-mode prompt-mode nil
    (:name "prompt"
     :keymap *prompt-mode-keymap*)
  (setf (lem::switchable-buffer-p (current-buffer)) t))

(define-attribute prompt-attribute
  (:light :foreground "gray27" :bold-p t)
  (:dark :foreground "snow" :bold-p t))

(define-key *prompt-mode-keymap* "Return" 'prompt-execute)
(define-key *prompt-mode-keymap* "Tab" 'prompt-completion)
(define-key *prompt-mode-keymap* "M-p" 'prompt-previous-history)
(define-key *prompt-mode-keymap* "M-n" 'prompt-next-history)
(define-key *prompt-mode-keymap* "C-g" 'prompt-abort)

(defun current-prompt-window ()
  (lem::frame-prompt-window (current-frame)))

(defun prompt-window-start-point (prompt-window)
  (let ((buffer (window-buffer prompt-window)))
    (character-offset (copy-point (buffer-start-point buffer) :temporary)
                      (prompt-window-start-charpos prompt-window))))

(defun get-prompt-string ()
  (points-to-string (prompt-window-start-point (current-prompt-window))
                    (buffer-end-point (window-buffer (current-prompt-window)))))

(define-command prompt-execute () ()
  (let ((input (get-prompt-string)))
    (when (or (zerop (length input))
              (null (existing-test-function (current-prompt-window)))
              (funcall (existing-test-function (current-prompt-window)) input))
      (error 'execute :input input))))

(define-command prompt-completion () ()
  (when (and (completion-function (current-prompt-window))
             lem::*minibuffer-completion-function*)
    (with-point ((start (prompt-window-start-point (current-prompt-window))))
      (funcall lem::*minibuffer-completion-function*
               (completion-function (current-prompt-window))
               start))))

(define-command prompt-previous-history () ()
  )

(define-command prompt-next-history () ()
  )

(define-command prompt-abort () ()
  (error 'abort-prompt))

(defun compute-window-rectangle (buffer)
  (flet ((compute-width ()
           (loop :for string :in (uiop:split-string (buffer-text buffer) :separator '(#\newline))
                 :maximize (+ 2 (string-width string)))))
    (let* ((width (alexandria:clamp (compute-width) 3 (- (display-width) 3)))
           (height (buffer-nlines buffer))
           (x (- (floor (display-width) 2)
                 (floor width 2)))
           (y (- (floor (display-height) 2)
                 (floor height 2))))
      (list x y width height))))

(defun make-prompt-window (buffer parameters)
  (destructuring-bind (x y width height)
      (compute-window-rectangle buffer)
    (make-instance 'prompt-window
                   :buffer buffer
                   :x x
                   :y y
                   :width width
                   :height height
                   :use-modeline-p nil
                   :completion-function (completion-function parameters)
                   :existing-test-function (existing-test-function parameters)
                   :called-window (called-window parameters))))

(defmethod lem::update-prompt-window ((window prompt-window))
  (destructuring-bind (x y width height)
      (compute-window-rectangle (window-buffer window))
    (lem::window-set-pos window x y)
    (lem::window-set-size window width height)))

(defun initialize-prompt (prompt-window buffer)
  (let ((*inhibit-read-only* t))
    (erase-buffer buffer))
  (let ((prompt-string (prompt-buffer-prompt-string buffer)))
    (when (plusp (length prompt-string))
      (insert-string (buffer-point buffer)
                     (prompt-buffer-prompt-string buffer)
                     :attribute 'prompt-attribute
                     :read-only t
                     :field t)
      (setf (prompt-window-start-charpos prompt-window)
            (length prompt-string)))
    (when-let (initial-string (prompt-buffer-initial-string buffer))
      (insert-string (buffer-point buffer) initial-string))))

(defun make-prompt-buffer (prompt-string initial-string)
  (let ((buffer (make-buffer "*prompt*" :temporary t)))
    (unless (typep buffer 'prompt-buffer)
      (change-class buffer
                    'prompt-buffer
                    :prompt-string prompt-string
                    :initial-string initial-string))
    buffer))

(defun show-prompt (prompt-string initial-string parameters)
  (let* ((buffer (make-prompt-buffer prompt-string initial-string))
         (prompt-window (make-prompt-window buffer parameters)))
    (setf (current-window) prompt-window)
    (assert (eq (current-buffer) buffer))
    (prompt-mode)
    (initialize-prompt prompt-window buffer)
    prompt-window))

(defun delete-prompt (prompt-window)
  (let ((frame (lem::get-frame-of-window prompt-window)))
    (when (eq prompt-window (frame-current-window frame))
      (let ((window (called-window prompt-window)))
        (setf (frame-current-window frame)
              (if (deleted-window-p window)
                  (first (window-list))
                  window))))
    (delete-window prompt-window)))

(defmacro with-unwind-setf (bindings form &body cleanup-forms)
  (let ((gensyms (mapcar (lambda (b)
                           (declare (ignore b))
                           (gensym))
                         bindings)))
    `(let ,(mapcar (lambda (g b)
                     `(,g ,(first b)))
                   gensyms
                   bindings)
       ,@(mapcar (lambda (b)
                   `(setf ,(first b) ,(second b)))
                 bindings)
       (unwind-protect ,form
         ,@cleanup-forms
         ,@(mapcar (lambda (g b)
                     `(setf ,(first b) ,g))
                   gensyms
                   bindings)))))

(defun !prompt-for-line (prompt-string
                         initial-string
                         completion-function
                         existing-test-function
                         history-name
                         &optional syntax-table)
  (declare (ignore history-name syntax-table))
  (when (lem::frame-prompt-window (current-frame))
    (editor-error "recursive use of prompt window"))
  (let* ((called-window (current-window))
         (prompt-window (show-prompt prompt-string
                                     initial-string
                                     (make-instance 'prompt-window-parameters
                                                    :completion-function completion-function
                                                    :existing-test-function existing-test-function
                                                    :called-window called-window))))
    (handler-case
        (with-unwind-setf (((lem::frame-prompt-window (current-frame))
                            prompt-window))
            (lem::command-loop)
          (delete-prompt prompt-window))
      (abort-prompt ()
        (error 'editor-abort))
      (execute (execute)
        (execute-input execute)))))

(define-command !prompt () ()
  (message "~A"
           (!prompt-for-line "hello: "
                             "cxxxr"
                             lem::*minibuffer-buffer-complete-function*
                             (lambda (name)
                               (member name (buffer-list) :test #'string= :key #'buffer-name))
                             nil)))

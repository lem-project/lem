(defpackage :lem.prompt-window
  (:use :cl :lem)
  (:import-from :alexandria
                :when-let))
(in-package :lem.prompt-window)

(defvar *history-table* (make-hash-table))

(define-condition execute ()
  ((input
    :initarg :input
    :reader execute-input)))

(define-condition abort-prompt ()
  ())

(defclass prompt-parameters ()
  ((completion-function
    :initarg :completion-function
    :reader prompt-window-completion-function)
   (existing-test-function
    :initarg :existing-test-function
    :reader completion-window-existing-test-function)
   (called-window
    :initarg :called-window
    :reader prompt-window-called-window)
   (history
    :initarg :history
    :reader prompt-window-history)))

(defclass prompt-window (floating-window prompt-parameters)
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

(defun current-point-in-prompt ()
  (buffer-point (window-buffer (current-prompt-window))))

(defun get-between-input-points ()
  (list (prompt-window-start-point (current-prompt-window))
        (buffer-end-point (window-buffer (current-prompt-window)))))

(defun get-prompt-string ()
  (apply #'points-to-string (get-between-input-points)))

(defun replace-prompt-input (input)
  (apply #'delete-between-points (get-between-input-points))
  (insert-string (current-point-in-prompt) input))

(defun replace-if-history-exists (next-history-fn)
  (multiple-value-bind (string exists-p)
      (funcall next-history-fn
               (prompt-window-history (current-prompt-window)))
    (when exists-p
      (replace-prompt-input string))))

(define-command prompt-execute () ()
  (let ((input (get-prompt-string)))
    (when (or (zerop (length input))
              (null (completion-window-existing-test-function (current-prompt-window)))
              (funcall (completion-window-existing-test-function (current-prompt-window)) input))
      (lem.history:add-history (prompt-window-history (current-prompt-window)) input)
      (error 'execute :input input))))

(define-command prompt-completion () ()
  (alexandria:when-let (completion-fn (prompt-window-completion-function (current-prompt-window)))
    (with-point ((start (prompt-window-start-point (current-prompt-window))))
      (lem.completion-mode:run-completion
       (lambda (point)
         (with-point ((start start)
                      (end point))
           (let ((items (funcall completion-fn
                                 (points-to-string start
                                                   (buffer-end-point (point-buffer end))))))
             (loop :for item :in items
                   :when (typecase item
                           (string
                            (lem.completion-mode:make-completion-item :label item
                                                                      :start start
                                                                      :end end))
                           (lem.completion-mode:completion-item
                            item))
                   :collect :it))))))))

(define-command prompt-previous-history () ()
  (let ((history (prompt-window-history (current-prompt-window))))
    (lem.history:backup-edit-string history (get-prompt-string))
    (replace-if-history-exists #'lem.history:prev-history)))

(define-command prompt-next-history () ()
  (let ((history (prompt-window-history (current-prompt-window))))
    (lem.history:backup-edit-string history (get-prompt-string))
    (or (replace-if-history-exists #'lem.history:next-history)
        (replace-if-history-exists #'lem.history:restore-edit-string))))

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
                   :completion-function (prompt-window-completion-function parameters)
                   :existing-test-function (completion-window-existing-test-function parameters)
                   :called-window (prompt-window-called-window parameters)
                   :history (prompt-window-history parameters))))

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
      (let ((window (prompt-window-called-window prompt-window)))
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

(defun get-history (history-name)
  (or (gethash history-name *history-table*)
      (setf (gethash history-name *history-table*)
            (lem.history:make-history))))

(defun !prompt-for-line (prompt-string
                         initial-string
                         completion-function
                         existing-test-function
                         history-name
                         &optional (syntax-table (current-syntax)))
  (when (lem::frame-prompt-window (current-frame))
    (editor-error "recursive use of prompt window"))
  (let* ((called-window (current-window))
         (prompt-window (show-prompt prompt-string
                                     initial-string
                                     (make-instance 'prompt-parameters
                                                    :completion-function completion-function
                                                    :existing-test-function existing-test-function
                                                    :called-window called-window
                                                    :history (get-history history-name)))))
    (handler-case
        (with-unwind-setf (((lem::frame-prompt-window (current-frame))
                            prompt-window))
            (with-current-syntax syntax-table
              (lem::command-loop))
          (delete-prompt prompt-window))
      (abort-prompt ()
        (error 'editor-abort))
      (execute (execute)
        (execute-input execute)))))

(define-command !prompt () ()
  (message "~A"
           (!prompt-for-line "hello: "
                             ""
                             nil
                             nil
                             #+(or)
                             (lambda (name)
                               (member name (buffer-list) :test #'string= :key #'buffer-name))
                             nil)))

(defpackage :lem.prompt-window
  (:use :cl :lem)
  (:import-from :alexandria
                :when-let)
  #+sbcl
  (:lock t))
(in-package :lem.prompt-window)

(defparameter +border-size+ 1)
(defparameter +min-width+   3)
(defparameter +min-height+  1)

(defvar *extra-side-margin* 0)

(defvar *history-table* (make-hash-table))

(defvar *prompt-activate-hook* '())
(defvar *prompt-deactivate-hook* '())

(define-condition execute ()
  ((input
    :initarg :input
    :reader execute-input)))

(defclass prompt-parameters ()
  ((completion-function
    :initarg :completion-function
    :initform nil
    :reader prompt-window-completion-function)
   (existing-test-function
    :initarg :existing-test-function
    :initform nil
    :reader prompt-window-existing-test-function)
   (caller-of-prompt-window
    :initarg :caller-of-prompt-window
    :initform nil
    :reader prompt-window-caller-of-prompt-window)
   (history
    :initarg :history
    :initform nil
    :reader prompt-window-history)))

(defclass floating-prompt (floating-window prompt-parameters)
  ((start-charpos
    :accessor prompt-window-start-charpos))
  (:default-initargs
   :border +border-size+))

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
  (setf (lem::not-switchable-buffer-p (current-buffer)) t)
  (setf (variable-value 'truncate-lines :buffer (current-buffer)) nil))

(define-attribute prompt-attribute
  (:light :foreground "gray27" :bold-p t)
  (:dark :foreground "snow" :bold-p t))

(define-key *prompt-mode-keymap* "Return" 'prompt-execute)
(define-key *prompt-mode-keymap* "Tab" 'prompt-completion)
(define-key *prompt-mode-keymap* "M-p" 'prompt-previous-history)
(define-key *prompt-mode-keymap* "M-n" 'prompt-next-history)

(defun current-prompt-window ()
  (frame-floating-prompt-window (current-frame)))

(defmethod prompt-start-point ((prompt floating-prompt))
  (let ((buffer (window-buffer prompt)))
    (character-offset (copy-point (buffer-start-point buffer) :temporary)
                      (prompt-window-start-charpos prompt))))

(defmethod lem::caller-of-prompt-window ((prompt floating-prompt))
  (prompt-window-caller-of-prompt-window prompt))

(defun current-prompt-start-point ()
  (prompt-start-point (current-prompt-window)))

(defun current-point-in-prompt ()
  (buffer-point (window-buffer (current-prompt-window))))

(defun get-between-input-points ()
  (list (current-prompt-start-point)
        (buffer-end-point (window-buffer (current-prompt-window)))))

(defun get-input-string ()
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
  (let ((input (get-input-string)))
    (when (or (zerop (length input))
              (null (prompt-window-existing-test-function (current-prompt-window)))
              (funcall (prompt-window-existing-test-function (current-prompt-window)) input))
      (lem.history:add-history (prompt-window-history (current-prompt-window)) input)
      (error 'execute :input input))))

(define-command prompt-completion () ()
  (alexandria:when-let (completion-fn (prompt-window-completion-function (current-prompt-window)))
    (with-point ((start (current-prompt-start-point)))
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
    (lem.history:backup-edit-string history (get-input-string))
    (replace-if-history-exists #'lem.history:prev-history)))

(define-command prompt-next-history () ()
  (let ((history (prompt-window-history (current-prompt-window))))
    (lem.history:backup-edit-string history (get-input-string))
    (or (replace-if-history-exists #'lem.history:next-history)
        (replace-if-history-exists #'lem.history:restore-edit-string))))

(defun compute-window-rectangle (buffer)
  (flet ((compute-width ()
           (loop :for string :in (uiop:split-string (buffer-text buffer) :separator '(#\newline))
                 :maximize (+ (string-width string) 2))))
    (let* ((b2 (* +border-size+ 2))
           (width  (max (min (compute-width)
                             (- (display-width)  b2 *extra-side-margin*))
                        +min-width+))
           (height (max (min (buffer-nlines buffer)
                             (- (display-height) b2))
                        +min-height+))
           (x (- (floor (display-width) 2)
                 (floor width 2)))
           (y (- (floor (display-height) 2)
                 (floor height 2))))
      (list x y width height))))

(defun make-prompt-window (buffer parameters)
  (destructuring-bind (x y width height)
      (compute-window-rectangle buffer)
    (make-instance 'floating-prompt
                   :buffer buffer
                   :x x
                   :y y
                   :width width
                   :height height
                   :use-modeline-p nil
                   :completion-function (prompt-window-completion-function parameters)
                   :existing-test-function (prompt-window-existing-test-function parameters)
                   :caller-of-prompt-window (prompt-window-caller-of-prompt-window parameters)
                   :history (prompt-window-history parameters))))

(defmethod update-prompt-window ((window floating-prompt))
  (destructuring-bind (x y width height)
      (compute-window-rectangle (window-buffer window))
    (unless (and (= x (window-x window))
                 (= y (window-y window)))
      (lem::window-set-pos window x y))
    (unless (and (= width (window-width window))
                 (= height (window-height window)))
      (lem::window-set-size window width height))))

(defun initialize-prompt-buffer (buffer)
  (let ((*inhibit-read-only* t)
        (prompt-string (prompt-buffer-prompt-string buffer)))
    (erase-buffer buffer)
    (when (plusp (length prompt-string))
      (insert-string (buffer-point buffer)
                     prompt-string
                     :attribute 'prompt-attribute
                     :read-only t
                     :field t))
    (when-let (initial-string (prompt-buffer-initial-string buffer))
      (insert-string (buffer-point buffer) initial-string))))

(defun make-prompt-buffer (prompt-string initial-string)
  (let ((buffer (make-buffer "*prompt*" :temporary t)))
    (unless (typep buffer 'prompt-buffer)
      (change-class buffer
                    'prompt-buffer
                    :prompt-string prompt-string
                    :initial-string initial-string))
    (initialize-prompt-buffer buffer)
    buffer))

(defun initialize-prompt (prompt-window)
  (let* ((buffer (window-buffer prompt-window))
         (prompt-string (prompt-buffer-prompt-string buffer)))
    (when (plusp (length prompt-string))
      (setf (prompt-window-start-charpos prompt-window)
            (length prompt-string)))
    (buffer-end (buffer-point buffer))))

(defun create-prompt (prompt-string initial-string parameters)
  (let* ((buffer (make-prompt-buffer prompt-string initial-string))
         (prompt-window (make-prompt-window buffer parameters)))
    (change-buffer-mode buffer 'prompt-mode)
    (initialize-prompt prompt-window)
    prompt-window))

(defun switch-to-prompt-window (prompt-window)
  (setf (current-window) prompt-window)
  (buffer-end (buffer-point (window-buffer prompt-window))))

(defun delete-prompt (prompt-window)
  (let ((frame (lem::get-frame-of-window prompt-window)))
    (when (eq prompt-window (frame-current-window frame))
      (let ((window (prompt-window-caller-of-prompt-window prompt-window)))
        (setf (frame-current-window frame)
              (if (deleted-window-p window)
                  (first (window-list))
                  window))))
    (delete-window prompt-window)))

(defun get-history (history-name)
  (or (gethash history-name *history-table*)
      (setf (gethash history-name *history-table*)
            (lem.history:make-history))))

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

(defun prompt-for-aux (&key (prompt-string (alexandria:required-argument :prompt-string))
                            (initial-string (alexandria:required-argument :initial-string))
                            (parameters (alexandria:required-argument :parameters))
                            (body-function (alexandria:required-argument :body-function))
                            (syntax-table nil))
  (when (frame-floating-prompt-window (current-frame))
    (editor-error "recursive use of prompt window"))
  (run-hooks *prompt-activate-hook*)
  (with-current-window (current-window)
    (let* ((prompt-window (create-prompt prompt-string
                                         initial-string
                                         parameters)))
      (switch-to-prompt-window prompt-window)
      (handler-case
          (with-unwind-setf (((frame-floating-prompt-window (current-frame))
                              prompt-window))
              (if syntax-table
                  (with-current-syntax syntax-table
                    (funcall body-function))
                  (funcall body-function))
            (delete-prompt prompt-window)
            (run-hooks *prompt-deactivate-hook*))
        (execute (execute)
          (execute-input execute))))))

(defmethod prompt-for-character (prompt-string)
  (let ((caller-of-prompt-window (current-window)))
    (prompt-for-aux :prompt-string prompt-string
                    :initial-string ""
                    :parameters (make-instance 'prompt-parameters
                                               :caller-of-prompt-window caller-of-prompt-window)
                    :body-function (lambda ()
                                     (with-current-window caller-of-prompt-window
                                       (redraw-display t)
                                       (let ((key (read-key)))
                                         (if (lem::abort-key-p key)
                                             (error 'editor-abort)
                                             (key-to-char key))))))))

(defun prompt-for-line-command-loop ()
  (handler-bind ((editor-abort
                   (lambda (c)
                     (error c)))
                 (editor-condition
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'lem-restart:message))))
    (lem::command-loop)))

(defmethod prompt-for-line (prompt-string
                            initial-string
                            completion-function
                            existing-test-function
                            history-name
                            &optional (syntax-table (current-syntax)))
  (prompt-for-aux :prompt-string prompt-string
                  :initial-string initial-string
                  :parameters (make-instance 'prompt-parameters
                                             :completion-function completion-function
                                             :existing-test-function existing-test-function
                                             :caller-of-prompt-window (current-window)
                                             :history (get-history history-name))
                  :syntax-table syntax-table
                  :body-function #'prompt-for-line-command-loop))

(defmethod active-minibuffer-window ()
  (current-prompt-window))

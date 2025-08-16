(defpackage :lem/prompt-window
  (:use :cl :lem)
  (:import-from :alexandria
                :when-let
                :when-let*)
  #+sbcl
  (:lock t)
  (:export :prompt-attribute
           :*prompt-completion-window-shape*
           :*automatic-tab-completion*
           :current-prompt-window
           :find-command-keybindings-in-keymap))
(in-package :lem/prompt-window)

(defconstant +border-size+ 1)
(defconstant +min-height+  1)

(defvar *fill-width* nil)
(defvar *history-table* (make-hash-table))

(defvar *special-paths*
  #+unix '(("//" . "/")
           ("~/" . "~/"))
  #-unix nil)

(define-condition execute-condition ()
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
    :reader prompt-window-history)
   (gravity
    :initarg :gravity
    :initform :center
    :reader prompt-gravity)
   (use-border
    :initarg :use-border
    :initform t
    :reader prompt-use-border-p)))

(defclass floating-prompt (floating-window prompt-parameters)
  ((start-charpos
    :accessor prompt-window-start-charpos)))

(defclass prompt-buffer (text-buffer)
  ((prompt-string
    :initarg :prompt-string
    :reader prompt-buffer-prompt-string)
   (initial-string
    :initarg :initial-string
    :reader prompt-buffer-initial-string)))

(define-major-mode prompt-mode nil
    (:name "prompt"
     :keymap *prompt-mode-keymap*)
  (setf (not-switchable-buffer-p (current-buffer)) t)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) nil)
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil))

(define-attribute prompt-attribute
  (t :foreground :base07 :bold t))

(define-key *prompt-mode-keymap* "Return" 'prompt-execute)
(define-key *prompt-mode-keymap* "Tab" 'prompt-completion)
(define-key *prompt-mode-keymap* "M-p" 'prompt-previous-history)
(define-key *prompt-mode-keymap* "M-n" 'prompt-next-history)
(define-key *prompt-mode-keymap* "Up" 'prompt-previous-history)
(define-key *prompt-mode-keymap* "Down" 'prompt-next-history)
(define-key *prompt-mode-keymap* 'delete-active-window 'prompt-quit)

(defun current-prompt-window ()
  (frame-floating-prompt-window (current-frame)))

(defun prompt-start-point (prompt)
  (let ((buffer (window-buffer prompt)))
    (character-offset (copy-point (buffer-start-point buffer) :temporary)
                      (prompt-window-start-charpos prompt))))

(defmethod window-parent ((prompt floating-prompt))
  (prompt-window-caller-of-prompt-window prompt))

(defmethod caller-of-prompt-window ((prompt floating-prompt))
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

(defmethod get-prompt-input-string ((prompt floating-prompt))
  (get-input-string))

(defun replace-prompt-input (input)
  (apply #'delete-between-points (get-between-input-points))
  (insert-string (current-point-in-prompt) input))

(defun replace-if-history-exists (next-history-fn)
  (multiple-value-bind (string exists-p)
      (funcall next-history-fn
               (prompt-window-history (current-prompt-window)))
    (when exists-p
      (replace-prompt-input string))))

(define-command prompt-quit () ()
  (error 'editor-abort :message nil))

(define-command prompt-execute () ()
  (let ((input (get-input-string)))
    (when (or (null (prompt-window-existing-test-function (current-prompt-window)))
              (funcall (prompt-window-existing-test-function (current-prompt-window)) input))
      (lem/common/history:add-history (prompt-window-history (current-prompt-window)) input)
      (error 'execute-condition :input input))))

(defvar *prompt-completion-window-shape* :drop-curtain)
(defvar *prompt-completion-window-gravity* :horizontally-adjacent-window)

(defun open-prompt-completion ()
  (alexandria:when-let (completion-fn (prompt-window-completion-function (current-prompt-window)))
    (with-point ((start (current-prompt-start-point)))
      (lem/completion-mode:run-completion
       (lambda (point)
         (with-point ((start start)
                      (end point))
           (let ((items (funcall completion-fn
                                 (points-to-string start
                                                   (buffer-end-point (point-buffer end))))))
             (loop :for item :in items
                   :when (typecase item
                           (string
                            (lem/completion-mode:make-completion-item :label item
                                                                      :start start
                                                                      :end end))
                           (lem/completion-mode:completion-item
                            item))
                   :collect :it))))
       :style `(:gravity ,*prompt-completion-window-gravity*
                :offset-y -1
                :shape ,*prompt-completion-window-shape*)
       :then (lambda ()
               (update-prompt-window (current-prompt-window)))))))

(define-command prompt-completion () ()
  (open-prompt-completion))


(define-command prompt-previous-history () ()
  (let ((history (prompt-window-history (current-prompt-window))))
    (lem/common/history:backup-edit-string history (get-input-string))
    (replace-if-history-exists #'lem/common/history:previous-history)))

(define-command prompt-next-history () ()
  (let ((history (prompt-window-history (current-prompt-window))))
    (lem/common/history:backup-edit-string history (get-input-string))
    (or (replace-if-history-exists #'lem/common/history:next-history)
        (replace-if-history-exists #'lem/common/history:restore-edit-string))))

(defun min-width ()
  (if *fill-width*
      (1- (display-width))
      (round (* (display-width) 2/3))))

(defun compute-window-rectangle (buffer &key gravity source-window child-completion-window)
  (destructuring-bind (width height) (lem/popup-window::compute-buffer-size buffer)
    (when child-completion-window
      (destructuring-bind (child-width child-height)
          (lem/popup-window::compute-buffer-size (window-buffer child-completion-window))
        (declare (ignore child-height))
        (setf width (max width child-width))))
    (lem/popup-window::compute-popup-window-rectangle
     (lem/popup-window::ensure-gravity gravity)
     :source-window source-window
     ;; Find File: <file-name>|
     ;;                       ^ ここにカーソルがあるとき、widthは1つ余分に幅が必要
     :width (alexandria:clamp (1+ width) (min-width) (- (display-width) 2))
     :height (alexandria:clamp height +min-height+ (- (display-height) 2)))))

(defun make-prompt-window (buffer parameters)
  (destructuring-bind (x y width height)
      (compute-window-rectangle buffer
                                :gravity (prompt-gravity parameters)
                                :source-window (prompt-window-caller-of-prompt-window parameters))
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
                   :history (prompt-window-history parameters)
                   :gravity (prompt-gravity parameters)
                   :border (if (prompt-use-border-p parameters) +border-size+ 0))))

(defun get-child-completion-window ()
  (when-let* ((context lem/completion-mode::*completion-context*)
              (popup-menu (lem/completion-mode::context-popup-menu context)))
    (when popup-menu
      (lem/popup-menu::popup-menu-window popup-menu))))

(defmethod update-prompt-window ((window floating-prompt))
  (let ((completion-window (get-child-completion-window)))
    (destructuring-bind (x y width height)
        (compute-window-rectangle (window-buffer window)
                                  :gravity (prompt-gravity window)
                                  :source-window (prompt-window-caller-of-prompt-window window)
                                  :child-completion-window completion-window)
      (unless (and (= x (window-x window))
                   (= y (window-y window)))
        (window-set-pos window x y))
      (let ((width (max width (if completion-window (window-width completion-window) 0))))
        (unless (and (= width (window-width window))
                     (= height (window-height window)))
          (window-set-size window width height)))
      (when completion-window
        (window-set-pos completion-window
                        x
                        (window-y completion-window))))))

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
    (setf (prompt-window-start-charpos prompt-window)
          (length prompt-string))
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
  (let ((frame (get-frame-of-window prompt-window)))
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
            (lem/common/history:make-history))))

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

(defun exit-prompt (old-window)
  (declare (ignore old-window))
  (error 'editor-abort :message nil))

(defun prompt-for-aux (&key (prompt-string (alexandria:required-argument :prompt-string))
                            (initial-string (alexandria:required-argument :initial-string))
                            (parameters (alexandria:required-argument :parameters))
                            (body-function (alexandria:required-argument :body-function))
                            (syntax-table nil)
                            (edit-callback nil)
                            (special-keymap nil))
  (when (frame-floating-prompt-window (current-frame))
    (editor-error "recursive use of prompt window"))
  (run-hooks *prompt-activate-hook*)

  (with-current-window (current-window)
    (let* ((prompt-window (create-prompt prompt-string
                                         initial-string
                                         parameters)))
      (switch-to-prompt-window prompt-window)

      (add-hook (window-leave-hook prompt-window) #'exit-prompt)
      (handler-case
          (with-unwind-setf (((frame-floating-prompt-window (current-frame))
                              prompt-window))

              (let ((*post-command-hook* *post-command-hook*))
                (when edit-callback
                  (add-hook *post-command-hook*
                            (lambda ()
                              (when (typep (this-command) 'lem:editable-advice)
                                (funcall edit-callback (get-input-string))))))
                (run-hooks *prompt-after-activate-hook*)
                (when *automatic-tab-completion*
                  (open-prompt-completion))
                (with-special-keymap (special-keymap)
                  (if syntax-table
                      (with-current-syntax syntax-table
                        (funcall body-function))
                      (funcall body-function))))
          
            (lem/completion-mode:completion-end)
            (remove-hook (window-leave-hook prompt-window) #'exit-prompt)
            (delete-prompt prompt-window)
            (run-hooks *prompt-deactivate-hook*))
        (execute-condition (e)
          (execute-input e))))))

(defmethod lem-core::%prompt-for-character (prompt-string &key (gravity :center))
  (prompt-for-aux :prompt-string prompt-string
                  :initial-string ""
                  :parameters (make-instance 'prompt-parameters
                                             :caller-of-prompt-window (current-window)
                                             :gravity gravity)
                  :body-function (lambda ()
                                   (redraw-display)
                                   (let ((key (read-key)))
                                     (if (abort-key-p key)
                                         (error 'editor-abort)
                                         (key-to-char key))))))

(defun prompt-for-line-command-loop ()
  (handler-bind ((editor-abort
                   (lambda (c)
                     (error c)))
                 (editor-condition
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'lem-restart:message))))
    (command-loop)))

(defmethod lem-core::%prompt-for-line (prompt-string
                                       &key initial-value
                                            completion-function
                                            test-function
                                            history-symbol
                                            (syntax-table (current-syntax))
                                            gravity
                                            edit-callback
                                            special-keymap
                                            (use-border t))
  (prompt-for-aux :prompt-string prompt-string
                  :initial-string initial-value
                  :parameters (make-instance 'prompt-parameters
                                             :completion-function completion-function
                                             :existing-test-function test-function
                                             :caller-of-prompt-window (current-window)
                                             :history (get-history history-symbol)
                                             :gravity (or gravity lem-core::*default-prompt-gravity*)
                                             :use-border use-border)
                  :syntax-table syntax-table
                  :body-function #'prompt-for-line-command-loop
                  :edit-callback edit-callback
                  :special-keymap special-keymap))

(defmethod active-prompt-window ()
  (current-prompt-window))

(defmethod prompt-active-p ((prompt floating-prompt))
  (eq prompt (current-window)))

(defun normalize-path-marker (path marker replace)
  (let ((split (str:split marker path)))
    (if (= 1 (length split))
        path
        (concatenate 'string replace (car (last split))))))

(defun normalize-path-input (path)
  (reduce (lambda (ag pair) (normalize-path-marker ag (car pair) (cdr pair)))
          *special-paths*
          :initial-value path))


(defun prompt-file-completion (string directory &key directory-only)
  (replace-prompt-input (normalize-path-input string))
  (flet ((move-to-file-start (point)
           ;; Move the point to the start of the file.
           ;; /foo/bar/baz.txt
           ;;          ^
           (line-end point)
           (search-backward point "/")
           (character-offset point 1)
           point))
    (mapcar (lambda (filename)
              (let ((label (tail-of-pathname filename)))
                (with-point ((s (current-prompt-start-point))
                             (e (current-prompt-start-point)))
                  (lem/completion-mode:make-completion-item
                   :label label
                   :start (move-to-file-start s)
                   :end (line-end e)))))
            (completion-file string directory :directory-only directory-only))))

(defun prompt-buffer-completion (string)
  (loop :for buffer :in (completion-buffer string)
        :collect (with-point ((s (current-prompt-start-point))
                              (e (current-prompt-start-point)))
                   (lem/completion-mode:make-completion-item
                    :detail (alexandria:if-let (filename (buffer-filename buffer))
                              (enough-namestring filename (probe-file "./"))
                              "")
                    :label (buffer-name buffer)
                    :start s
                    :end (line-end e)))))

(declaim (inline find-command-keybindings-in-keymap))
(defun find-command-keybindings-in-keymap (command &optional (keymap *global-keymap*))
  "Return a list of keybindings (strings) for a COMMAND (command object or string).

  Search in the given KEYMAP. See also collect-command-all-keybindings."
  (when (stringp command)
    (setf command (find-command command)))
  (alexandria:when-let (keybindings (collect-command-keybindings (command-name command) keymap))
    (mapcar (lambda (keybinding)
              (format nil "~{~A~^ ~}" keybinding))
            keybindings)))

(defun collect-command-all-keybindings (buffer command)
  (format nil "~{~A~^, ~}"
          (append (find-command-keybindings-in-keymap command (mode-keymap (buffer-major-mode buffer)))
                  (loop :for mode :in (buffer-minor-modes buffer)
                        :for keymap := (mode-keymap mode)
                        :when keymap
                        :append (find-command-keybindings-in-keymap command keymap))
                  (find-command-keybindings-in-keymap command *global-keymap*))))

(defun prompt-command-completion (string &key candidates)
  "Filter the list of commands from an input string and return a list of command completion items.

  For each command, find and display its available keybindings for the current keymaps.
  Display CANDIDATES command names (list of strings) before the rest of all commands.
  The rest of commands are sorted by name."
  (flet ((collect-items (candidates)
           (loop :for name :in candidates
                 :for command := (find-command name)
                 ;; avoid commands created in a REPL session but now non-existent.
                 :unless (null command)
                 :collect (lem/completion-mode:make-completion-item
                           :label name
                           :detail (collect-command-all-keybindings
                                    (current-buffer)
                                    command))))

         (filter-items (items)
           (if (find #\- string)
               (completion-hyphen string
                                  items
                                  :key #'lem/completion-mode:completion-item-label)
               (completion string
                           items
                           :key #'lem/completion-mode:completion-item-label))))

    (let* ((all-items (collect-items (sort (all-command-names) #'string<)))
           (candidate-items (collect-items candidates))
           (items (remove-duplicates
                   (append candidate-items all-items)
                   :test #'equal
                   :key #'lem/completion-mode:completion-item-label
                   :from-end t)))
      (filter-items items))))

(setf *prompt-file-completion-function* 'prompt-file-completion)
(setf *prompt-buffer-completion-function* 'prompt-buffer-completion)
(setf *prompt-command-completion-function* 'prompt-command-completion)

(defvar *file-prompt-keymap* (make-keymap :name '*file-mode-prompt-keymap*))
(define-key *file-prompt-keymap* "C-Backspace" 'file-prompt-parent-folder)

(define-command file-prompt-parent-folder () ()
  "In file prompt jump the parent folder and show the completion results for that folder."
  (when (char= (character-at (current-point) -1) #\/)
      (delete-previous-char))
  (with-point ((end (current-point)))
    (let ((point (search-backward (current-point) "/")))
      (when point
        (lem:character-offset point 1)
        (with-point ((start (current-point)))
          (delete-between-points start end)))))
  (lem/completion-mode:completion-refresh))

(defmethod lem-core::%prompt-for-file (prompt directory default existing gravity)
  (let ((result
          (lem-core::%prompt-for-line (if default
                                (format nil "~a(~a) " prompt default)
                                prompt)
                            :initial-value (when directory (princ-to-string directory))
                            :completion-function
                            (when *prompt-file-completion-function*
                              (lambda (str)
                                (funcall *prompt-file-completion-function*
                                         (if (alexandria:emptyp str)
                                             "./"
                                             str)
                                         (or directory
                                             (namestring (user-homedir-pathname))))))
                            :test-function (and existing #'virtual-probe-file)
                            :history-symbol 'prompt-for-file
                            :gravity gravity
                            :special-keymap *file-prompt-keymap*)))
    (if (string= result "")
        default
        result)))

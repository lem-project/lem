(defpackage :lem-vi-mode/core
  (:use :cl
        :lem
        :lem/universal-argument)
  (:import-from :cl-package-locks)
  (:import-from :cl-ppcre)
  (:export :*enable-hook*
           :*disable-hook*
           :vi-state
           :vi-mode
           :define-vi-state
           :current-state
           :change-state
           :with-state
           :*command-keymap*
           :*insert-keymap*
           :*inactive-keymap*
           :post-command-hook
           :state-enabled-hook
           :state-disabled-hook
           :normal
           :insert
           :change-directory
           :expand-filename-modifiers
           :kill-region-without-appending))
(in-package :lem-vi-mode/core)

(defvar *default-cursor-color* nil)

(defvar *enable-hook* '())
(defvar *disable-hook* '())

(defun enable-hook ()
  (run-hooks *enable-hook*))

(defun disable-hook ()
  (run-hooks *disable-hook*))

(define-global-mode vi-mode (emacs-mode)
  (:name "vi"
   :enable-hook #'enable-hook
   :disable-hook #'disable-hook))


(defvar *modeline-element*)

(define-attribute state-modeline-white
  (t :foreground "white" :reverse t))

(define-attribute state-modeline-yellow
  (t :foreground "yellow" :reverse t))

(define-attribute state-modeline-green
  (t :foreground "#003300" :reverse t))

(define-attribute state-modeline-aqua
  (t :foreground "#33CCFF" :reverse t))

(define-attribute state-modeline-orange
  (t :foreground "orange" :reverse t))

(defstruct (vi-modeline-element (:conc-name element-))
  name
  attribute)

(defmethod convert-modeline-element ((element vi-modeline-element) window)
  (if (or (eq (lem:current-window) window)
          (string= (element-name element) " COMMAND "))
      (values (element-name element)
              (element-attribute element))
      (values "" 'state-modeline-white)))

(defun initialize-vi-modeline ()
  (setf *modeline-element* (make-vi-modeline-element))
  (pushnew *modeline-element* (lem:variable-value 'lem:modeline-format :global)))

(defun finalize-vi-modeline ()
  (setf (lem:variable-value 'lem:modeline-format :global)
        (remove-if #'vi-modeline-element-p
                   (lem:variable-value 'lem:modeline-format :global)))
  (modeline-remove-status-list *modeline-element*)
  (set-attribute 'cursor :background *default-cursor-color*)
  (lem-if:update-cursor-shape (lem-core:implementation) :box))

(defun change-element-by-state (state)
  (setf (element-name *modeline-element*) (format nil " ~A " (state-name state))
        (element-attribute *modeline-element*) (state-modeline-color state)))


(defclass vi-state ()
  ((name :initarg :name
         :reader state-name)
   (message
    :initarg :message
    :reader state-message)
   (cursor-type
    :initarg :cursor-type
    :initform :box
    :reader state-cursor-type)
   (modeline-color
    :initarg :modeline-color
    :initform 'state-modeline-white
    :reader state-modeline-color)
   (keymap
    :initarg :keymap
    :reader state-keymap)
   (cursor-color
    :initarg :cursor-color
    :accessor state-cursor-color))
  (:default-initargs
   :message nil
   :cursor-color nil
   :keymap *global-keymap*))

(defmethod initialize-instance ((state vi-state) &rest initargs &key name &allow-other-keys)
  (unless name
    (setf (getf initargs :name) (class-name (class-of state))))
  (apply #'call-next-method state initargs))

(defvar *current-state* nil)

;;; vi-state methods
(defmacro define-vi-state (name direct-super-classes direct-slot-specs &rest options)
  (let ((cleaned-super-classes (if (null direct-super-classes) '(vi-state) direct-super-classes)))
    `(progn
       (assert (find 'vi-state ',cleaned-super-classes :test #'(lambda (expected-class class) (closer-mop:subclassp class expected-class))) () "At least one of the direct-super-classes should be vi-state or a subclass of vi-state!")
       (defclass ,name ,cleaned-super-classes
         ,direct-slot-specs
         ,@options)
       (setf (get ',name 'state)
             (make-instance ',name)))))

(defgeneric post-command-hook (state))

(defmethod post-command-hook ((state vi-state)))

(defgeneric state-enabled-hook (state))

(defmethod state-enabled-hook ((state vi-state))
  (let ((msg (state-message state)))
    (unless (null msg)
      (message msg))))

(defgeneric state-disabled-hook (state))

(defmethod state-disabled-hook ((state vi-state)))

(defun current-state ()
  *current-state*)

(defun ensure-state (state)
  (setf state
        (if (symbolp state)
            (get state 'state)
            state))
  (assert (typep state 'vi-state))
  state)

(defun change-state (name)
  (and *current-state*
       (state-disabled-hook (ensure-state *current-state*))) 
  (let ((state (ensure-state name)))
    (setf *current-state* name)
    (change-global-mode-keymap 'vi-mode (state-keymap state))
    (change-element-by-state state)
    (state-enabled-hook state)
    (unless *default-cursor-color*
      (setf *default-cursor-color*
            (attribute-background (ensure-attribute 'cursor nil))))
    (set-attribute 'cursor :background (or (state-cursor-color state) *default-cursor-color*))))

(defmacro with-state (state &body body)
  (alexandria:with-gensyms (old-state)
    `(let ((,old-state (current-state)))
       (change-state ,state)
       (unwind-protect (progn ,@body)
         (change-state ,old-state)))))


(defvar *command-keymap* (make-keymap :name '*command-keymap*
                                      :parent *global-keymap*))
(defvar *inactive-keymap* (make-keymap :parent *global-keymap*))

(define-vi-state normal () ()
  (:default-initargs
   :keymap *command-keymap*
   :modeline-color 'state-modeline-yellow))

;; insert state
(defvar *insert-keymap* (make-keymap :name '*insert-keymap* :parent *global-keymap*))

(define-vi-state insert () ()
  (:default-initargs
   :message "-- INSERT --"
   :cursor-color "IndianRed"
   :cursor-type :bar
   :modeline-color 'state-modeline-aqua
   :keymap *insert-keymap*))

(define-vi-state vi-modeline () ()
  (:default-initargs
   :name "COMMAND"
   :modeline-color 'state-modeline-green
   :keymap *inactive-keymap*))

(defun prompt-activate-hook () (change-state 'vi-modeline))
(defun prompt-deactivate-hook () (change-state 'normal))

(defun vi-post-command-hook ()
  (when (mode-active-p (current-buffer) 'vi-mode)
    (post-command-hook (ensure-state (current-state)))))

(add-hook *post-command-hook* 'vi-post-command-hook)

(add-hook *enable-hook*
          (lambda ()
            (initialize-vi-modeline)
            (change-state 'normal)
            (add-hook *prompt-activate-hook* 'prompt-activate-hook)
            (add-hook *prompt-deactivate-hook* 'prompt-deactivate-hook)))

(add-hook *disable-hook*
          (lambda ()
            (finalize-vi-modeline)
            (remove-hook *prompt-activate-hook* 'prompt-activate-hook)
            (remove-hook *prompt-deactivate-hook* 'prompt-deactivate-hook)))

(defmethod state-enabled-hook :after (state)
  (lem-if:update-cursor-shape (lem-core:implementation)
                              (state-cursor-type state)))

(defvar *previous-cwd* nil)

(defun change-directory (new-directory)
  (check-type new-directory (or string pathname))
  (let* ((previous-directory (uiop:getcwd))
         (new-directory (cond
                          ((equal new-directory "")
                           (user-homedir-pathname))
                          ((equal new-directory "-")
                           (or *previous-cwd* previous-directory))
                          (t
                           (truename
                            (merge-pathnames (uiop:ensure-directory-pathname new-directory) previous-directory))))))
    (assert (uiop:absolute-pathname-p new-directory))
    (uiop:chdir new-directory)
    (unless (uiop:pathname-equal *previous-cwd* previous-directory)
      (setf *previous-cwd* previous-directory))
    new-directory))

(defun expand-filename-modifiers (string &optional (base-filename (lem:buffer-filename)))
  (ppcre:regex-replace-all "%(?::[a-z])*"
                           string
                           (lambda (match &rest registers)
                             (declare (ignore registers))
                             (let ((result (enough-namestring (or base-filename
                                                                  (lem:buffer-filename)
                                                                  (uiop:getcwd))
                                                              (uiop:getcwd))))
                               (ppcre:do-matches-as-strings (flag "(?<=:)([a-z])" match result)
                                 (setf result
                                       (ecase (aref flag 0)
                                         (#\p (namestring
                                               (uiop:ensure-absolute-pathname result (uiop:getcwd))))
                                         (#\h
                                          (namestring
                                           (if (uiop:directory-pathname-p result)
                                               (uiop:pathname-parent-directory-pathname result)
                                               (uiop:pathname-directory-pathname result))))
                                         (#\t
                                          (let ((result-path (pathname result)))
                                            (namestring
                                             (make-pathname :name (pathname-name result-path)
                                                            :type (pathname-type result-path)))))
                                         (#\r
                                          (make-pathname :defaults (pathname result)
                                                         :type nil))
                                         (#\e (or (pathname-type (pathname result))
                                                  "")))))))
                           :simple-calls t))

(defun kill-region-without-appending (start end)
  "Same as lem:kill-region except this won't append to the existing killring"
  (when (point< end start)
    (rotatef start end))
  (let ((killed-string (delete-character start (count-characters start end))))
    (copy-to-clipboard-with-killring killed-string)))

(defpackage :lem-vi-mode/options
  (:use :cl)
  (:import-from :lem-vi-mode/core
                :change-directory)
  (:import-from :parse-number
                :parse-number)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :alexandria
                :if-let
                :when-let
                :once-only
                :with-gensyms)
  (:export :define-vi-option
           :get-option
           :vi-option
           :vi-option-name
           :vi-option-value
           :vi-option-default
           :vi-option-type
           :vi-option-aliases
           :vi-option-getter
           :vi-option-set-hook
           :vi-option-documentation
           :reset-vi-option-value
           :toggle-vi-option-value
           :execute-set-command

           ;; Options
           :autochdir))
(in-package :lem-vi-mode/options)

(defstruct vi-option
  (name nil :type string)
  %value
  default
  (type t :type (member t boolean number string))
  (aliases '() :type list)
  (getter nil :type (or function null))
  (set-hook nil :type (or function null))
  (documentation nil :type (or string null)))

(defvar *options* (make-hash-table :test 'equal))
(defvar *option-aliases* (make-hash-table :test 'equal))

(defun canonical-option-name (name)
  (or (gethash name *option-aliases*)
      name))

(defun get-option (name &optional (error-if-not-exists t))
  (check-type name string)
  (let ((name (canonical-option-name name)))
    (multiple-value-bind (option exists)
        (gethash name *options*)
      (when (and (null exists)
                 error-if-not-exists)
        (lem:editor-error "Unknown option: ~A" name))
      option)))

(defun vi-option-value (option)
  (values
   (if-let (getter (vi-option-getter option))
     (funcall getter)
     (vi-option-%value option))
   (vi-option-name option)))

(defun (setf vi-option-value) (new-value option)
  (with-slots (type set-hook) option
    (unless (typep new-value type)
      (lem:editor-error "Option '~A' accepts only ~S, but given ~S"
                        (vi-option-name option) type new-value))
    (let ((old-value (vi-option-value option)))
      (multiple-value-prog1
          (values
           (setf (vi-option-%value option) new-value)
           (vi-option-name option)
           old-value
           t)
        (when set-hook
          (funcall set-hook new-value))))))

(defun reset-option-value (option)
  (setf (vi-option-value option)
        (vi-option-default option)))

(defun toggle-option-value (option)
  (with-slots (name type) option
    (unless (eq type 'boolean)
      (lem:editor-error "Can't toggle non-boolean option: '~A' (type=~S)" name type)))
  (setf (vi-option-value option)
        (not (vi-option-value option))))

(defun parse-option-string (option-string)
  (coerce
   (nth-value 1
              (ppcre:scan-to-strings "^(no|inv)?([^?!&=:]+)(\\?|\\!|&|=|:)?(.+)?$" option-string))
   'list))

(defun execute-set-command (option-string)
  (destructuring-bind (prefix option-name suffix new-value)
      (parse-option-string option-string)
    (let ((option (get-option option-name)))
      (cond
        ((equal suffix "?")
         (vi-option-value option))
        ((equal prefix "no")
         (setf (vi-option-value option) nil))
        ((or (equal prefix "inv")
             (equal suffix "!"))
         (toggle-option-value option))
        ((equal suffix "&")
         (reset-option-value option))
        ((member suffix '("=" ":") :test 'equal)
         (setf (vi-option-value option)
               (case (vi-option-type option)
                 (boolean
                  (cond
                    ((string-equal new-value "t") t)
                    ((string-equal new-value "nil") nil)
                    (t new-value)))
                 (number
                  (handler-case (parse-number new-value)
                    (error () new-value)))
                 (string
                  new-value)
                 (otherwise new-value))))
        (t
         (assert (and (null prefix)
                      (null suffix)))
         (setf (vi-option-value option) t))))))

(defmacro define-vi-option (name (default &key (type t) aliases) &rest others)
  (once-only (default)
    (with-gensyms (option alias new-value)
      (let ((name-str (string-downcase name)))
        `(progn
           (check-type ,default ,type)
           (dolist (,alias ',aliases)
             (setf (gethash ,alias *option-aliases*) ,name-str))
           (let ((,option
                   (make-vi-option :name ,name-str
                                   :%value ,default
                                   :default ,default
                                   :type ',type
                                   :aliases ',aliases
                                   :getter ,(when-let (getter-arg (find :getter others :key #'car))
                                              `(lambda () ,@(rest getter-arg)))
                                   :set-hook ,(when-let (set-hook-arg (find :set-hook others :key #'car))
                                                `(lambda ,@(rest set-hook-arg)))
                                   :documentation ,(when-let (doc-arg (find :documentation others :key #'car))
                                                     (second doc-arg)))))
             (setf (gethash ,name-str *options*) ,option)
             (defun ,name ()
               (vi-option-value (get-option ,name-str)))
             (defun (setf ,name) (,new-value)
               (setf (vi-option-value (get-option ,name-str)) ,new-value))))))))

(defun auto-change-directory (buffer-or-window)
  (change-directory (etypecase buffer-or-window
                      (lem:buffer (lem:buffer-directory buffer-or-window))
                      (lem:window (lem:buffer-directory (lem:window-buffer buffer-or-window))))))

(define-vi-option autochdir (nil :type boolean :aliases ("acd"))
  (:documentation "A flag on whether change the current directory to the buffer directory automatically.
  Default: nil
  Aliases: acd")
  (:set-hook (new-value)
   (if new-value
       (progn
         (lem:add-hook lem:*find-file-hook* 'auto-change-directory)
         (dolist (window (lem:window-list))
           (lem:add-hook (lem-core::window-switch-to-buffer-hook window) 'auto-change-directory)
           (lem:add-hook (lem-core:window-leave-hook window) 'auto-change-directory)))
       (progn
         (lem:remove-hook lem:*find-file-hook* 'auto-change-directory)
         (dolist (window (lem:window-list))
           (lem:remove-hook (lem-core::window-switch-to-buffer-hook window) 'auto-change-directory)
           (lem:remove-hook (lem-core:window-leave-hook window) 'auto-change-directory))))))

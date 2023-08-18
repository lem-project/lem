(defpackage :lem-vi-mode/options
  (:use :cl
        :split-sequence)
  (:import-from :lem-vi-mode/core
                :change-directory)
  (:import-from :parse-number
                :parse-number)
  (:import-from :cl-ppcre
                :scan-to-strings
                :register-groups-bind)
  (:import-from :alexandria
                :if-let
                :when-let
                :once-only
                :with-gensyms
                :disjoin)
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
           :execute-set-command))
(in-package :lem-vi-mode/options)

(defstruct vi-option
  (name nil :type string)
  %value
  default
  (type t :type (member t boolean number string list))
  (aliases '() :type list)
  (getter nil :type (or function null))
  (set-hook nil :type (or function null))
  (documentation nil :type (or string null)))

(define-condition vi-option-error (simple-error) ())

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

(defun ensure-option (name-or-option &optional (error-if-not-exists t))
  (etypecase name-or-option
    (vi-option name-or-option)
    (string (get-option name-or-option error-if-not-exists))))

(defun vi-option-value (option)
  (let ((option (ensure-option option)))
    (values
     (if-let (getter (vi-option-getter option))
       (funcall getter)
       (vi-option-%value option))
     (vi-option-name option))))

(defun (setf vi-option-value) (new-value option)
  (let ((option (ensure-option option)))
    (with-slots (type set-hook) option
      (unless (typep new-value type)
        (lem:editor-error "Option '~A' accepts only ~S, but given ~S"
                          (vi-option-name option) type new-value))
      (let ((old-value (vi-option-value option)))
        (handler-case
            (progn
              (when set-hook
                (funcall set-hook new-value))
              (setf (vi-option-%value option) new-value)
              (values (vi-option-value option)
                      (vi-option-name option)
                      old-value
                      t))
          (vi-option-error (e)
            (lem:editor-error (princ-to-string e))))))))

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
              (ppcre:scan-to-strings "^(no|inv)?([^?!&=:\\+\\-\\^]+)(\\?|\\!|&|(?:\\+|\\-|\\^)?=|:)?(.+)?$" option-string))
   'list))

(defun execute-set-command (option-string)
  (destructuring-bind (&optional prefix option-name suffix new-value)
      (parse-option-string option-string)
    (unless option-name
      (lem:editor-error "Unknown option: ~A" option-string)
      (return-from execute-set-command nil))
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
                 (list
                  (check-type new-value string)
                  (split-sequence #\, new-value))
                 (otherwise new-value))))
        ((member suffix '("+=" "^=") :test 'equal)
         (ecase (vi-option-type option)
           (list
            (let ((current-value (vi-option-value option)))
              (if (member new-value current-value
                          :test 'equal)
                  (vi-option-value option)
                  (setf (vi-option-value option)
                        (if (string= suffix "+=")
                            (append current-value (list new-value))
                            (cons new-value current-value))))))
           (string
            (setf (vi-option-value option)
                  (if (string= suffix "+=")
                      (concatenate 'string
                                   (vi-option-value option)
                                   new-value)
                      (concatenate 'string
                                   new-value
                                   (vi-option-value option)))))
           (number
            (setf (vi-option-value option)
                  (funcall (if (string= suffix "+=")
                               #'+
                               #'*)
                           (vi-option-value option)
                           new-value)))
           (boolean
            (lem:editor-error "Can't ~A a boolean option: ~A"
                              (if (string= suffix "+=")
                                  "increment"
                                  "multiply")
                              (vi-option-name option)))))
        ((string= suffix "-=")
         (ecase (vi-option-type option)
           (list
            (setf (vi-option-value option)
                  (remove new-value (vi-option-value option)
                          :test 'equal)))
           (string
            (lem:editor-error "Can't subtract a string option: ~A"
                              (vi-option-name option)))
           (number
            (decf (vi-option-value option) new-value))
           (boolean
            (lem:editor-error "Can't decrement a boolean option: ~A"
                              (vi-option-name option)))))
        (t
         (assert (and (null prefix) (null suffix)))
         (if (eq (vi-option-type option) 'boolean)
             (setf (vi-option-value option) t)
             ;; Show the current value for other than boolean
             (vi-option-value option)))))))

(defmacro define-vi-option (name (default &key (type t) aliases) &rest others)
  (check-type name string)
  (once-only (default)
    (with-gensyms (option alias)
      `(progn
         (check-type ,default ,type)
         (dolist (,alias ',aliases)
           (setf (gethash ,alias *option-aliases*) ,name))
         (let ((,option
                 (make-vi-option :name ,name
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
           (setf (gethash ,name *options*) ,option))))))

(defun auto-change-directory (buffer-or-window)
  (change-directory (etypecase buffer-or-window
                      (lem:buffer (lem:buffer-directory buffer-or-window))
                      (lem:window (lem:buffer-directory (lem:window-buffer buffer-or-window))))))

(define-vi-option "autochdir" (nil :type boolean :aliases ("acd"))
  (:documentation "Boolean to change the current directory to the buffer's directory automatically.
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

(define-vi-option "number" (nil :type boolean :aliases ("nu"))
  (:documentation "Boolean to show the line number.
  Default: nil
  Aliases: nu")
  (:getter (lem:variable-value 'lem/line-numbers:line-numbers :global))
  (:set-hook (new-value)
   (setf (lem:variable-value 'lem/line-numbers:line-numbers :global) new-value)))

(defvar *default-iskeyword* '("@" "48-57" "_" "192-255"))

(defun compile-iskeyword (value)
  (apply #'disjoin
         (mapcar (lambda (rule)
                   (check-type rule string)
                   (cond
                     ((string= rule "@")
                      #'alpha-char-p)
                     ((string= rule "@-@")
                      (lambda (c)
                        (char= c #\@)))
                     (t
                      (or (ppcre:register-groups-bind ((#'parse-integer start) (#'parse-integer end))
                              ("(\\d{2,})-(\\d{2,})" rule)
                            (lambda (c)
                              (<= start (char-code c) end)))
                          (ppcre:register-groups-bind (start end)
                              ("(.)-(.)" rule)
                            (let ((start-code (char-code (aref start 0)))
                                  (end-code (char-code (aref end 0))))
                              (lambda (c)
                                (<= start-code (char-code c) end-code))))
                          (progn
                            (unless (= (length rule) 1)
                              (error 'vi-option-error
                                     :format-control "Invalid rule in iskeyword: ~A"
                                     :format-arguments (list rule)))
                            (let ((rule-char (aref rule 0)))
                              (lambda (c)
                                (char= c rule-char))))))))
                 value)))

(define-vi-option "iskeyword" (*default-iskeyword* :type list :aliases ("isk"))
  (:documentation "Comma-separated string to specify the characters should be recognized as a keyword. (buffer local)
  Default: @,48-57,_,192-255
  Aliases: isk")
  (:getter
   (symbol-macrolet ((vi-iskeyword
                       (gethash "vi-iskeyword" (lem-base::buffer-variables (lem:current-buffer)))))
     (car
      (or vi-iskeyword
          (setf vi-iskeyword
                (cons *default-iskeyword*
                      (compile-iskeyword *default-iskeyword*)))))))
  (:set-hook (new-value)
   (setf (gethash "vi-iskeyword" (lem-base::buffer-variables (lem:current-buffer)))
         (cons new-value
               (compile-iskeyword new-value)))))

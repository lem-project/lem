(in-package :transient)

(defmethod keymap-activate ((keymap keymap))
  (log:info "keymap ~A activated" keymap)
  (show-transient keymap))

(defmacro add-dynamic-property (class-name properties-accessor property-name &optional default-value)
  "define <CLASS-NAME>-<PROPERTY-NAME> getter and setter methods.

the getter retrieves from PROPERTIES-ACCESSOR using :PROPERTY-NAME as key.
if the value is a function, it funcalls it. the setter stores directly.
if DEFAULT-VALUE is provided and non-nil, it is used as the default for getf."
  (let* ((keyword (intern (symbol-name property-name) :keyword))
         (getter-name (intern (format nil "~A-~A" class-name property-name)))
         (obj-sym (gensym "OBJ")))
    `(progn
       (defmethod ,getter-name ((,obj-sym ,class-name))
         (let ((prop ,(if default-value
                          `(getf (,properties-accessor ,obj-sym) ,keyword ,default-value)
                          `(getf (,properties-accessor ,obj-sym) ,keyword))))
           (if (functionp prop)
               (funcall prop)
               prop)))
       (defmethod (setf ,getter-name) (val (,obj-sym ,class-name))
         (setf (getf (,properties-accessor ,obj-sym) ,keyword) val)))))

(add-dynamic-property keymap keymap-properties show-p t)
(add-dynamic-property prefix prefix-properties show-p t)

(defgeneric prefix-render (prefix)
  (:documentation "render prefix into a layout item. returns nil to use default rendering."))

;; should return :row or :column
(defmethod keymap-display-style ((keymap keymap))
  (getf (keymap-properties keymap) :display-style :row))

(defmethod (setf keymap-display-style) (val (keymap keymap))
  (setf (getf (keymap-properties keymap) :display-style) val))

(defclass choice (prefix)
  ((choices
    :accessor prefix-choices)))

(defmacro define-transient (name &body bindings)
  `(defparameter ,name (parse-transient ',bindings)))

(defun parse-transient (bindings)
  (let ((keymap (make-keymap)))
    (loop for tail = bindings then (cdr tail)
          while tail
          do (let ((binding (car tail)))
               (cond
                 ;; inline property
                 ((keywordp binding)
                  (let ((val (second tail)))
                    (setf (getf (keymap-properties keymap) binding) val))
                  ;; advance another cell because we're already consumed it (second tail)
                  (setf tail (cdr tail)))
                 ;; direct child keymap (:keymap ...)
                 ((eq (car binding) :keymap)
                  (let ((sub-map (parse-transient (cdr binding))))
                    (keymap-add-child keymap sub-map t)))
                 ;; key binding (:key ...)
                 ((eq (car binding) :key)
                  (let* ((key (second binding))
                         ;; prefix-class depends on the first cell in the :suffix value (if its a list at all)
                         (prefix-type (intern (symbol-name (getf binding :type 'prefix))))
                         (prefix (make-instance prefix-type)))
                    (setf (prefix-key prefix) (car (parse-keyspec key)))
                    ;; sometimes the suffix will not be set (e.g. prefix-type is :choice). we
                    ;; initialize it to nil to avoid unbound errors.
                    (setf (prefix-suffix prefix) nil)
                    (loop for (key value) on (cddr binding) by 'cddr
                          ;; key-method is used for (setf prefix-<key-method> <value>)
                          for key-method = (intern (format nil "PREFIX-~A" (string key)))
                          do (let ((setf-expr `(setf (,key-method prefix) value))
                                   (final-value)
                                   (should-set t))
                               (cond
                                 ;; if the suffix is a keymap we need to parse recursively
                                 ((and (listp value) (eq (car value) :keymap))
                                  (setf final-value (parse-transient value)))
                                 ((eq key :type)
                                  (setf should-set nil))
                                 (t
                                  (setf final-value value)))
                               (when should-set
                                 (funcall (fdefinition (list 'setf key-method))
                                          final-value
                                          prefix))))
                    (keymap-add-prefix keymap prefix t))))))
    keymap))
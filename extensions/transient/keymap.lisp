(in-package :lem/transient)

(defmethod keymap-activate ((keymap keymap))
  "called when a keymap is activated by the event scheduler."
  (show-transient keymap))

(defmacro add-dynamic-property (class-name properties-accessor property-name &optional default-value)
  "define <CLASS-NAME>-<PROPERTY-NAME> getter and setter methods.

the getter retrieves from PROPERTIES-ACCESSOR using :PROPERTY-NAME as key.
if the value is a function, it funcalls it. the setter stores directly.
if DEFAULT-VALUE is provided and non-nil, it is used as the default for getf."
  (let* ((keyword (intern (symbol-name property-name) :keyword))
         (getter-name (intern (format nil "~A-~A" class-name property-name) :lem/transient))
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

(defmacro add-static-property (class-name properties-accessor property-name &optional default-value)
  "define <CLASS-NAME>-<PROPERTY-NAME> getter and setter methods.

the getter retrieves from PROPERTIES-ACCESSOR using :PROPERTY-NAME as key.
the setter stores directly."
  (let* ((keyword (intern (symbol-name property-name) :keyword))
         (getter-name (intern (format nil "~A-~A" class-name property-name) :lem/transient))
         (obj-sym (gensym "OBJ")))
    `(progn
       (defmethod ,getter-name ((,obj-sym ,class-name))
         ,(if default-value
              `(getf (,properties-accessor ,obj-sym) ,keyword ,default-value)
              `(getf (,properties-accessor ,obj-sym) ,keyword)))
       (defmethod (setf ,getter-name) (val (,obj-sym ,class-name))
         (setf (getf (,properties-accessor ,obj-sym) ,keyword) val)))))

;; these are properties that we want to be "dynamic", as in can be assigned a function that
;; returns the value later instead of the value itself.
(add-dynamic-property keymap keymap-properties show-p t)
(add-dynamic-property prefix prefix-properties show-p t)
;; static properties dont take a function that returns a value, just a value.
(add-static-property keymap keymap-properties display-style :row)
(add-static-property prefix prefix-properties id)
;; TODO: it would be better to store the parsed key sequence instead of the stringified one and work with that.
(add-static-property prefix prefix-properties display-key)

(defun find-prefix-by-id (keymap id)
  (labels ((f (node)
             (cond ((typep node 'keymap)
                    (dolist (child (keymap-children node))
                      (let ((res (f child)))
                        (when res (return-from f res)))))
                   ((typep node 'prefix)
                    (if (eql (prefix-id node) id)
                        node
                        (let ((suffix (prefix-suffix node)))
                          (when (or (typep suffix 'keymap)
                                    (typep suffix 'prefix))
                            (f suffix))))))))
    (f keymap)))

(defclass infix (prefix)
  ())

(defclass choice (infix)
  ((choices
    :accessor prefix-choices)
   (value))
  (:documentation "a prefix that may take on different values."))

(defclass toggle (infix)
  ((value :initform nil))
  (:documentation "a boolean infix."))

(defmethod prefix-value ((prefix prefix))
  (slot-value prefix 'value))

(defmethod prefix-value ((prefix choice))
  (if (slot-boundp prefix 'value)
      (slot-value prefix 'value)
      (car (prefix-choices prefix))))

(defmethod (setf prefix-value) (new-value (prefix prefix))
  (setf (slot-value prefix 'value) new-value))

;; infixes dont modify the keymap menu, we drop the key and dont append it to the recorded keyseq
(defmethod prefix-behavior ((prefix infix))
  :drop)

;; this one applies the next value from the choices list without a prompt
;; (defmethod prefix-suffix ((choice choice))
;;   (labels ((suffix ()
;;              (let* ((choices (prefix-choices choice))
;;                     (current-value (prefix-value choice))
;;                     (position (position current-value choices :test 'equal)))
;;                (let ((new-value (if position
;;                                     ;; mod is to wrap around to 0. :D
;;                                     (elt choices (mod (1+ position) (length choices)))
;;                                     (first choices))))
;;                  (log:info "switching to value ~A~%" new-value)
;;                  (setf (prefix-value choice) new-value)))))
;;     #'suffix))

(defmethod prefix-suffix ((choice choice))
  (labels ((suffix ()
             (let* ((choices (prefix-choices choice))
                    (current-value (prefix-value choice))
                    (new-value))
               (with-last-read-key-sequence
                   (setf new-value
                         (prompt-for-string "new value: "
                                            :initial-value current-value
                                            :completion-function (lambda (x)
                                                                   choices))))
               (when new-value
                 (log:info "switching to value ~A~%" new-value)
                 (setf (prefix-value choice) new-value)))))
    #'suffix))

(defmethod prefix-suffix ((prefix toggle))
  (labels ((suffix ()
             (setf (prefix-value prefix) (not (prefix-value prefix)))))
    #'suffix))

(defmacro define-transient (name &body bindings)
  `(defparameter ,name (parse-transient ',bindings)))

(defun parse-transient (bindings)
  "defines a transient menu. args yet to be documented."
  (let ((keymap (make-keymap)))
    (loop for tail = bindings then (cdr tail)
          while tail
          do (let ((binding (car tail)))
               (cond
                  ;; inline property
                  ((keywordp binding)
                   (let ((val (second tail)))
                     (let ((key-method (intern (format nil "KEYMAP-~A" (string binding)) :lem/transient)))
                       (if (fboundp key-method)
                           (funcall (fdefinition (list 'setf key-method)) val keymap)
                           (setf (getf (keymap-properties keymap) binding) val))))
                   ;; advance another cell because we're already consumed it (second tail)
                   (setf tail (cdr tail)))
                  ;; direct child keymap (:keymap ...)
                  ((eq (car binding) :keymap)
                   (let ((sub-map (parse-transient (cdr binding))))
                     (keymap-add-child keymap sub-map t)))
                 ;; key binding (:key ...)
                 ((eq (car binding) :key)
                  (let* ((key (second binding))
                         (prefix-type (intern (symbol-name (getf binding :type 'prefix)) :lem/transient))
                         (prefix (make-instance prefix-type))
                         (last-keymap keymap))
                    (let ((parsed-key (parse-keyspec key)))
                      ;; store the full key string for multi-key bindings
                      (when (cdr parsed-key)
                        (setf (prefix-display-key prefix) key))
                      ;; we need to create intermediate prefixes if the key is longer than one
                      (loop for cell on parsed-key
                            for i from 0
                            for lastp = (null (cdr cell))
                            for current-key = (car cell)
                            for current-prefix = (if lastp
                                                     prefix
                                                     ;; reuse existing intermediate prefix with same key, or create new one
                                                     (let ((existing (find current-key (keymap-children last-keymap)
                                                                          :test (lambda (k child)
                                                                                  (and (typep child 'prefix)
                                                                                       (prefix-intermediate-p child)
                                                                                       (equal k (prefix-key child)))))))
                                                       (if existing
                                                           (progn
                                                             (setf last-keymap (prefix-suffix existing))
                                                             existing)
                                                           (let* ((new-prefix (make-instance 'prefix))
                                                                  (new-keymap (make-instance 'keymap*)))
                                                             (keymap-add-prefix last-keymap new-prefix t)
                                                             (setf (prefix-suffix new-prefix) new-keymap)
                                                             (setf (prefix-intermediate-p new-prefix) t)
                                                             (setf last-keymap new-keymap)
                                                             new-prefix))))
                            do (setf (prefix-key current-prefix) current-key))
                    (keymap-add-prefix last-keymap prefix t)
                    ;; sometimes the suffix will not be set (e.g. prefix-type is :choice). we
                    ;; initialize it to nil to avoid unbound errors.
                    (setf (prefix-suffix prefix) nil)
                    (loop for (key value) on (cddr binding) by 'cddr
                          ;; key-method is used for (setf prefix-<key-method> <value>)
                          for key-method = (intern (format nil "PREFIX-~A" (string key)) :lem/transient)
                          do (let ((setf-expr `(setf (,key-method prefix) value))
                                   (final-value)
                                   (should-set t))
                               (cond
                                 ;; if the suffix is a keymap we need to parse recursively
                                 ((and (listp value) (eq (car value) :keymap))
                                  (setf final-value (parse-transient (cdr value))))
                                 ((eq key :type)
                                  (setf should-set nil))
                                 (t
                                  (setf final-value value)))
                               (when should-set
                                 (funcall (fdefinition (list 'setf key-method))
                                          final-value
                                          prefix))))))))))
    keymap))
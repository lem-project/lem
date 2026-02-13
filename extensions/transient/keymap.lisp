(in-package :lem/transient)

(defmethod keymap-activate ((keymap keymap))
  "called when a keymap is activated by the event scheduler."
  (let ((active-modes (all-active-modes (current-buffer))))
    (cond ((loop for mode in active-modes
                 for mode-keymap = (mode-transient-keymap mode)
                 when mode-keymap
                   do (show-transient
                       (if (keymap-contains-p mode-keymap keymap)
                           keymap
                           mode-keymap))
                      (return t)))
          ((or (keymap-show-p keymap) *transient-always-show*)
           (show-transient keymap))
          (t
           (hide-transient)))))

(defgeneric mode-transient-keymap (mode)
  (:documentation "returns the keymap to be passed to show-transient.")
  (:method ((mode mode))
    nil))

(defmacro add-property (class-name properties-accessor property-name &optional default-value)
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

;; some stuff we need for working with "transient keymaps"
(add-property keymap keymap-properties show-p nil)
(add-property keymap keymap-properties display-style :row)
(add-property prefix prefix-properties show-p t)
(add-property prefix prefix-properties id)
;; TODO: it would be better to store the parsed key sequence instead of the stringified one and work with that.
(add-property prefix prefix-properties display-key)

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

(defun keymap-contains-p (keymap target)
  "return T if KEYMAP contains TARGET as a direct or indirect child."
  (labels ((f (node)
             (cond ((eq node target) t)
                   ((typep node 'keymap)
                    (dolist (child (keymap-children node))
                      (when (f child) (return t))))
                   ((typep node 'prefix)
                    (let ((suffix (prefix-suffix node)))
                      (when (or (typep suffix 'keymap)
                                (typep suffix 'prefix))
                        (f suffix)))))))
    (f keymap)))

(defclass infix (prefix)
  ((variable
    :accessor infix-variable
    :initarg :variable
    :initform nil)))

(defclass choice (infix)
  ((choices
    :accessor prefix-choices)
   (value))
  (:documentation "a prefix that may take on different values."))

(defclass toggle (infix)
  ((value :initform nil))
  (:documentation "a boolean infix."))

(defmethod prefix-value ((prefix prefix))
  (let ((var (infix-variable prefix)))
    (if var
        (symbol-value var)
        (slot-value prefix 'value))))

(defmethod prefix-value ((prefix choice))
  (let ((var (infix-variable prefix)))
    (if var
        (symbol-value var)
        (if (slot-boundp prefix 'value)
            (slot-value prefix 'value)
            (car (prefix-choices prefix))))))

(defmethod (setf prefix-value) (new-value (prefix prefix))
  (let ((var (infix-variable prefix)))
    (if var
        (setf (symbol-value var) new-value)
        (setf (slot-value prefix 'value) new-value))))

;; infixes dont modify the keymap menu, we drop the key and dont append it to the recorded keyseq
(defmethod prefix-behavior ((prefix infix))
  :drop)

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
                 (setf (prefix-value choice) new-value)))))
    #'suffix))

(defmethod prefix-suffix ((prefix toggle))
  (labels ((suffix ()
             (setf (prefix-value prefix) (not (prefix-value prefix)))))
    #'suffix))

(defmacro define-transient (name &body bindings)
  `(defparameter ,name (parse-transient ',bindings)))

(defun parse-transient-method (object key val method-name)
  (let* ((key-string (string key))
         (key-method (intern (format nil "~A-~A" method-name key-string) :lem/transient))
         (length (length key-string)))
    (cond ((and (> length 5)
                (string-equal "-func" (subseq key-string (- length 5))))
           (let* ((prefix-key-string (subseq key-string 0 (- length 5)))
                  (key-method (intern (format nil "~A-~A" method-name prefix-key-string)
                                      :lem/transient)))
             (eval `(defmethod ,key-method ((object (eql ,object)))
                      ,val))))
          ((fboundp key-method)
           (funcall (fdefinition (list 'setf key-method)) val object))
          (t
           (let ((property-method (intern (format nil "~A-PROPERTIES" method-name)
                                          :lem/transient)))
             (when (fboundp property-method)
               (let ((props (funcall (fdefinition property-method) object)))
                 (setf (getf props key) val)
                 (funcall (fdefinition (list 'setf property-method)) props object))))))))

(defun parse-transient (bindings)
  "defines a transient menu. args yet to be documented."
  (let ((keymap (make-keymap)))
    (setf (keymap-show-p keymap) t)
    (loop for tail = bindings then (cdr tail)
          while tail
          do (let ((binding (car tail)))
               (cond
                 ;; inline property
                 ((keywordp binding)
                  (let ((val (second tail)))
                    (parse-transient-method keymap binding val "KEYMAP")
                    ;; advance another cell because we're already consumed it (second tail)
                    (setf tail (cdr tail))))
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
                      (loop
                        for cell on parsed-key
                        for i from 0
                        for lastp = (null (cdr cell))
                        for current-key = (car cell)
                        do (let ((current-prefix
                                   (if lastp
                                       prefix
                                       ;; reuse existing intermediate prefix with same key, or create new one
                                       (let ((existing (find
                                                        current-key
                                                        (keymap-children last-keymap)
                                                        :test (lambda (k child)
                                                                (and (typep child 'prefix)
                                                                     (prefix-intermediate-p child)
                                                                     (equal
                                                                      k
                                                                      (prefix-key child)))))))
                                         (if existing
                                             (progn
                                               (setf last-keymap (prefix-suffix existing))
                                               existing)
                                             (let* ((new-prefix (make-instance 'prefix))
                                                    (new-keymap (make-keymap)))
                                               (keymap-add-prefix last-keymap new-prefix t)
                                               (setf (prefix-suffix new-prefix) new-keymap)
                                               (setf (prefix-intermediate-p new-prefix) t)
                                               (setf (keymap-show-p new-keymap) t)
                                               (setf last-keymap new-keymap)
                                               new-prefix))))))
                             (setf (prefix-key current-prefix) current-key)))
                      (keymap-add-prefix last-keymap prefix t)
                      ;; sometimes the suffix will not be set (e.g. prefix-type is :choice). we
                      ;; initialize it to nil to avoid unbound errors.
                      (setf (prefix-suffix prefix) nil)
                      (loop for (key value) on (cddr binding) by 'cddr
                            do (let ((final-value)
                                     (should-set t))
                                 (cond
                                   ;; if the suffix is a keymap we need to parse recursively
                                   ((and (listp value) (eq (car value) :keymap))
                                    (setf final-value (parse-transient (cdr value))))
                                   ;; variable syncing: set the variable slot on the infix
                                   ;; we need a special case for it since its "infix-variable" and
                                   ;; not "prefix-variable" since its a slot in the infix class.
                                   ;; there's probably a nicer way to go about things but this is
                                   ;; just for 'parse-transient' which is designed as a
                                   ;; convenience anyway.
                                   ((eq key :variable)
                                    (setf (infix-variable prefix) value)
                                    (setf should-set nil))
                                   ((eq key :type)
                                    (setf should-set nil))
                                   (t
                                    (setf final-value value)))
                                 (when should-set
                                   (parse-transient-method prefix
                                                           key
                                                           final-value
                                                           "PREFIX"))))))))))
    keymap))
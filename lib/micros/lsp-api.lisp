(defpackage :micros/lsp-api
  (:use :cl)
  (:export :hover-symbol
           :completions
           :make-symbol-spec
           :symbol-informations
           :load-systems
           :compile-and-load-file))
(in-package :micros/lsp-api)

;;; hover-symbol
(defun describe-variable (symbol)
  (list "Variable"
        (with-output-to-string (stream)
          (when (boundp symbol)
            (sb-impl::describe-variable symbol stream)))))

(defun describe-function (symbol)
  (list "Function"
        (with-output-to-string (stream)
          (when (fboundp symbol)
            (let ((arglist (cons symbol (micros/backend:arglist symbol))))
              (write-line "```lisp" stream)
              (let ((*print-case* :downcase))
                (format stream "~(~A~)~%" arglist))
              (write-line "```" stream)
              (let ((doc (documentation symbol 'function)))
                (when doc
                  (write-line doc stream))))))))

(defun describe-class (symbol)
  (list "Class"
        (with-output-to-string (stream)
          (sb-impl::describe-class symbol nil stream))))

(defun describe-type (symbol)
  (list "Type"
        (with-output-to-string (stream)
          (sb-impl::describe-type symbol stream))))

(defun describe-declaration (symbol)
  (list "Declaration"
        (with-output-to-string (stream)
          (sb-impl::describe-declaration symbol stream))))

(defun describe-plist (symbol)
  (list "Symbol-plist:"
        (with-output-to-string (stream)
          (let ((plist (symbol-plist symbol)))
            (when plist
              (loop :for (k v) :on plist :by #'cddr
                    :do (format stream
                                "  ~@:_~A -> ~A~%"
                                (prin1-to-string k)
                                (prin1-to-string v))))))))

(defun describe-symbol-in-markdown (symbol)
  (string-right-trim '(#\newline #\space)
                     (with-output-to-string (stream)
                       (let ((contents
                               (remove ""
                                       (list (describe-variable symbol)
                                             (describe-function symbol)
                                             (describe-class symbol)
                                             (describe-type symbol)
                                             (describe-declaration symbol)
                                             (describe-plist symbol))
                                       :key #'second
                                       :test #'string=)))
                         (loop :for (header body) :in contents
                               :for first := t :then nil
                               :do (unless first
                                     (write-line "----------" stream))
                                   (format stream "## ~A~%" header)
                                   (write-string body stream))))))

(defun hover-symbol (symbol-name)
  (micros::with-buffer-syntax ()
    (multiple-value-bind (symbol status)
        (micros::parse-symbol symbol-name)
      (when status
        (describe-symbol-in-markdown symbol)))))

;;; completions
(defstruct (completed-item (:type list))
  label
  classification
  signature
  documentation
  sort-text)

(defun parse-classification-string (classification-string)
  (loop :for classification :in '(:variable
                                  :function
                                  :generic-function
                                  :type
                                  :class
                                  :macro
                                  :special-operator
                                  :package)
        :for i :from 0
        :unless (char= #\- (char classification-string i))
        :collect classification))

(defun symbol-signature (symbol)
  (let ((*print-case* :downcase))
    (handler-case
        (princ-to-string (micros::arglist symbol))
      (error ()
        nil))))

(defun completed-string-to-symbol (completed-string default-package-name)
  (multiple-value-bind (symbol-name package-name internalp)
      (micros::tokenize-symbol-thoroughly completed-string)
    (declare (ignore internalp))
    (let ((package (if (null package-name)
                       (find-package default-package-name)
                       (find-package package-name))))
      (when package
        (find-symbol symbol-name package)))))

(defun completions (symbol-string package-name)
  (destructuring-bind (completions timeout-p)
      (micros:fuzzy-completions symbol-string package-name
                                :limit 100)
    (declare (ignore timeout-p))
    (loop :for (completed-string score chunks classification-string) :in completions
          :for classification-detail := (format nil "~(~{~A~^, ~}~)"
                                                (parse-classification-string classification-string))
          :for symbol := (completed-string-to-symbol completed-string package-name)
          :for signature := (symbol-signature symbol)
          :for documentation := (describe-symbol-in-markdown symbol)
          :for index :from 0
          :collect (make-completed-item :label completed-string
                                        :classification classification-detail
                                        :signature signature
                                        :documentation documentation
                                        :sort-text (format nil "~10,'0D" index)))))

;;; symbol-informations
(defstruct (symbol-information (:type list))
  name
  detail
  kind)

(defstruct (symbol-spec (:type list))
  name
  package)

(defun find-symbol* (symbol-name package-name)
  (let ((package (find-package package-name)))
    (when package
      (find-symbol symbol-name package))))

(defun symbol-kind (symbol)
  (cond ((boundp symbol)
         :variable)
        ((fboundp symbol)
         :function)
        ((find-class symbol nil)
         :class)
        ((find-package symbol)
         :package)))

(defun symbol-informations (symbol-specs)
  (loop :for (symbol-name package-name) :in symbol-specs
        :collect (let ((symbol (find-symbol* symbol-name package-name)))
                   (make-symbol-information :name symbol-name
                                            :detail (when symbol (symbol-signature symbol))
                                            :kind (when symbol (symbol-kind symbol))))))

;;;
(defun load-systems (system-names)
  (ql:quickload system-names))

(defun compile-and-load-file (filename)
  (uiop:with-temporary-file (:pathname output-file :type "fasl")
    (let* ((stream (make-broadcast-stream))
           (*standard-output* stream)
           (*error-output* stream))
      (when (uiop:compile-file* filename :output-file output-file)
        (load output-file)
        t))))

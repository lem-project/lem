(defpackage :lem-clojure-mode/detective
  (:use :cl :lem :lem/detective)
  (:import-from :lem-clojure-mode
                :clojure-mode)
  (:export :capture-reference
           :clojure-detective-search))

(in-package :lem-clojure-mode/detective)

;;;; Clojure Detective Integration
;;;;
;;;; Provides code navigation for Clojure definitions:
;;;; - defn, defn-, defmulti, defmethod (functions)
;;;; - defmacro (macros)
;;;; - defprotocol, defrecord, deftype (classes/types)
;;;; - def, defonce (variables)
;;;; - ns (packages/namespaces)
;;;; - deftest (misc - tests)

;;;; Capture Reference Methods

(defun parse-clojure-name (line)
  "Extract the name from a Clojure definition line."
  (let ((parts (ppcre:split "\\s+" (string-trim '(#\space #\tab) line))))
    (when (>= (length parts) 2)
      (let ((name (second parts)))
        ;; Remove trailing parentheses or brackets
        (string-trim '(#\( #\) #\[ #\] #\{ #\}) name)))))

(defmethod capture-reference ((position lem:point) (class (eql :function-reference)))
  "Capture Clojure function definitions (defn, defn-, defmulti, defmethod)."
  (let* ((line (line-string position))
         (name (parse-clojure-name line)))
    (when name
      (make-instance 'lem/detective:function-reference
                     :reference-name name
                     :reference-point position))))

(defmethod capture-reference ((position lem:point) (class (eql :macro-reference)))
  "Capture Clojure macro definitions (defmacro)."
  (let* ((line (line-string position))
         (name (parse-clojure-name line)))
    (when name
      (make-instance 'lem/detective:macro-reference
                     :reference-name name
                     :reference-point position))))

(defmethod capture-reference ((position lem:point) (class (eql :class-reference)))
  "Capture Clojure type definitions (defprotocol, defrecord, deftype)."
  (let* ((line (line-string position))
         (name (parse-clojure-name line)))
    (when name
      (make-instance 'lem/detective:class-reference
                     :reference-name name
                     :reference-point position))))

(defmethod capture-reference ((position lem:point) (class (eql :variable-reference)))
  "Capture Clojure variable definitions (def, defonce)."
  (let* ((line (line-string position))
         (name (parse-clojure-name line)))
    (when name
      (make-instance 'lem/detective:variable-reference
                     :reference-name name
                     :reference-point position))))

(defmethod capture-reference ((position lem:point) (class (eql :package-reference)))
  "Capture Clojure namespace definitions (ns)."
  (let* ((line (line-string position))
         (name (parse-clojure-name line)))
    (when name
      (make-instance 'lem/detective:package-reference
                     :reference-name name
                     :reference-point position))))

(defmethod capture-reference ((position lem:point) (class (eql :misc-reference)))
  "Capture misc Clojure definitions (deftest, etc.)."
  (ppcre:register-groups-bind (type name)
      ("^\\s*\\(\\s*(\\w+)\\s+([^\\s\\[\\(]+)" (line-string position))
    (when (and type name)
      (make-instance 'lem/detective:misc-reference
                     :misc-custom-type type
                     :reference-name (string-trim '(#\space #\tab #\)) name)
                     :reference-point position))))

;;;; Detective Search Configuration

(defun clojure-capture-function (point class)
  "Default capture function for Clojure."
  (capture-reference (copy-point point :temporary) class))

(defvar *clojure-detective-search*
  (make-instance 'search-regex
                 :function-regex
                 (make-capture-regex
                  :regex "^\\s*\\((?:defn-?|defmulti|defmethod)\\s"
                  :function #'clojure-capture-function)
                 :macro-regex
                 (make-capture-regex
                  :regex "^\\s*\\(defmacro\\s"
                  :function #'clojure-capture-function)
                 :class-regex
                 (make-capture-regex
                  :regex "^\\s*\\((?:defprotocol|defrecord|deftype|definterface)\\s"
                  :function #'clojure-capture-function)
                 :variable-regex
                 (make-capture-regex
                  :regex "^\\s*\\((?:def|defonce)\\s"
                  :function #'clojure-capture-function)
                 :package-regex
                 (make-capture-regex
                  :regex "^\\s*\\(ns\\s"
                  :function #'clojure-capture-function)
                 :misc-regex
                 (make-capture-regex
                  :regex "^\\s*\\((?:deftest|defspec)\\s"
                  :function #'clojure-capture-function)))

;;;; Mode Integration

(defun setup-clojure-detective ()
  "Setup detective for the current Clojure buffer."
  (setf (variable-value 'lem/language-mode:detective-search :buffer)
        *clojure-detective-search*))

;; Add to clojure-mode hook
(add-hook lem-clojure-mode:*clojure-mode-hook* 'setup-clojure-detective)

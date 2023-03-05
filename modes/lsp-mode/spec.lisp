(defpackage :lem-lsp-mode/spec
  (:use :cl)
  (:export :spec-language-id
           :spec-root-uri-patterns
           :spec-command
           :spec-install-command
           :spec-readme-url
           :spec-mode
           :spec-port
           :get-language-spec
           :register-language-spec
           :get-spec-command))
(in-package :lem-lsp-mode/spec)

(defclass spec ()
  ((language-id
    :initarg :language-id
    :initform (alexandria:required-argument :language-id)
    :reader spec-language-id)
   (root-uri-patterns
    :initarg :root-uri-patterns
    :initform nil
    :reader spec-root-uri-patterns)
   (command
    :initarg :command
    :initform nil
    :reader spec-command)
   (install-command
    :initarg :install-command
    :initform nil
    :reader spec-install-command)
   (readme-url
    :initarg :readme-url
    :initform nil
    :reader spec-readme-url)
   (mode
    :initarg :mode
    :initform (alexandria:required-argument :mode)
    :reader spec-mode)
   (port
    :initarg :port
    :initform nil
    :reader spec-port)))

(defun get-language-spec (major-mode)
  (let ((spec (get major-mode 'spec)))
    (assert (typep spec 'spec))
    spec))

(defun register-language-spec (major-mode spec)
  (check-type spec spec)
  (setf (get major-mode 'spec) spec))

(defun get-spec-command (spec &rest args)
  (let ((command (spec-command spec)))
    (if (functionp command)
        (apply command args)
        command)))

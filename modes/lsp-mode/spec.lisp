(defpackage :lem-lsp-mode/spec
  (:use :cl)
  (:export :spec-language-id
           :spec-root-uri-patterns
           :spec-command
           :spec-install-command
           :spec-readme-url
           :spec-mode
           :spec-port
           :spec-equal
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

(defun spec-equal (spec1 spec2)
  (equal (spec-language-id spec1)
         (spec-language-id spec2)))

(defun get-language-spec (major-mode)
  (make-instance (get major-mode 'spec)))

(defun register-language-spec (major-mode spec-name)
  (setf (get major-mode 'spec) spec-name))

(defun get-spec-command (spec &rest args)
  (let ((command (spec-command spec)))
    (if (functionp command)
        (apply command args)
        command)))

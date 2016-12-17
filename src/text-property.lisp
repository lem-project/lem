(defpackage lem.text-property
  (:use :cl)
  (:export :text-property
           :text-property-p
           :make-text-property
           :text-property-string
           :text-property-plist))

(in-package :lem.text-property)

(defstruct (text-property (:constructor %make-text-property))
  string
  plist)

(defun make-text-property (string &rest args)
  (%make-text-property :string string :plist args))

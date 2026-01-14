(defpackage :transient
  (:use :cl :lem)
  (:export))

(in-package :transient)

(defmethod keymap-activate ((keymap (eql *root-keymap*)))
  (log:info "activated root keymap~%"))

(defmethod keymap-activate ((keymap keymap))
  (log:info "activated ~A~%" keymap))
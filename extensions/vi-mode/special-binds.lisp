(defpackage :lem-vi-mode/special-binds
  (:use :cl)
  (:import-from :lem-vi-mode/core
                :mode-specific-keymaps)
  (:import-from :lem/directory-mode
                :directory-mode
                :*directory-mode-keymap*))
(in-package :lem-vi-mode/special-binds)

(defmethod mode-specific-keymaps ((mode directory-mode))
  (list *directory-mode-keymap*))

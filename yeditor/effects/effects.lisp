(defpackage :yeditor-effects
  (:use :cl)
  (:import-from :yeditor-effects/e-buffer    :install-buffer)
  (:import-from :yeditor-effects/e-mode      :install-mode)
  (:import-from :yeditor-effects/e-hook      :install-hook)
  (:import-from :yeditor-effects/e-keymap    :install-keybinding)
  (:import-from :yeditor-effects/e-variable  :install-variable)
  (:import-from :yeditor-effects/e-command   :install-command)
  (:import-from :yeditor-effects/e-overlay   :install-overlay)
  (:import-from :yeditor-effects/e-timer     :install-timer)
  (:export :install-buffer :install-mode :install-hook
           :install-keybinding :install-variable :install-command
           :install-overlay :install-timer))
(in-package :yeditor-effects)

(defpackage :yeditor-effects
  (:use :cl)
  (:import-from :yeditor-effects/e-buffer :install-buffer)
  (:import-from :yeditor-effects/e-mode :install-mode)
  (:import-from :yeditor-effects/e-hook :install-hook)
  (:export :install-buffer :install-mode :install-hook))
(in-package :yeditor-effects)

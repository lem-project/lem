(in-package :lem-generics)

(defgeneric global-mode-region-beginning (global-mode &optional buffer))

(defmethod global-mode-region-beginning ((global-mode lem-core::emacs-mode)
                                         &optional (buffer (current-buffer)))
  (region-beginning buffer))


(defgeneric global-mode-region-end (global-mode &optional buffer))

(defmethod global-mode-region-end ((global-mode lem-core::emacs-mode)
                                   &optional (buffer (current-buffer)))
  (region-end buffer))

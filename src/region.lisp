(in-package :lem-core)

(defgeneric global-mode-region-beginning (global-mode &optional buffer))
(defgeneric global-mode-region-end (global-mode &optional buffer))
(defgeneric set-region-point-global (global-mode start end))

(defmethod global-mode-region-beginning ((global-mode emacs-mode)
                                         &optional (buffer (current-buffer)))
  (region-beginning buffer))

(defmethod global-mode-region-end ((global-mode emacs-mode)
                                   &optional (buffer (current-buffer)))
  (region-end buffer))

(defmethod set-region-point-global ((global-mode emacs-mode) (start point) (end point))
  (declare (ignore global-mode))
  (cond
    ((buffer-mark-p (current-buffer))
     (move-point start (cursor-region-beginning (current-point)))
     (move-point end (cursor-region-end (current-point))))
    (t
     (line-start start)
     (line-end end))))

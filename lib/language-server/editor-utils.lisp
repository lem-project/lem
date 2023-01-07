(in-package :lem-language-server)

(defun backward-up-list (point)
  (lem:scan-lists point -1 1 t))

(defun forward-up-list (point)
  (lem:scan-lists point 1 1 t))

(defun forward-down-list (point)
  (lem:scan-lists point 1 -1 t))

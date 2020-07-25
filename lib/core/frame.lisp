(in-package :lem)

(defparameter *frame-count* 0)
(defparameter *frame-list* '())

(defstruct (frame (:constructor %make-frame))
  id
  current-window
  ;; root window
  window-tree
  ;; minibuffer
  minibuffer-buffer
  echoarea-buffer
  minibuf-window)

(defun make-frame (window-tree current-window)
  (%make-frame :id (prog1
                       *frame-count*
                     (incf *frame-count*))
               :window-tree window-tree
               :current-window current-window))

(defun add-frame (frame)
  (setf *frame-list* (append *frame-list* (list frame))))

(defun remove-frame (id)
  (let ((target-frame (find id *frame-list* :key #'frame-id)))
    (when target-frame
      (progn
        (setf *frame-list* (remove id *frame-list* :key #'frame-id))
        (frame-window-tree target-frame)))))

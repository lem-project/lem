(in-package :lem)

(defparameter *frame-count* 0)
(defparameter *frame-list* '())  ;; TODO: change to defvar when ends testing

(defstruct frame
  id
  ;; window
  current-window
  (window-tree nil)
  (floating-windows '())
  (header-windows '())
  (modified-floating-windows nil)
  (modified-header-windows nil)
  ;; minibuffer
  minibuffer-buffer
  echoarea-buffer
  (minibuf-window nil)
  (minibuffer-calls-window nil)
  (minibuffer-start-charpos nil))

(defun add-frame (frame)
  (setf *frame-list* (append *frame-list* (list frame))))

(defun remove-frame (id)
  (let ((target-frame (find id *frame-list* :key #'frame-id)))
    (when target-frame
      (progn
        (setf *frame-list* (remove id *frame-list* :key #'frame-id))
        (frame-window-tree target-frame)))))

(defun register-frame (frame)
  (let ((id *frame-count*))
    (setf (frame-id frame) id)
    (incf *frame-count*)
    id))

(defun setup-frame ()
  (let ((frame (make-frame)))
    (register-frame frame)
    (add-frame frame)
    (setup-minibuffer frame)
    (setup-windows frame)
    frame))

(defun teardown-frame (frame)
  (teardown-windows frame)
  ;; (teardown-minibuffer frame) ; minibufferをfloating-windowとして扱うので開放処理はしない
  (remove-frame (frame-id frame)))

(defun teardown-frames ()
  (mapc #'teardown-frame *frame-list*))

(defun redraw-frame (&optional (frame (first *frame-list*)))
  (redraw-display* frame))

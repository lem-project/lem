(in-package :lem)

(export '(redraw-frame))

(defparameter *display-frame-map* (make-hash-table))

(defstruct frame
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
  (minibuffer-window nil)
  (minibuffer-calls-window nil)
  (minibuffer-start-charpos nil))

(defun map-frame (implementation frame)
  (setf (gethash implementation *display-frame-map*) frame))

(defun get-frame (implementation)
  (gethash implementation *display-frame-map*))

(defun current-frame ()
  (get-frame (implementation)))

(defun unmap-frame (implementation)
  (let ((frame (gethash implementation *display-frame-map*)))
    (remhash implementation frame)
    frame))

(defun setup-frame (frame)
  (setup-minibuffer frame)
  (setup-windows frame))

(defun teardown-frame (frame)
  (teardown-windows frame)
  ;; (teardown-minibuffer frame) ; minibufferをfloating-windowとして扱うので開放処理はしない
)

(defun teardown-frames ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (teardown-frame v))
           *display-frame-map*))

(defun redraw-frame (frame)
  (redraw-display (and (redraw-after-modifying-floating-window (implementation))
                       (frame-modified-floating-windows frame)))
  (setf (frame-modified-floating-windows frame) nil))

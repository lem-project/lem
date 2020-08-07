(in-package :lem)

(export '(frame
          make-frame
          frame-current-window
          frame-window-tree
          frame-floating-windows
          frame-header-windows
          frame-modified-floating-windows
          frame-modified-header-windows
          frame-minibuffer-buffer
          frame-echoarea-buffer
          frame-minibuffer-window
          frame-minibuffer-calls-window
          frame-minibuffer-start-charpos
          map-frame
          get-frame
          current-frame
          unmap-frame
          setup-frame
          teardown-frame
          teardown-frames
          redraw-frame))

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

(defun map-frame (display frame)
  (setf (gethash display *display-frame-map*) frame))

(defun get-frame (display)
  (gethash display *display-frame-map*))

(defun current-frame ()
  (get-frame (implementation)))

(defun unmap-frame (display)
  (let ((frame (gethash display *display-frame-map*)))
    (remhash display frame)
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

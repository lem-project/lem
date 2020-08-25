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

(defclass frame ()
  (;; window
   (current-window
    :initarg :current-window
    :initform nil
    :accessor frame-current-window)
   (window-tree
    :initarg :window-tree
    :initform nil
    :accessor frame-window-tree)
   (floating-windows
    :initarg :floating-windows
    :initform '()
    :accessor frame-floating-windows)
   (header-windows
    :initarg :header-windows
    :initform '()
    :reader frame-header-windows)
   (modified-floating-windows
    :initarg :modified-floating-windows
    :initform nil
    :accessor frame-modified-floating-windows)
   (modified-header-windows
    :initarg :modified-header-windows
    :initform nil
    :accessor frame-modified-header-windows)
   ;; minibuffer
   (minibuffer-buffer
    :initarg :minibuffer-buffer
    :initform nil
    :accessor frame-minibuffer-buffer)
   (echoarea-buffer
    :initarg :echoarea-buffer
    :initform nil
    :accessor frame-echoarea-buffer)
   (minibuffer-window
    :initarg :minibuffer-window
    :initform nil
    :accessor frame-minibuffer-window)
   (minibuffer-calls-window
    :initarg :minibuffer-calls-window
    :initform nil
    :accessor frame-minibuffer-calls-window)
   (minibuffer-start-charpos
    :initarg :minibuffer-start-charpos
    :initform nil
    :accessor frame-minibuffer-start-charpos)))

(defun make-frame (&optional (old-frame (current-frame)))
  (let ((frame (make-instance 'frame)))
    (when old-frame
      (dolist (window (frame-header-windows old-frame))
        (add-header-window frame window)))
    frame))

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

(defun setup-frame (frame buffer)
  (setup-minibuffer frame)
  (setup-frame-windows frame buffer)
  (lem-if:set-first-view (implementation) (window-view (frame-current-window frame))))

(defun teardown-frame (frame)
  (teardown-windows frame)
  (teardown-minibuffer frame))

(defun teardown-frames ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (teardown-frame v))
           *display-frame-map*))

(defun redraw-frame (frame)
  (redraw-display (and (redraw-after-modifying-floating-window (implementation))
                       (frame-modified-floating-windows frame)))
  (setf (frame-modified-floating-windows frame) nil))


(defun add-header-window (frame window)
  (with-slots (header-windows) frame
    (alexandria:nconcf header-windows (list window))))

(defun remove-header-window (frame window)
  (with-slots (header-windows) frame
    (setf header-windows (remove window header-windows))))


(defun topleft-window-y (frame)
  (length (frame-header-windows frame)))

(defun topleft-window-x (frame)
  (declare (ignore frame))
  0)

(defun max-window-width (frame)
  (- (display-width) (topleft-window-x frame)))

(defun max-window-height (frame)
  (- (display-height)
     (if (sticky-bottom-minibuffer-p) 1 0)
     (topleft-window-y frame)))

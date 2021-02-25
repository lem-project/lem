(in-package :lem)

(export '(update-prompt-window
          frame
          make-frame
          frame-current-window
          frame-window-tree
          frame-floating-windows
          frame-header-windows
          frame-modified-floating-windows
          frame-modified-header-windows
          frame-floating-prompt-window
          frame-prompt-window
          map-frame
          get-frame
          current-frame
          unmap-frame
          setup-frame
          teardown-frame
          teardown-frames
          redraw-frame))

(defparameter *display-frame-map* (make-hash-table))

(defgeneric update-prompt-window (window)
  (:method (window)))

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
   (minibuffer
    :initarg :minibuffer
    :initform nil
    :accessor frame-minibuffer)
   ;; prompt
   (prompt-window
    :initform nil
    :accessor frame-floating-prompt-window)))

(defmethod frame-prompt-window ((frame frame))
  (or (frame-minibuffer frame)
      (frame-floating-prompt-window frame)))

(defmethod frame-caller-of-prompt-window ((frame frame))
  (caller-of-prompt-window (frame-prompt-window frame)))

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
  (assert (null (frame-minibuffer frame)))
  ;; (setf (frame-minibuffer frame) (create-minibuffer frame))
  (setup-frame-windows frame buffer)
  (lem-if:set-first-view (implementation) (window-view (frame-current-window frame))))

(defun teardown-frame (frame)
  (teardown-windows frame)
  (when (frame-minibuffer frame)
    (teardown-minibuffer (frame-minibuffer frame))))

(defun teardown-frames ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (teardown-frame v))
           *display-frame-map*))

(defun redraw-frame (frame)
  (when (frame-floating-prompt-window frame)
    (update-prompt-window (frame-floating-prompt-window frame)))
  (redraw-display (and (redraw-after-modifying-floating-window (implementation))
                       (frame-modified-floating-windows frame)))
  (setf (frame-modified-floating-windows frame) nil))

(defun window-in-frame-p (window frame)
  (when (or (find window (window-list frame))
            (find window (frame-floating-windows frame))
            (find window (frame-header-windows frame)))
    t))

(defun get-frame-of-window (window)
  ;; TODO: frameのリストを用意し、その中から探すようにする
  (when (window-in-frame-p window (current-frame))
    (current-frame)))


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
     (if (frame-minibuffer frame) 1 0)
     (topleft-window-y frame)))

(in-package :lem-core)

(defclass side-window (floating-window) ())

(defun make-leftside-window (buffer &key (width 30))
  (cond ((frame-leftside-window (current-frame))
         (with-current-window (frame-leftside-window (current-frame))
           (switch-to-buffer buffer)))
        (t
         (setf (frame-leftside-window (current-frame))
               (make-instance 'side-window
                              :buffer buffer
                              :x 0
                              :y 1
                              :width width
                              :height (display-height)
                              :use-modeline-p nil
                              :background-color nil
                              :border 0))
         (balance-windows))))

(defun delete-leftside-window ()
  (delete-window (frame-leftside-window (current-frame)))
  (setf (frame-leftside-window (current-frame)) nil)
  (balance-windows))

(defun resize-leftside-window (width)
  (let ((window (frame-leftside-window (current-frame))))
    (window-set-size window width (window-height window))
    (balance-windows)))

(defun resize-leftside-window-relative (offset)
  (let* ((window (frame-leftside-window (current-frame)))
         (new-width (+ (window-width window) offset)))
    (when (< 2 new-width)
      (window-set-size window
                       new-width
                       (window-height window))
      (balance-windows)
      t)))

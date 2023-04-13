(in-package :lem)

(define-editor-variable mouse-button-down-functions '())
(define-editor-variable mouse-button-up-functions '())

(deftype mouse-button ()
  '(member :button-1 :button-2 :button-3))

(defun move-to-x-y-position (window x y)
  (setf (current-window) window)
  (move-point (current-point) (window-view-point window))
  (move-to-next-virtual-line (current-point) y)
  (move-to-virtual-line-column (current-point) x))

(defun handle-mouse-button-down (x y button)
  (check-type button mouse-button)
  (case button
    (:button-1
     (multiple-value-bind (window x y)
         (focus-window-position (current-frame) x y)
       (when window
         (move-to-x-y-position window x y)
         (setf (window-last-mouse-button-down-point window)
               (copy-point (current-point) :temporary))
         (buffer-mark-cancel (current-buffer))
         (run-hooks (variable-value 'mouse-button-down-functions)))))))

(defun handle-mouse-button-up (x y button)
  (declare (ignore button))
  (let ((window (focus-window-position (current-frame) x y)))
    (when window
      (setf (window-last-mouse-button-down-point window) nil))
    (run-hooks (variable-value 'mouse-button-up-functions))))

(defun handle-mouse-motion (x y button)
  (check-type button (or null mouse-button))
  (case button
    (:button-1
     (multiple-value-bind (window x y)
         (focus-window-position (current-frame) x y)
       (when (and window (window-last-mouse-button-down-point window))
         (move-to-x-y-position window x y)
         (set-current-mark (window-last-mouse-button-down-point window)))))))

(defun handle-mouse-wheel (x y wheel-x wheel-y)
  (declare (ignore wheel-x))
  (let ((window (focus-window-position (current-frame) x y)))
    (when window
      (with-current-window window
        (scroll-up wheel-y)))))

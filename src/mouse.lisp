(in-package :lem)

(defparameter *scroll-speed* 3)

(define-editor-variable mouse-button-down-functions '())

(deftype mouse-button ()
  '(member :button-1 :button-2 :button-3))

(defclass mouse-event ()
  ((button :initarg :button
           :reader mouse-event-button
           :type mouse-button)
   (x :initarg :x
      :reader mouse-event-x)
   (y :initarg :y
      :reader mouse-event-y)))

(defclass mouse-button-down (mouse-event)
  ((clicks :initarg :clicks
           :reader mouse-button-down-clicks)))

(defclass mouse-button-up (mouse-event)
  ())

(defclass mouse-motion (mouse-event)
  ())

(defclass mouse-wheel (mouse-event)
  ((wheel-x :initarg :wheel-x :reader mouse-wheel-x)
   (wheel-y :initarg :wheel-y :reader mouse-wheel-y)))

(defun move-to-x-y-position (window x y)
  (switch-to-window window)
  (setf (current-window) window)
  (move-point (current-point) (window-view-point window))
  (move-to-next-virtual-line (current-point) y)
  (move-to-virtual-line-column (current-point) x))

(defmethod handle-mouse-event ((mouse-event mouse-button-down) window)
  (case (mouse-event-button mouse-event)
    (:button-1
     (let ((x (mouse-event-x mouse-event))
           (y (mouse-event-y mouse-event))
           (clicks (mouse-button-down-clicks mouse-event)))
       (move-to-x-y-position window x y)
       (cond ((= clicks 1)
              (setf (window-last-mouse-button-down-point window)
                    (copy-point (current-point) :temporary))
              (buffer-mark-cancel (current-buffer))
              (run-hooks (variable-value 'mouse-button-down-functions)))
             ((= clicks 2)
              (select-expression-at-current-point))
             ((<= 3 clicks)
              (select-form-at-current-point)))))))

(defmethod handle-mouse-event ((mouse-event mouse-button-up) window)
  (setf (window-last-mouse-button-down-point window) nil))

(defmethod handle-mouse-event ((mouse-event mouse-motion) window)
  (let ((x (mouse-event-x mouse-event))
        (y (mouse-event-y mouse-event))
        (button (mouse-event-button mouse-event)))
    (case button
      (:button-1
       (when (and window (window-last-mouse-button-down-point window))
         (move-to-x-y-position window x y)
         (set-current-mark (window-last-mouse-button-down-point window)))))))

(defmethod handle-mouse-event ((mouse-event mouse-wheel) window)
  (with-current-window window
    (scroll-up (* (mouse-wheel-y mouse-event)
                  *scroll-speed*))))

(defun handle-mouse-button-down (x y button clicks)
  (check-type button mouse-button)
  (multiple-value-bind (window x y)
      (focus-window-position (current-frame) x y)
    (when window
      (handle-mouse-event (make-instance 'mouse-button-down
                                         :button button
                                         :x x
                                         :y y
                                         :clicks clicks)
                          window))))

(defun handle-mouse-button-up (x y button)
  (let ((window (focus-window-position (current-frame) x y)))
    (when window
      (handle-mouse-event (make-instance 'mouse-button-up
                                         :button button
                                         :x x
                                         :y y)
                          window))))

(defun handle-mouse-motion (x y button)
  (check-type button (or null mouse-button))
  (multiple-value-bind (window x y)
      (focus-window-position (current-frame) x y)
    (when window
      (handle-mouse-event (make-instance 'mouse-motion
                                         :button button
                                         :x x
                                         :y y)
                          window))))

(defun handle-mouse-wheel (x y wheel-x wheel-y)
  (multiple-value-bind (window x y)
      (focus-window-position (current-frame) x y)
    (when window
      (handle-mouse-event (make-instance 'mouse-wheel
                                         :x x
                                         :y y
                                         :wheel-x wheel-x
                                         :wheel-y wheel-y)
                          window))))


(defun select-expression-at-current-point ()
  (cond ((syntax-open-paren-char-p (character-at (current-point)))
         (with-point ((start (current-point))
                      (end (current-point)))
           (form-offset end 1)
           (set-current-mark start)
           (move-point (current-point) end)))
        ((syntax-closed-paren-char-p (character-at (current-point) -1))
         (with-point ((start (current-point))
                      (end (current-point)))
           (character-offset start 1)
           (form-offset start -1)
           (set-current-mark start)
           (move-point (current-point) end)))
        (t
         (multiple-value-bind (start end)
             (symbol-region-at-point (current-point))
           (when start
             (set-current-mark start)
             (move-point (current-point) end))))))

(defun select-form-at-current-point ()
  (with-point ((start (current-point))
               (end (current-point)))
    (when (or (maybe-beginning-of-string start)
              (scan-lists start -1 1 t))
      (move-point end start)
      (form-offset end 1)
      (set-current-mark start)
      (move-point (current-point) end))))

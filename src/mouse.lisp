(in-package :lem)

(defclass window-separator ()
  ((start-x :initarg :start-x
            :accessor window-separator-start-x)
   (start-y :initarg :start-y
            :accessor window-separator-start-y)
   (left-window :initarg :left-window
                :reader window-separator-left-window)
   (right-window :initarg :right-window
                 :reader window-separator-right-window)))

;;;;;;;;;;;;;;;;;;

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

(defun mouse-event-p (value)
  (typep value 'mouse-event))

(defun get-x-y-position-point (window x y)
  (with-point ((point (buffer-point (window-buffer window))))
    (move-point point (window-view-point window))
    (move-to-next-virtual-line point y window)
    (move-to-virtual-line-column point x window)
    point))

(defun move-current-point-to-x-y-position (window x y)
  (switch-to-window window)
  (setf (current-window) window)
  (move-point (current-point) (window-view-point window))
  (move-to-next-virtual-line (current-point) y)
  (move-to-virtual-line-column (current-point) x))

(defvar *last-mouse-event*)
(defun last-mouse-event () *last-mouse-event*)
(defun set-last-mouse-event (mouse-event)
  (setf *last-mouse-event* mouse-event))

(defvar *last-dragged-separator* nil)

(defmethod handle-button-1 ((window window) x y clicks)
  (cond ((= clicks 1)
         (let* ((point (get-x-y-position-point window x y))
                (callback (text-property-at point :click-callback)))
           (cond (callback
                  (funcall callback window point))
                 (t
                  (move-current-point-to-x-y-position window x y)
                  (setf (window-last-mouse-button-down-point window)
                        (copy-point (current-point) :temporary))
                  (buffer-mark-cancel (current-buffer))
                  (run-hooks (variable-value 'mouse-button-down-functions))))))
        ((= clicks 2)
         (move-current-point-to-x-y-position window x y)
         (select-expression-at-current-point))
        ((<= 3 clicks)
         (move-current-point-to-x-y-position window x y)
         (select-form-at-current-point))))

(defmethod handle-button-1 ((separator window-separator) x y clicks)
  (setf *last-dragged-separator* separator))

(defmethod handle-mouse-event ((mouse-event mouse-button-down))
  (multiple-value-bind (window x y)
      (focus-window-position (current-frame)
                             (mouse-event-x mouse-event)
                             (mouse-event-y mouse-event))
    (when window
      (handle-button-1 window x y (mouse-button-down-clicks mouse-event))
      (return-from handle-mouse-event)))
  (multiple-value-bind (left-window right-window)
      (focus-separator-position (current-frame)
                                (mouse-event-x mouse-event)
                                (mouse-event-y mouse-event))
    (when (and left-window right-window)
      (handle-button-1 (make-instance 'window-separator
                                      :start-x (mouse-event-x mouse-event)
                                      :start-y (mouse-event-y mouse-event)
                                      :left-window left-window
                                      :right-window right-window)
                       (mouse-event-x mouse-event)
                       (mouse-event-y mouse-event)
                       (mouse-button-down-clicks mouse-event)))))

(defmethod handle-mouse-event ((mouse-event mouse-button-up))
  (let ((window (focus-window-position (current-frame)
                                       (mouse-event-x mouse-event)
                                       (mouse-event-y mouse-event))))
    (cond (window
           (setf (window-last-mouse-button-down-point window) nil))
          (t
           (setf *last-dragged-separator* nil)))))

(defun find-overlay-that-can-hover (point)
  (dolist (overlay (point-overlays point))
    (alexandria:when-let (callback (overlay-get overlay :hover-callback))
      (return (values overlay callback)))))

(defvar *last-hover-overlay* nil)

(defun handle-mouse-hover (point)
  (multiple-value-bind (overlay callback)
      (find-overlay-that-can-hover point)
    (when (and overlay
               (not (eq overlay *last-hover-overlay*)))
      (setf *last-hover-overlay* overlay)
      (funcall callback)
      (return-from handle-mouse-hover))
    (unless overlay
      (when *last-hover-overlay*
        (alexandria:when-let (callback (overlay-get *last-hover-overlay* :unhover-callback))
          (funcall callback))
        (setf *last-hover-overlay* nil)))))

(defmethod handle-mouse-event ((mouse-event mouse-motion))
  (cond (*last-dragged-separator*
         (let ((x (mouse-event-x mouse-event))
               (button (mouse-event-button mouse-event)))
           (when (eq button :button-1)
             (let ((diff-x (- x (window-separator-start-x *last-dragged-separator*))))
               (grow-window-width (window-separator-left-window *last-dragged-separator*)
                                  diff-x)
               (setf (window-separator-start-x *last-dragged-separator*) x)))))
        (t
         (multiple-value-bind (window x y)
             (focus-window-position (current-frame)
                                    (mouse-event-x mouse-event)
                                    (mouse-event-y mouse-event))
           (when window
             (case (mouse-event-button mouse-event)
               ((nil)
                (let ((point (get-x-y-position-point window x y)))
                  (alexandria:when-let (callback (text-property-at point :hover-callback))
                    (funcall callback window point)
                    (return-from handle-mouse-event))
                  (handle-mouse-hover point)))
               (:button-1
                (when (window-last-mouse-button-down-point window)
                  (move-current-point-to-x-y-position window x y)
                  (set-current-mark (window-last-mouse-button-down-point window))))))))))

(defmethod handle-mouse-event ((mouse-event mouse-wheel))
  (let ((window (focus-window-position (current-frame)
                                       (mouse-event-x mouse-event)
                                       (mouse-event-y mouse-event))))
    (when window
      (with-current-window window
        (scroll-up (* (mouse-wheel-y mouse-event)
                      *scroll-speed*))))))


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


(define-command <mouse-event> () ()
  (handle-mouse-event (last-mouse-event)))

(defun find-mouse-command (event)
  (etypecase event
    (mouse-button-down
     '<mouse-event>)
    (mouse-button-up
     '<mouse-event>)
    (mouse-motion
     '<mouse-event>)
    (mouse-wheel
     '<mouse-event>)))


(defun set-hover-message (overlay message &key style)
  (overlay-put overlay
               :hover-callback
               (lambda ()
                 (let ((hover-window (display-popup-message message
                                                            :timeout nil
                                                            :style style)))
                   (overlay-put overlay 'hover-window hover-window))))
  (overlay-put overlay
               :unhover-callback
               (lambda ()
                 (let ((hover-window (overlay-get overlay 'hover-window)))
                   (when hover-window
                     (delete-popup-message hover-window))))))


(defun receive-mouse-button-down (x y button clicks)
  (check-type button mouse-button)
  (send-event (make-instance 'mouse-button-down
                             :button button
                             :x x
                             :y y
                             :clicks clicks)))

(defun receive-mouse-button-up (x y button)
  (send-event (make-instance 'mouse-button-up
                             :button button
                             :x x
                             :y y)))

(defun receive-mouse-motion (x y button)
  (check-type button (or null mouse-button))
  (send-event (make-instance 'mouse-motion
                             :button button
                             :x x
                             :y y)))

(defun receive-mouse-wheel (x y wheel-x wheel-y)
  (send-event (make-instance 'mouse-wheel
                             :x x
                             :y y
                             :wheel-x wheel-x
                             :wheel-y wheel-y)))

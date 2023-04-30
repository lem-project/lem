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
      :reader mouse-event-y)
   (target :initarg :target
           :reader mouse-event-target)))

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

(defun mouse-event-window (mouse-event)
  (let ((target (mouse-event-target mouse-event)))
    (check-type target (or null window))
    target))

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

(defmethod handle-button-1 ((mouse-event mouse-button-down) (window window))
  (let ((x (mouse-event-x mouse-event))
        (y (mouse-event-y mouse-event))
        (clicks (mouse-button-down-clicks mouse-event)))
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
           (select-form-at-current-point)))))

(defmethod handle-button-1 ((mouse-event mouse-button-down) (separator window-separator))
  (setf *last-dragged-separator* separator))

(defmethod handle-mouse-event ((mouse-event mouse-button-down))
  (case (mouse-event-button mouse-event)
    (:button-1
     (handle-button-1 mouse-event (mouse-event-target mouse-event)))))

(defmethod handle-mouse-event ((mouse-event mouse-button-up))
  (cond ((mouse-event-window mouse-event)
         (setf (window-last-mouse-button-down-point (mouse-event-window mouse-event)) nil))
        (t
         (setf *last-dragged-separator* nil))))

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

(defmethod handle-mouse-motion ((mouse-event mouse-motion) target)
  (when *last-dragged-separator*
    (let ((x (mouse-event-x mouse-event))
          (button (mouse-event-button mouse-event)))
      (when (eq button :button-1)
        (let ((diff-x (- x (window-separator-start-x *last-dragged-separator*))))
          (grow-window-width (window-separator-left-window *last-dragged-separator*)
                             diff-x)
          (setf (window-separator-start-x *last-dragged-separator*) x))))))

(defmethod handle-mouse-motion ((mouse-event mouse-motion) (window window))
  (let ((x (mouse-event-x mouse-event))
        (y (mouse-event-y mouse-event))
        (button (mouse-event-button mouse-event)))
    (case button
      ((nil)
       (let ((point (get-x-y-position-point window x y)))
         (alexandria:when-let (callback (text-property-at point :hover-callback))
           (funcall callback window point)
           (return-from handle-mouse-motion))
         (handle-mouse-hover point)))
      (:button-1
       (when (window-last-mouse-button-down-point window)
         (move-current-point-to-x-y-position window x y)
         (set-current-mark (window-last-mouse-button-down-point window)))))))

(defmethod handle-mouse-event ((mouse-event mouse-motion))
  (handle-mouse-motion mouse-event (mouse-event-target mouse-event)))

(defmethod handle-mouse-event ((mouse-event mouse-wheel))
  (with-current-window (mouse-event-window mouse-event)
    (scroll-up (* (mouse-wheel-y mouse-event)
                  *scroll-speed*))))

(defun receive-mouse-button-down (x y button clicks)
  (check-type button mouse-button)
  (multiple-value-bind (window x y)
      (focus-window-position (current-frame) x y)
    (when window
      (send-event (make-instance 'mouse-button-down
                                 :button button
                                 :x x
                                 :y y
                                 :target window
                                 :clicks clicks))
      (return-from receive-mouse-button-down)))
  (multiple-value-bind (left-window right-window)
      (focus-separator-position (current-frame) x y)
    (when (and left-window right-window)
      (send-event (make-instance 'mouse-button-down
                                 :button button
                                 :x x
                                 :y y
                                 :target (make-instance 'window-separator
                                                        :start-x x
                                                        :start-y y
                                                        :left-window left-window
                                                        :right-window right-window)
                                 :clicks clicks)))))

(defun receive-mouse-button-up (x y button)
  (let ((window (focus-window-position (current-frame) x y)))
    (send-event (if window
                    (make-instance 'mouse-button-up
                                   :button button
                                   :x x
                                   :y y
                                   :target window)
                    (make-instance 'mouse-button-up
                                   :button button
                                   :x x
                                   :y y
                                   :target nil)))))

(defun receive-mouse-motion (x y button)
  (check-type button (or null mouse-button))
  (when *last-dragged-separator*
    (send-event (make-instance 'mouse-motion
                               :button button
                               :x x
                               :y y
                               :target nil))
    (return-from receive-mouse-motion))
  (multiple-value-bind (window relative-x relative-y)
      (focus-window-position (current-frame) x y)
    (when window
      (send-event (make-instance 'mouse-motion
                                 :button button
                                 :x relative-x
                                 :y relative-y
                                 :target window)))))

(defun receive-mouse-wheel (x y wheel-x wheel-y)
  (multiple-value-bind (window x y)
      (focus-window-position (current-frame) x y)
    (when window
      (send-event (make-instance 'mouse-wheel
                                 :x x
                                 :y y
                                 :target window
                                 :wheel-x wheel-x
                                 :wheel-y wheel-y)))))


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


(define-command <mouse-button-down> () ()
  (handle-mouse-event (last-mouse-event)))

(define-command <mouse-button-up> () ()
  (handle-mouse-event (last-mouse-event)))

(define-command <mouse-motion> () ()
  (handle-mouse-event (last-mouse-event)))

(define-command <mouse-wheel> () ()
  (handle-mouse-event (last-mouse-event)))

(defun find-mouse-command (event)
  (etypecase event
    (mouse-button-down
     '<mouse-button-down>)
    (mouse-button-up
     '<mouse-button-up>)
    (mouse-motion
     '<mouse-motion>)
    (mouse-wheel
     '<mouse-wheel>)))


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

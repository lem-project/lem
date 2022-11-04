(defpackage :lem.popup-window
  (:use :cl :lem)
  (:export :get-focus-item
           :apply-print-spec)
  #+sbcl
  (:lock t))
(in-package :lem.popup-window)

(defconstant +border-size+ 1)
(defconstant +min-width+   3)
(defconstant +min-height+  1)

(defvar *extra-right-margin* 0)
(defvar *extra-width-margin* 0)

(defvar *menu-buffer* nil)
(defvar *menu-window* nil)
(defvar *focus-overlay* nil)
(defvar *print-spec* nil)
(defvar *action-callback* nil)
(defvar *focus-attribute* nil)
(defvar *non-focus-attribute* nil)

(define-attribute popup-menu-attribute
  (t :foreground "white" :background "RoyalBlue"))
(define-attribute non-focus-popup-menu-attribute
  (t :background "#444" :foreground "white"))

(defgeneric adjust-for-redrawing (gravity popup-window)
  (:method (gravity popup-window)))

(defgeneric compute-popup-window-rectangle (gravity &key source-window width height border-size))

(defclass gravity ()
  ((offset-x :initarg :offset-x :accessor gravity-offset-x :initform 0)
   (offset-y :initarg :offset-y :accessor gravity-offset-y :initform 0)))
(defclass gravity-center (gravity) ())
(defclass gravity-top (gravity) ())
(defclass gravity-topright (gravity) ())
(defclass gravity-cursor (gravity) ())
(defclass gravity-follow-cursor (gravity-cursor) ())

(defclass popup-window (floating-window)
  ((gravity
    :initarg :gravity
    :reader popup-window-gravity)
   (source-window
    :initarg :source-window
    :reader popup-window-source-window)
   (base-width
    :initarg :base-width
    :reader popup-window-base-width)
   (base-height
    :initarg :base-height
    :reader popup-window-base-height)
   (style
    :initarg :style
    :reader popup-window-style)))

(defun ensure-gravity (gravity)
  (if (typep gravity 'gravity)
      gravity
      (ecase gravity
        (:center (make-instance 'gravity-center))
        (:top (make-instance 'gravity-top))
        (:topright (make-instance 'gravity-topright))
        (:cursor (make-instance 'gravity-cursor))
        (:follow-cursor (make-instance 'gravity-follow-cursor)))))

(defmethod adjust-for-redrawing ((gravity gravity-follow-cursor) popup-window)
  (destructuring-bind (x y width height)
      (compute-popup-window-rectangle (popup-window-gravity popup-window)
                                      :source-window (popup-window-source-window popup-window)
                                      :width (popup-window-base-width popup-window)
                                      :height (popup-window-base-height popup-window)
                                      :border-size (floating-window-border popup-window))
    (lem::window-set-size popup-window width height)
    (lem::window-set-pos popup-window
                         (+ x (floating-window-border popup-window))
                         (+ y (floating-window-border popup-window)))))

(defmethod compute-popup-window-rectangle :around ((gravity gravity) &key &allow-other-keys)
  (destructuring-bind (x y width height)
      (call-next-method)
    (list (+ x (gravity-offset-x gravity))
          (+ y (gravity-offset-y gravity))
          width
          height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-center)
                                           &key width height &allow-other-keys)
  (let ((x (- (floor (display-width) 2)
              (floor width 2)))
        (y (- (floor (display-height) 2)
              (floor height 2))))
    (list x y width height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-cursor)
                                           &key source-window width height border-size)
  (let* ((border-size (or border-size 0))
         (b2 (* border-size 2))
         (disp-w (max (- (display-width)  b2 *extra-right-margin*)
                      +min-width+))
         (disp-h (max (- (display-height) b2)
                      +min-height+))
         (width  (max (+ width *extra-width-margin*)
                      +min-width+))
         (height (max height
                      +min-height+))
         (x (+ (window-x source-window)
               (window-cursor-x source-window)))
         (y (max (min (+ (window-y source-window)
                         (window-cursor-y source-window)
                         border-size)
                      (1- (display-height)))
                 0))
         (w width)
         (h height))
    ;; calc y and h
    (when (> (+ y height) disp-h)
      (decf h (- (+ y height) disp-h)))
    (when (< h (min height (floor disp-h 3)))
      (setf h height)
      (decf y (+ height b2 1)))
    (when (< y 0)
      (decf h (- y))
      (setf y 0))
    (when (<= h 0) ; for safety
      (setf y 0)
      (setf h (min height disp-h)))
    ;; calc x and w
    (when (> (+ x width) disp-w)
      (decf x (- (+ x width) disp-w)))
    (when (< x 0)  ; for safety
      (setf x 0)
      (setf w (min width disp-w)))
    (list x y w h)))

(defmethod compute-popup-window-rectangle ((gravity gravity-top) &key source-window width height
                                                                 &allow-other-keys)
  (let* ((x (- (floor (display-width) 2)
               (floor width 2)))
         (y (+ (window-y source-window) 1)))
    (list x y width height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-topright)
                                           &key source-window width height border-size
                                           &allow-other-keys)
  (let* ((b2 (* (or border-size 0) 2))
         (win-x (window-x source-window))
         (win-y (window-y source-window))
         (win-w (max (- (window-width  source-window) b2 2)
                     +min-width+))
         (win-h (max (- (window-height source-window) b2)
                     +min-height+))
         (width  (max (+ width *extra-width-margin*)
                      +min-width+))
         (height (max height
                      +min-height+))
         (x (+ win-x (- win-w width)))
         (y (+ win-y 1))
         (w width)
         (h height))
    ;; calc y and h
    (when (> (+ y height) (+ win-y win-h))
      (decf h (- (+ y height) (+ win-y win-h))))
    (when (<= h 0)    ; for safety
      (setf y win-y)
      (setf h (min height win-h)))
    ;; calc x and w
    (when (< x win-x) ; for safety
      (setf x win-x)
      (setf w (min width win-w)))
    (list x y w h)))

(defmethod window-redraw ((popup-window popup-window) force)
  (adjust-for-redrawing (popup-window-gravity popup-window) popup-window)
  (call-next-method))

(defstruct style
  (gravity :cursor)
  (use-border t)
  (background-color nil)
  (offset-x 0)
  (offset-y 0))

(defun merge-style (style &key (gravity nil gravity-p)
                               (use-border nil use-border-p)
                               (background-color nil background-color-p))
  (make-style :gravity (if gravity-p
                           gravity
                           (style-gravity style))
              :use-border (if use-border-p
                              use-border
                              (style-use-border style))
              :background-color (if background-color-p
                                    background-color
                                    (style-background-color style))
              :offset-x (style-offset-x style)
              :offset-y (style-offset-y style)))

(defun ensure-style (style)
  (cond ((null style)
         (make-style))
        ((style-p style)
         style)
        (t
         (apply #'make-style style))))

(defun make-popup-window (&key (source-window (alexandria:required-argument :source-window))
                               (buffer (alexandria:required-argument :buffer))
                               (width (alexandria:required-argument :width))
                               (height (alexandria:required-argument :height))
                               style)
  (let* ((style (ensure-style style))
         (border-size (if (style-use-border style) +border-size+ 0))
         (gravity (ensure-gravity (style-gravity style))))
    (setf (gravity-offset-x gravity) (style-offset-x style)
          (gravity-offset-y gravity) (style-offset-y style))
    (destructuring-bind (x y w h)
        (compute-popup-window-rectangle gravity
                                        :source-window source-window
                                        :width width
                                        :height height
                                        :border-size border-size)
      (make-instance 'popup-window
                     :buffer buffer
                     :x (+ x border-size)
                     :y (+ y border-size)
                     :width  w
                     :height h
                     :use-modeline-p nil
                     :gravity gravity
                     :source-window source-window
                     :base-width  width
                     :base-height height
                     :border border-size
                     :background-color (style-background-color style)
                     :style style))))

(defun update-popup-window (&key (source-window (alexandria:required-argument :source-window))
                                 (width (alexandria:required-argument :width))
                                 (height (alexandria:required-argument :height))
                                 (destination-window
                                  (alexandria:required-argument :destination-window)))
  (let* ((style (popup-window-style destination-window))
         (border-size (if (style-use-border style) +border-size+ 0))
         (gravity (ensure-gravity (style-gravity style))))
    (setf (gravity-offset-x gravity) (style-offset-x style)
          (gravity-offset-y gravity) (style-offset-y style))
    (destructuring-bind (x y w h)
        (compute-popup-window-rectangle gravity
                                        :source-window source-window
                                        :width width
                                        :height height
                                        :border-size border-size)
      (lem::window-set-size destination-window w h)
      (lem::window-set-pos destination-window
                           (+ x border-size)
                           (+ y border-size))
      destination-window)))

(defun quit-popup-window (floating-window)
  (delete-window floating-window))

(defun focus-point ()
  (alexandria:when-let (buffer *menu-buffer*)
    (buffer-point buffer)))

(defun update-focus-overlay (point)
  (when *focus-overlay*
    (delete-overlay *focus-overlay*))
  (with-point ((start point)
               (end point))
    (setf *focus-overlay*
          (make-overlay (line-start start)
                        (line-end end)
                        *focus-attribute*))))

(defgeneric apply-print-spec (print-spec point item)
  (:method ((print-spec function) point item)
    (let ((string (funcall print-spec item)))
      (insert-string point string))))

(defun fill-background-color (buffer background-color)
  (with-point ((p (buffer-start-point buffer))
               (start (buffer-start-point buffer)))
    (flet ((put-attribute (start end attribute)
             (put-text-property
              start end
              :attribute (make-attribute
                          :foreground
                          (or (and attribute
                                   (attribute-foreground attribute))
                              (alexandria:when-let
                                  (attribute (ensure-attribute *non-focus-attribute* nil))
                                (attribute-foreground attribute)))
                          :background background-color
                          :bold-p (and attribute
                                       (attribute-bold-p attribute))
                          :underline-p (and attribute
                                            (attribute-underline-p attribute))))))
      (loop
        (let ((start-attribute (ensure-attribute (text-property-at p :attribute) nil)))
          (unless (next-single-property-change p :attribute)
            (put-attribute start (buffer-end-point buffer) start-attribute)
            (return))
          (put-attribute start p start-attribute)
          (move-point start p))))))

(defun compute-buffer-width (buffer)
  (with-point ((point (buffer-start-point buffer)))
    (loop :maximize (point-column (line-end point))
          :while (line-offset point 1))))

(defun fill-in-the-background-with-space (buffer)
  (labels ((fill-space (width)
             (with-point ((point (buffer-start-point buffer) :left-inserting))
               (loop :do (move-to-column point width t)
                     :while (line-offset point 1)))))
    (let ((width (compute-buffer-width buffer)))
      (fill-space width)
      width)))

(defun fill-background (buffer)
  (let ((width (fill-in-the-background-with-space buffer))
        (attribute (ensure-attribute *non-focus-attribute* nil)))
    (if attribute
        (fill-background-color buffer (attribute-background attribute))
        (log:error "*non-focus-attribute* is an invalid value" *non-focus-attribute*))
    width))

(defun insert-items (point items print-spec)
  (with-point ((start point :right-inserting))
    (loop :for (item . continue-p) :on items
          :do (move-point start point)
              (apply-print-spec print-spec point item)
              (line-end point)
              (put-text-property start point :item item)
              (when continue-p
                (insert-character point #\newline)))
    (buffer-start point)))

(defun make-menu-buffer ()
  (or *menu-buffer*
      (setq *menu-buffer*
            (make-buffer "*popup menu*" :enable-undo-p nil :temporary t))))

(defun create-menu-buffer (items print-spec)
  (let* ((buffer (make-menu-buffer))
         (point (buffer-point buffer)))
    (erase-buffer buffer)
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (insert-items point items print-spec)
    (update-focus-overlay (buffer-start-point buffer))
    (let ((width (fill-background buffer)))
      (values buffer width))))

(defun get-focus-item ()
  (alexandria:when-let (p (focus-point))
    (text-property-at (line-start p) :item)))

(defmethod lem-if:display-popup-menu (implementation items
                                      &key action-callback
                                           print-spec
                                           (focus-attribute 'popup-menu-attribute)
                                           (non-focus-attribute 'non-focus-popup-menu-attribute)
                                           style)
  (let ((style (ensure-style style))
        (focus-attribute (ensure-attribute focus-attribute))
        (non-focus-attribute (ensure-attribute non-focus-attribute)))
    (setf *print-spec* print-spec)
    (setf *action-callback* action-callback)
    (setf *focus-attribute* focus-attribute)
    (setf *non-focus-attribute* non-focus-attribute)
    (multiple-value-bind (buffer width)
        (create-menu-buffer items print-spec)
      (setf *menu-window*
            (make-popup-window :source-window (current-window)
                               :buffer buffer
                               :width width
                               :height (min 20 (length items))
                               :style (merge-style
                                       style
                                       :background-color (or (style-background-color style)
                                                             (attribute-background
                                                              non-focus-attribute))))))))

(defmethod lem-if:popup-menu-update (implementation items)
  (multiple-value-bind (buffer width)
      (create-menu-buffer items *print-spec*)
    (update-focus-overlay (buffer-point buffer))
    (let ((source-window (current-window)))
      (when (eq source-window
                (frame-prompt-window (current-frame)))
        ;; prompt-window内でcompletion-windowを出している場合,
        ;; completion-windowの位置を決める前にprompt-windowの調整を先にしておかないとずれるため,
        ;; ここで更新する
        (lem::update-floating-prompt-window (current-frame)))
      (update-popup-window :source-window source-window
                           :width width
                           :height (min 20 (length items))
                           :destination-window *menu-window*))))

(defmethod lem-if:popup-menu-quit (implementation)
  (when *focus-overlay*
    (delete-overlay *focus-overlay*))
  (quit-popup-window *menu-window*)
  (when *menu-buffer*
    (delete-buffer *menu-buffer*)
    (setf *menu-buffer* nil)))

(defun move-focus (function)
  (alexandria:when-let (point (focus-point))
    (funcall function point)
    (window-see *menu-window*)
    (update-focus-overlay point)))

(defmethod lem-if:popup-menu-down (implementation)
  (move-focus
   (lambda (point)
     (unless (line-offset point 1)
       (buffer-start point)))))

(defmethod lem-if:popup-menu-up (implementation)
  (move-focus
   (lambda (point)
     (unless (line-offset point -1)
       (buffer-end point)))))

(defmethod lem-if:popup-menu-first (implementation)
  (move-focus
   (lambda (point)
     (buffer-start point))))

(defmethod lem-if:popup-menu-last (implementation)
  (move-focus
   (lambda (point)
     (buffer-end point))))

(defmethod lem-if:popup-menu-select (implementation)
  (alexandria:when-let ((f *action-callback*)
                        (item (get-focus-item)))
    (funcall f item)))

(defun compute-size-from-buffer (buffer)
  (let ((width (compute-buffer-width buffer))
        (height (buffer-nlines buffer)))
    (list width height)))

(defun make-popup-buffer (text)
  (let ((buffer (make-buffer "*Popup Message*" :temporary t :enable-undo-p nil)))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (erase-buffer buffer)
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    buffer))

(defmethod lem-if:display-popup-message (implementation buffer-or-string
                                         &key timeout
                                              destination-window
                                              style)
  (let ((buffer (etypecase buffer-or-string
                  (string (make-popup-buffer buffer-or-string))
                  (buffer buffer-or-string))))
    (destructuring-bind (width height)
        (compute-size-from-buffer buffer)
      (delete-popup-message destination-window)
      (let ((window (make-popup-window :source-window (current-window)
                                       :buffer buffer
                                       :width width
                                       :height height
                                       :style style)))
        (buffer-start (window-view-point window))
        (window-see window)
        (when timeout
          (check-type timeout number)
          (start-timer (round (* timeout 1000))
                       nil
                       (lambda ()
                         (unless (deleted-window-p window)
                           (delete-window window)))))
        window))))

(defmethod lem-if:delete-popup-message (implementation popup-message)
  (when (and popup-message (not (deleted-window-p popup-message)))
    (delete-window popup-message)))

(defmethod lem:show-message (value &key timeout (style '(:gravity :follow-cursor)))
  (setf (frame-message-window (current-frame))
        (display-popup-message value
                               :timeout timeout
                               :destination-window (frame-message-window (current-frame))
                               :style style)))

(defmethod lem:clear-message ()
  (delete-popup-message (frame-message-window (current-frame)))
  (setf (frame-message-window (current-frame)) nil))

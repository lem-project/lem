(defpackage :lem/popup-window
  (:use :cl :lem)
  (:export :get-focus-item
           :write-header
           :find-popup-menu
           :apply-print-spec)
  #+sbcl
  (:lock t))
(in-package :lem/popup-window)

(defconstant +border-size+ 1)
(defconstant +min-width+   3)
(defconstant +min-height+  1)

(defvar *extra-right-margin* 0)
(defvar *extra-width-margin* 0)

(defvar *popup-menu* nil)

(defparameter *style* '(:use-border t :offset-y 0))

(defclass popup-menu ()
  ((buffer
    :initarg :buffer
    :accessor popup-menu-buffer)
   (window
    :initarg :window
    :accessor popup-menu-window)
   (focus-overlay
    :initarg :focus-overlay
    :accessor popup-menu-focus-overlay)
   (action-callback
    :initarg :action-callback
    :accessor popup-menu-action-callback)
   (focus-attribute
    :initarg :focus-attribute
    :accessor popup-menu-focus-attribute)))

(define-attribute popup-menu-attribute
  (t :foreground "white" :background "RoyalBlue"))
(define-attribute non-focus-popup-menu-attribute)

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
(defclass gravity-vertically-adjacent-window (gravity) ())
(defclass gravity-horizontally-adjacent-window (gravity) ())

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

(defmethod lem:window-parent ((window popup-window))
  (popup-window-source-window window))

(defun find-popup-menu (&key parent-window current-window)
  (assert (alexandria:xor parent-window current-window))
  (cond (parent-window
         (when (and *popup-menu*
                    (eq parent-window (window-parent (popup-menu-window *popup-menu*))))
           *popup-menu*))
        (current-window
         (when (and *popup-menu*
                    (eq current-window (popup-menu-window *popup-menu*)))
           *popup-menu*))))

(defun ensure-gravity (gravity)
  (if (typep gravity 'gravity)
      gravity
      (ecase gravity
        (:center (make-instance 'gravity-center))
        (:top (make-instance 'gravity-top))
        (:topright (make-instance 'gravity-topright))
        (:cursor (make-instance 'gravity-cursor))
        (:follow-cursor (make-instance 'gravity-follow-cursor))
        (:vertically-adjacent-window (make-instance 'gravity-vertically-adjacent-window))
        (:horizontally-adjacent-window (make-instance 'gravity-horizontally-adjacent-window)))))

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

(defmethod compute-popup-window-rectangle ((gravity gravity-vertically-adjacent-window)
                                           &key source-window width height #+(or)border-size
                                           &allow-other-keys)
  (let ((x (+ (window-x source-window)
              (window-width source-window)))
        (y (window-y source-window)))
    (list x y width height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-horizontally-adjacent-window)
                                           &key source-window width height border-size
                                           &allow-other-keys)
  (let ((x (- (window-x source-window) border-size))
        (y (+ (window-y source-window)
              (window-height source-window)
              border-size)))
    (list x
          y
          (max width (window-width source-window))
          height)))

(defmethod window-redraw ((popup-window popup-window) force)
  (adjust-for-redrawing (popup-window-gravity popup-window) popup-window)
  (call-next-method))

(defstruct style
  (gravity :cursor)
  (use-border t)
  (background-color nil)
  (offset-x 0)
  (offset-y 0)
  (cursor-invisible nil)
  shape)

(defun merge-style (style &key (gravity nil gravity-p)
                               (use-border nil use-border-p)
                               (background-color nil background-color-p)
                               (cursor-invisible nil cursor-invisible-p)
                               (shape nil shape-p))
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
              :offset-y (style-offset-y style)
              :cursor-invisible (if cursor-invisible-p
                                    cursor-invisible
                                    (style-cursor-invisible style))
              :shape (if shape-p
                         shape
                         (style-shape style))))

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
                     :border-shape (style-shape style)
                     :background-color (style-background-color style)
                     :cursor-invisible (style-cursor-invisible style)
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

(defun focus-point (popup-menu)
  (buffer-point (popup-menu-buffer popup-menu)))

(defun make-focus-overlay (point focus-attribute)
  (with-point ((start point)
               (end point))
    (line-start start)
    (line-end end)
    (let ((ov (make-overlay start end focus-attribute)))
      (overlay-put ov :display-line t)
      ov)))

(defun update-focus-overlay (popup-menu point)
  (delete-overlay (popup-menu-focus-overlay popup-menu))
  (setf (popup-menu-focus-overlay popup-menu)
        (make-focus-overlay point (popup-menu-focus-attribute popup-menu))))

(defgeneric write-header (print-spec point)
  (:method (print-spec point)))

(defgeneric apply-print-spec (print-spec point item)
  (:method ((print-spec function) point item)
    (let ((string (funcall print-spec item)))
      (insert-string point string))))

(defun compute-buffer-width (buffer)
  (with-point ((point (buffer-start-point buffer)))
    (loop :maximize (point-column (line-end point))
          :while (line-offset point 1))))

(defun compute-buffer-height (buffer)
  (buffer-nlines buffer))

(defun compute-buffer-size (buffer)
  (list (compute-buffer-width buffer)
        (compute-buffer-height buffer)))

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

(defun get-focus-item (&optional (popup-menu *popup-menu*))
  (when popup-menu
    (alexandria:when-let (p (focus-point popup-menu))
      (text-property-at (line-start p) :item))))

(defun make-menu-buffer ()
  (make-buffer "*popup menu*" :enable-undo-p nil :temporary t))

(defun buffer-start-line (buffer)
  (buffer-value buffer 'start-line))

(defun (setf buffer-start-line) (line buffer)
  (setf (buffer-value buffer 'start-line) line))

(defun setup-menu-buffer (buffer items print-spec focus-attribute &optional last-line)
  (clear-overlays buffer)
  (erase-buffer buffer)
  (setf (variable-value 'line-wrap :buffer buffer) nil)
  (let ((point (buffer-point buffer)))
    (write-header print-spec point)
    (let ((header-exists (not (start-line-p point)))
          (start-line 1))
      (when header-exists
        (insert-character point #\newline)
        (setf start-line (line-number-at-point point)))
      (setf (buffer-start-line buffer) start-line)
      (insert-items point items print-spec)
      (buffer-start point)
      (when header-exists
        (move-to-line point start-line))
      (when last-line (move-to-line point last-line))
      (let ((focus-overlay (make-focus-overlay point focus-attribute))
            (width (compute-buffer-width buffer)))
        (values width
                focus-overlay
                (+ (1- start-line)
                   (length items)))))))

(defmethod lem-if:display-popup-menu (implementation items
                                      &key action-callback
                                           print-spec
                                           (style *style*)
                                           (max-display-items 20))
  (when *popup-menu*
    (lem-if:popup-menu-quit implementation))
  (let ((style (ensure-style style))
        (focus-attribute (ensure-attribute 'popup-menu-attribute))
        (non-focus-attribute (ensure-attribute 'non-focus-popup-menu-attribute))
        (buffer (make-menu-buffer)))
    (multiple-value-bind (menu-width focus-overlay height)
        (setup-menu-buffer buffer
                           items
                           print-spec
                           focus-attribute)
      (let ((window (make-popup-window :source-window (current-window)
                                       :buffer buffer
                                       :width menu-width
                                       :height (min max-display-items height)
                                       :style (merge-style
                                               style
                                               :background-color (or (style-background-color style)
                                                                     (attribute-background
                                                                      non-focus-attribute))
                                               :cursor-invisible t))))
        (setf *popup-menu*
              (make-instance 'popup-menu
                             :buffer buffer
                             :window window
                             :focus-overlay focus-overlay
                             :action-callback action-callback
                             :focus-attribute focus-attribute))))))

(defmethod lem-if:popup-menu-update (implementation items &key print-spec (max-display-items 20) keep-focus)
  (when *popup-menu*
    (let ((last-line (line-number-at-point (buffer-point (popup-menu-buffer *popup-menu*)))))
      (multiple-value-bind (menu-width focus-overlay height)
          (setup-menu-buffer (popup-menu-buffer *popup-menu*)
                             items
                             print-spec
                             (popup-menu-focus-attribute *popup-menu*)
                             (if keep-focus last-line))
        (setf (popup-menu-focus-overlay *popup-menu*) focus-overlay)
        (let ((source-window (current-window)))
          (when (eq source-window
                    (frame-prompt-window (current-frame)))
            ;; prompt-window内でcompletion-windowを出している場合,
            ;; completion-windowの位置を決める前にprompt-windowの調整を先にしておかないとずれるため,
            ;; ここで更新する
            (lem::update-floating-prompt-window (current-frame)))
          (update-popup-window :source-window source-window
                               :width menu-width
                               :height (min max-display-items height)
                               :destination-window (popup-menu-window *popup-menu*)))))))

(defmethod lem-if:popup-menu-quit (implementation)
  (when *popup-menu*
    (delete-window (popup-menu-window *popup-menu*))
    (delete-buffer (popup-menu-buffer *popup-menu*))
    (setf *popup-menu* nil)))

(defun header-point-p (point)
  (< (line-number-at-point point)
     (buffer-start-line (point-buffer point))))

(defun move-focus (popup-menu function)
  (alexandria:when-let (point (focus-point popup-menu))
    (funcall function point)
    (line-start point)
    (window-see (popup-menu-window popup-menu))
    (let ((buffer (point-buffer point)))
      (when (header-point-p point)
        (move-to-line point (buffer-start-line buffer))))
    (update-focus-overlay popup-menu point)))

(defmethod lem-if:popup-menu-down (implementation)
  (move-focus
   *popup-menu*
   (lambda (point)
     (unless (line-offset point 1)
       (buffer-start point)))))

(defmethod lem-if:popup-menu-up (implementation)
  (move-focus
   *popup-menu*
   (lambda (point)
     (unless (line-offset point -1)
       (buffer-end point))
     (when (header-point-p point)
       (buffer-end point)))))

(defmethod lem-if:popup-menu-first (implementation)
  (move-focus
   *popup-menu*
   (lambda (point)
     (buffer-start point))))

(defmethod lem-if:popup-menu-last (implementation)
  (move-focus
   *popup-menu*
   (lambda (point)
     (buffer-end point))))

(defmethod lem-if:popup-menu-select (implementation)
  (alexandria:when-let ((f (popup-menu-action-callback *popup-menu*))
                        (item (get-focus-item)))
    (funcall f item)))

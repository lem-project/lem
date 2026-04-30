(defpackage :lem-pixel-demo
  (:use :cl :lem)
  (:export :pixel-demo-animate
           :pixel-demo-follow-mouse
           :pixel-demo-debug
           :pixel-demo-compare
           :pixel-demo-toggle-mode
           :pixel-demo-stop))
(in-package :lem-pixel-demo)

;;; Demo state management

(defvar *demo-window* nil
  "Current demo floating window.")

(defvar *demo-timer* nil
  "Current demo timer.")

(defvar *demo-buffer* nil
  "Current demo buffer.")

(defvar *demo-mode* nil
  "Current demo mode: :animate, :follow-mouse, :debug, or nil.")

(defun cleanup-demo ()
  "Clean up any running demo."
  (when *demo-timer*
    (stop-timer *demo-timer*)
    (setf *demo-timer* nil))
  (when *demo-window*
    (ignore-errors (delete-window *demo-window*))
    (setf *demo-window* nil))
  (when *demo-buffer*
    (ignore-errors (delete-buffer *demo-buffer*))
    (setf *demo-buffer* nil))
  (setf *demo-mode* nil))

(defun make-demo-buffer (name content)
  "Create a temporary buffer for demo."
  (let ((buffer (make-buffer name :temporary t :enable-undo-p nil)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (with-point ((p (buffer-point buffer)))
        (insert-string p content)))
    buffer))

(defun update-demo-buffer (content)
  "Update the demo buffer content."
  (when *demo-buffer*
    (with-buffer-read-only *demo-buffer* nil
      (erase-buffer *demo-buffer*)
      (with-point ((p (buffer-point *demo-buffer*)))
        (insert-string p content)))))

;;; Animation Demo
;;; Demonstrates smooth pixel-based movement vs jerky character-based movement

(defvar *animation-x* 0)
(defvar *animation-y* 0)
(defvar *animation-dx* 2)
(defvar *animation-dy* 1)
(defvar *use-pixel-mode* t)

(defun animation-step ()
  "Perform one animation step."
  (when (and *demo-window* (eq *demo-mode* :animate))
    (let* ((char-width (lem-if:get-char-width (implementation)))
           (char-height (lem-if:get-char-height (implementation)))
           (display-width (* (display-width) char-width))
           (display-height (* (display-height) char-height))
           (win-width (or (floating-window-pixel-width *demo-window*)
                          (* (window-width *demo-window*) char-width)))
           (win-height (or (floating-window-pixel-height *demo-window*)
                           (* (window-height *demo-window*) char-height))))
      ;; Update position
      (incf *animation-x* *animation-dx*)
      (incf *animation-y* *animation-dy*)
      ;; Bounce off walls
      (when (or (<= *animation-x* 0) (>= (+ *animation-x* win-width) display-width))
        (setf *animation-dx* (- *animation-dx*))
        (setf *animation-x* (max 0 (min *animation-x* (- display-width win-width)))))
      (when (or (<= *animation-y* 0) (>= (+ *animation-y* win-height) display-height))
        (setf *animation-dy* (- *animation-dy*))
        (setf *animation-y* (max 0 (min *animation-y* (- display-height win-height)))))
      ;; Update window position
      (if *use-pixel-mode*
          ;; Pixel mode: smooth movement
          (floating-window-set-pixel-position *demo-window* *animation-x* *animation-y*)
          ;; Character mode: jerky movement (for comparison)
          (let ((char-x (floor *animation-x* char-width))
                (char-y (floor *animation-y* char-height)))
            (lem-if:set-view-pos (implementation)
                                 (lem-core::window-view *demo-window*)
                                 char-x char-y)))
      ;; Update display info
      (update-demo-buffer
       (format nil "~A~%~%X: ~6D px~%Y: ~6D px~%~%Press M-x pixel-demo-stop~%to end demo"
               (if *use-pixel-mode* "PIXEL MODE" "CHAR MODE")
               *animation-x*
               *animation-y*))
      (redraw-display))))

(define-command pixel-demo-animate () ()
  "Start the smooth animation demo.
Demonstrates pixel-based vs character-based window positioning.
A floating window bounces around the screen smoothly."
  (cleanup-demo)
  (setf *demo-mode* :animate)
  (setf *animation-x* 100)
  (setf *animation-y* 100)
  (setf *animation-dx* 3)
  (setf *animation-dy* 2)
  (setf *use-pixel-mode* t)
  (setf *demo-buffer* (make-demo-buffer "*Pixel Demo*" "PIXEL MODE\n\nStarting..."))
  (setf *demo-window* (make-floating-window
                       :buffer *demo-buffer*
                       :x 5 :y 5
                       :width 24 :height 8
                       :pixel-x *animation-x*
                       :pixel-y *animation-y*
                       :use-border t))
  (setf *demo-timer* (make-timer #'animation-step :name "pixel-demo-animate"))
  (start-timer *demo-timer* 16 :repeat t)  ; ~60fps
  (message "Animation started. Use M-x pixel-demo-toggle-mode to switch modes, M-x pixel-demo-stop to end."))

(define-command pixel-demo-toggle-mode () ()
  "Toggle between pixel mode and character mode in animation demo."
  (when (eq *demo-mode* :animate)
    (setf *use-pixel-mode* (not *use-pixel-mode*))
    (message "Switched to ~A mode" (if *use-pixel-mode* "PIXEL" "CHARACTER"))))

;;; Mouse Following Demo
;;; Demonstrates a floating window that follows the mouse cursor at pixel precision

(defun follow-mouse-step ()
  "Update window position to follow mouse."
  (when (and *demo-window* (eq *demo-mode* :follow-mouse))
    (let ((mouse-event (lem-core::last-mouse-event)))
      (when mouse-event
        (let* ((pixel-x (lem-core::mouse-event-pixel-x mouse-event))
               (pixel-y (lem-core::mouse-event-pixel-y mouse-event))
               (char-x (lem-core::mouse-event-x mouse-event))
               (char-y (lem-core::mouse-event-y mouse-event)))
          (when (and pixel-x pixel-y)
            ;; Offset so window appears below and to the right of cursor
            (let ((offset-x 20)
                  (offset-y 20))
              (floating-window-set-pixel-position *demo-window*
                                                  (+ pixel-x offset-x)
                                                  (+ pixel-y offset-y))
              (update-demo-buffer
               (format nil "Mouse Following~%~%Pixel: (~D, ~D)~%Char:  (~D, ~D)~%~%Move your mouse!"
                       pixel-x pixel-y char-x char-y))
              (redraw-display))))))))

(define-command pixel-demo-follow-mouse () ()
  "Start the mouse following demo.
A floating window follows your mouse cursor at pixel precision."
  (cleanup-demo)
  (setf *demo-mode* :follow-mouse)
  (setf *demo-buffer* (make-demo-buffer "*Mouse Follow*" "Mouse Following\n\nMove your mouse!"))
  (setf *demo-window* (make-floating-window
                       :buffer *demo-buffer*
                       :x 10 :y 10
                       :width 22 :height 7
                       :pixel-x 200
                       :pixel-y 200
                       :use-border t))
  (setf *demo-timer* (make-timer #'follow-mouse-step :name "pixel-demo-follow"))
  (start-timer *demo-timer* 16 :repeat t)  ; ~60fps
  (message "Mouse following started. Move your mouse! M-x pixel-demo-stop to end."))

;;; Debug Coordinate Display
;;; Shows real-time coordinate information

(defun debug-display-step ()
  "Update coordinate debug display."
  (when (and *demo-window* (eq *demo-mode* :debug))
    (let* ((mouse-event (lem-core::last-mouse-event))
           (char-width (lem-if:get-char-width (implementation)))
           (char-height (lem-if:get-char-height (implementation))))
      (multiple-value-bind (win-px win-py win-pw win-ph)
          (floating-window-pixel-bounds *demo-window*)
        (let ((content
                (format nil "=== Pixel Coordinate Debug ===~%~%~
                            Display:~%  Size: ~Dx~D chars~%  Char: ~Dx~D px~%~%~
                            Window:~%  Char: (~D,~D) ~Dx~D~%  Pixel: (~D,~D) ~Dx~D~%~%~
                            ~A"
                        (display-width) (display-height)
                        char-width char-height
                        (window-x *demo-window*) (window-y *demo-window*)
                        (window-width *demo-window*) (window-height *demo-window*)
                        win-px win-py win-pw win-ph
                        (if mouse-event
                            (format nil "Mouse:~%  Char: (~D,~D)~%  Pixel: (~D,~D)"
                                    (lem-core::mouse-event-x mouse-event)
                                    (lem-core::mouse-event-y mouse-event)
                                    (lem-core::mouse-event-pixel-x mouse-event)
                                    (lem-core::mouse-event-pixel-y mouse-event))
                            "Mouse: (no event)"))))
          (update-demo-buffer content)
          (redraw-display))))))

(define-command pixel-demo-debug () ()
  "Start the coordinate debug display.
Shows real-time pixel and character coordinate information."
  (cleanup-demo)
  (setf *demo-mode* :debug)
  (setf *demo-buffer* (make-demo-buffer "*Coord Debug*" "Loading..."))
  (setf *demo-window* (make-floating-window
                       :buffer *demo-buffer*
                       :x 2 :y 2
                       :width 35 :height 14
                       :pixel-x 50
                       :pixel-y 50
                       :use-border t))
  (setf *demo-timer* (make-timer #'debug-display-step :name "pixel-demo-debug"))
  (start-timer *demo-timer* 100 :repeat t)  ; 10fps is enough for debug
  (message "Debug display started. M-x pixel-demo-stop to end."))

;;; Comparison demo - side by side pixel vs char mode

(defvar *compare-window-pixel* nil)
(defvar *compare-window-char* nil)
(defvar *compare-buffer-pixel* nil)
(defvar *compare-buffer-char* nil)
(defvar *compare-x* 0)
(defvar *compare-y* 0)

(defun compare-animation-step ()
  "Animate both windows for comparison."
  (when (eq *demo-mode* :compare)
    (let* ((char-width (lem-if:get-char-width (implementation)))
           (char-height (lem-if:get-char-height (implementation)))
           (display-width (* (display-width) char-width))
           (display-height (* (display-height) char-height))
           (win-width (* 20 char-width)))
      ;; Circular motion
      (let* ((time (/ (get-internal-real-time) 1000.0))
             (center-x (/ display-width 2))
             (center-y (/ display-height 2))
             (radius 150)
             (x (+ center-x (* radius (cos time))))
             (y (+ center-y (* radius (sin time)))))
        ;; Update pixel window (smooth)
        (when *compare-window-pixel*
          (floating-window-set-pixel-position *compare-window-pixel*
                                              (round (- x win-width 20))
                                              (round y)))
        ;; Update char window (quantized)
        (when *compare-window-char*
          (let ((char-x (floor (+ x 20) char-width))
                (char-y (floor y char-height)))
            (lem-if:set-view-pos (implementation)
                                 (lem-core::window-view *compare-window-char*)
                                 char-x char-y)))
        (redraw-display)))))

(define-command pixel-demo-compare () ()
  "Start side-by-side comparison of pixel vs character positioning.
Two windows orbit the center - left uses pixel mode (smooth), right uses char mode (jerky)."
  (cleanup-demo)
  ;; Clean up compare-specific windows
  (when *compare-window-pixel*
    (ignore-errors (delete-window *compare-window-pixel*)))
  (when *compare-window-char*
    (ignore-errors (delete-window *compare-window-char*)))
  (when *compare-buffer-pixel*
    (ignore-errors (delete-buffer *compare-buffer-pixel*)))
  (when *compare-buffer-char*
    (ignore-errors (delete-buffer *compare-buffer-char*)))
  (setf *demo-mode* :compare)
  ;; Create pixel mode window
  (setf *compare-buffer-pixel* (make-demo-buffer "*Pixel Mode*" "  PIXEL MODE\n  (smooth)"))
  (setf *compare-window-pixel* (make-floating-window
                                :buffer *compare-buffer-pixel*
                                :x 10 :y 10
                                :width 16 :height 4
                                :pixel-x 100
                                :pixel-y 100
                                :use-border t))
  ;; Create char mode window
  (setf *compare-buffer-char* (make-demo-buffer "*Char Mode*" "  CHAR MODE\n  (quantized)"))
  (setf *compare-window-char* (make-floating-window
                               :buffer *compare-buffer-char*
                               :x 30 :y 10
                               :width 16 :height 4
                               :use-border t))
  (setf *demo-timer* (make-timer #'compare-animation-step :name "pixel-demo-compare"))
  (start-timer *demo-timer* 16 :repeat t)
  (message "Comparison started. Watch pixel (left) vs char (right) modes. M-x pixel-demo-stop to end."))

(define-command pixel-demo-stop () ()
  "Stop any running pixel demo."
  (when *demo-timer*
    (stop-timer *demo-timer*)
    (setf *demo-timer* nil))
  (when *demo-window*
    (ignore-errors (delete-window *demo-window*))
    (setf *demo-window* nil))
  (when *demo-buffer*
    (ignore-errors (delete-buffer *demo-buffer*))
    (setf *demo-buffer* nil))
  (when *compare-window-pixel*
    (ignore-errors (delete-window *compare-window-pixel*))
    (setf *compare-window-pixel* nil))
  (when *compare-window-char*
    (ignore-errors (delete-window *compare-window-char*))
    (setf *compare-window-char* nil))
  (when *compare-buffer-pixel*
    (ignore-errors (delete-buffer *compare-buffer-pixel*))
    (setf *compare-buffer-pixel* nil))
  (when *compare-buffer-char*
    (ignore-errors (delete-buffer *compare-buffer-char*))
    (setf *compare-buffer-char* nil))
  (setf *demo-mode* nil)
  (message "Demo stopped."))

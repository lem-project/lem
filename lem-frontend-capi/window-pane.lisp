(in-package :lem-capi)

(defvar *default-font-spec*
  #+win32 (cons "Consolas" 10)
  #+linux (cons "DejaVu Sans Mono" 10.5)
  #+macosx (cons "Osaka" 10))

(defclass window-pane (capi:output-pane)
  ((normal-font
    :initform nil
    :accessor window-pane-normal-font)
   (bold-font
    :initform nil
    :accessor window-pane-bold-font)
   (character-extent
    :initform nil
    :accessor window-pane-character-extent)
   (drawing-queue
    :initform nil
    :accessor window-pane-drawing-queue)
   (pixmap
    :initform nil
    :accessor window-pane-pixmap)
   (window
    :initform nil
    :initarg :window
    :reader window-pane-window)
   (window-panel
    :initarg :window-panel
    :reader window-pane-window-panel))
  (:default-initargs
   :foreground :black
   :background :white
   :input-model '((:gesture-spec input-key)
                  ((:button-1 :press) input-mouse-button :button-1 :press)
                  ((:button-1 :release) input-mouse-button :button-1 :release)
                  ((:button-1 :motion) input-mouse-button :button-1 :motion)
                  ((:button-2 :press) input-mouse-button :button-2 :press)
                  ((:button-3 :press) input-mouse-button :button-3 :press)
                  . #-win32 ()
                  #+win32 #.(loop :for code :from 1 :to 127
                                  :for char := (code-char code)
                                  :collect `((,char :press :meta) input-key)
                                  :collect `((,char :press :meta :control) input-key)
                                  :collect `((,char :press :meta :control :shift) input-key)))
   :resize-callback 'window-pane-resize-callback
   :display-callback 'window-pane-display-callback
   :composition-callback 'composition-callback
   :use-native-input-method t))

(let ((last-len 0))
  (defun composition-callback (window-pane what)
    (with-error-handler ()
      (setf window-pane (lem:window-view (lem:current-window)))
      (cond
        ((eq what :start) (setf last-len 0))
        ((eq what :end))
        ((listp what)
         (let ((x (lem:point-column (lem:current-point)))
               (y (lem:window-cursor-y (lem:current-window))))
           (clear-rectangle window-pane x y last-len 1)
           (loop :for (string face-plist) :in (getf what :string-face-lists)
                 :do (unless (string= "" string)
                       (let ((foreground (getf face-plist :foreground (capi:simple-pane-foreground window-pane)))
                             (background (getf face-plist :background (capi:simple-pane-background window-pane)))
                             #+(or)(font (getf face-plist :font))
                             #+(or)(italic-p (getf face-plist :italic-p))
                             (bold-p (getf face-plist :bold-p))
                             (underline-p (getf face-plist :underline-p)))
                         (%draw-string window-pane string x y foreground background
                                       underline-p bold-p nil)
                         (let ((width (lem:string-width string)))
                           (incf x width)
                           (setf last-len (max last-len width))))))
           (update-window window-pane)))))))

(defun input-key (window-pane x y gesture-spec)
  (declare (ignore window-pane x y))
  (with-error-handler ()
    (when-let (key (gesture-spec-to-key gesture-spec))
      (lem:send-event key))))

(defun input-mouse-button (window-pane x y button press-release)
  (case button
    (:button-1
     (case press-release
       (:press
        (multiple-value-bind (w h) (window-pane-char-size window-pane)
          (let ((window (window-pane-window window-pane))
                (x (floor x w))
                (y (floor y h)))
            (lem:send-event (lambda ()
                              (setf (lem:current-window) window)
                              (move-to-cursor window x y)
                              (lem:redraw-display))))))
       (:motion)
       (:release)))
    (:button-2)
    (:button-3)))

(defun move-to-cursor (window x y)
  (lem:move-point (lem:current-point) (lem::window-view-point window))
  (lem:move-to-next-virtual-line (lem:current-point) y)
  (lem:move-to-virtual-line-column (lem:current-point) x))

(defmacro with-drawing ((window-pane) &body body)
  (check-type window-pane symbol)
  `(push (lambda (,window-pane) ,@body)
         (window-pane-drawing-queue ,window-pane)))

(defun reinitialize-pixmap (window-pane)
  (when (window-pane-pixmap window-pane)
    (gp:destroy-pixmap-port (window-pane-pixmap window-pane)))
  (multiple-value-bind (w h)
      (capi:simple-pane-visible-size window-pane)
    (setf (window-pane-pixmap window-pane)
          (gp:create-pixmap-port window-pane w h :drawing-mode :quality))))

(defun update-window (window-pane)
  (with-apply-in-pane-process-wait-single (window-pane)
    (multiple-value-bind (w h) (capi:simple-pane-visible-size window-pane)
      (unless (null (window-pane-drawing-queue window-pane))
        (unless (window-pane-pixmap window-pane)
          (setf (window-pane-pixmap window-pane) (gp:create-pixmap-port window-pane w h :drawing-mode :quality)))
        (dolist (fn (nreverse (window-pane-drawing-queue window-pane)))
          (funcall fn (window-pane-pixmap window-pane)))
        (gp:copy-pixels window-pane (window-pane-pixmap window-pane) 0 0 w h 0 0)
        (setf (window-pane-drawing-queue window-pane) nil)))))

(defun window-pane-display-callback (window-pane &rest args)
  (declare (ignore args))
  (with-error-handler ()
    (when (window-pane-pixmap window-pane)
      (multiple-value-bind (w h) (capi:simple-pane-visible-size window-pane)
        (gp:copy-pixels window-pane (window-pane-pixmap window-pane) 0 0 w h 0 0)))))

(defun window-pane-resize-callback (window-pane &rest args)
  (declare (ignore args))
  (with-error-handler ()
    (let ((window-panel (window-pane-window-panel window-pane)))
      (when (window-panel-initialized window-panel)
        (window-panel-resize-callback window-panel)))))

(defun destroy-window-pane (window-pane)
  (gp:destroy-pixmap-port (window-pane-pixmap window-pane)))

(defun change-font (window-pane family size)
  (let ((normal-font (gp:find-best-font
                      window-pane
                      (gp:make-font-description :family family
                                                :size size
                                                :weight :normal)))
        (bold-font (gp:find-best-font
                    window-pane
                    (gp:make-font-description :family family
                                              :size size
                                              :weight :bold))))
    (setf (window-pane-normal-font window-pane) normal-font)
    (setf (window-pane-bold-font window-pane) bold-font)
    (setf (window-pane-character-extent window-pane)
          (multiple-value-list (gp:get-string-extent window-pane "a" normal-font)))))

(defun update-font-if-required (window-pane)
  (unless (window-pane-normal-font window-pane)
    (change-font window-pane
                 (car *default-font-spec*)
                 (cdr *default-font-spec*))))

(defun window-pane-char-size (window-pane)
  (update-font-if-required window-pane)
  (destructuring-bind (left top right bottom)
      (window-pane-character-extent window-pane)
    (values (- right left)
            (+ (abs top) bottom))))

(defun window-pane-char-width (window-pane)
  (window-pane-char-size window-pane))

(defun window-pane-char-height (window-pane)
  (nth-value 1 (window-pane-char-size window-pane)))

(defun window-pane-size (window-pane)
  (multiple-value-bind (width height)
      (capi:simple-pane-visible-size window-pane)
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (values (round width char-width)
              (round height char-height)))))

(defun window-pane-width (window-pane)
  (round (capi:simple-pane-visible-width window-pane)
         (window-pane-char-width window-pane)))

(defun window-pane-height (window-pane)
  (round (capi:simple-pane-visible-height window-pane)
         (window-pane-char-height window-pane)))

(defun %%draw-string (window-pane string pixel-x pixel-y foreground background underline bold reverse)
  (let ((font (if bold
                  (window-pane-bold-font window-pane)
                  (window-pane-normal-font window-pane))))
    (destructuring-bind (left top right bottom)
        (window-pane-character-extent window-pane)
      (let* ((char-width (- right left))
             (char-height (+ (abs top) bottom)))
        (when reverse (rotatef foreground background))
        (with-drawing (window-pane)
          (gp:draw-rectangle window-pane
                             pixel-x
                             pixel-y
                             (* (lem:string-width string) char-width)
                             char-height
                             :foreground background
                             :filled t)
          (loop :with x1 := pixel-x :and y1 := (+ pixel-y char-height (- bottom))
                :for c :across string
                :do (gp:draw-character window-pane c x1 y1
                                       :font font
                                       :foreground foreground)
                    (incf x1 (* char-width (if (lem:wide-char-p c) 2 1)))
                :finally (when underline
                           (gp:draw-line window-pane
                                         pixel-x
                                         (+ pixel-y char-height -4)
                                         x1
                                         (+ pixel-y char-height -4)
                                         :foreground foreground))))))))

(defun %draw-string (window-pane string x y foreground background underline bold reverse)
  (update-font-if-required window-pane)
  (destructuring-bind (left top right bottom)
      (window-pane-character-extent window-pane)
    (let* ((char-width (- right left))
           (char-height (+ (abs top) bottom))
           (x (* x char-width))
           (y (* y char-height)))
      (%%draw-string window-pane string x y foreground background underline bold reverse))))

(defun attribute-arguments (window-pane attribute)
  (if attribute
      (let ((foreground (convert-color (lem:attribute-foreground attribute)
                                       (capi:simple-pane-foreground window-pane)))
            (background (convert-color (lem:attribute-background attribute)
                                       (capi:simple-pane-background window-pane)))
            (underline (lem:attribute-underline-p attribute))
            (bold (lem:attribute-bold-p attribute))
            (reverse (lem:attribute-reverse-p attribute)))
        (list foreground
              background
              underline
              bold
              reverse))
      (list (capi:simple-pane-foreground window-pane)
            (capi:simple-pane-background window-pane)
            nil nil nil)))

(defun draw-string (window-pane string x y attribute)
  (with-apply-in-pane-process-wait-single (window-pane)
    (apply #'%draw-string window-pane string x y (attribute-arguments window-pane attribute))))

(defun draw-string-in-modeline (window-pane string x y attribute)
  (declare (ignore y))
  (with-apply-in-pane-process-wait-single (window-pane)
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (let ((x (* x char-width))
            (y (- (capi:simple-pane-visible-height window-pane) char-height)))
        (apply #'%%draw-string window-pane string x y
               (attribute-arguments window-pane attribute))))))

(defun draw-rectangle (window-pane x y width height color)
  (with-apply-in-pane-process-wait-single (window-pane)
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (let ((x (* x char-width))
            (y (* y char-height))
            (w (* width char-width))
            (h (* height char-height)))
        (with-drawing (window-pane)
          (gp:draw-rectangle window-pane
                             x y w h
                             :filled t
                             :foreground color))))))

(defun clear-rectangle (window-pane x y width height)
  (with-apply-in-pane-process-wait-single (window-pane)
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (let ((x (* x char-width))
            (y (* y char-height))
            (w (* width char-width))
            (h (* height char-height)))
        (with-drawing (window-pane)
          (gp:clear-rectangle window-pane
                              x y w h))))))

(defun clear-eol (window-pane x y)
  (with-apply-in-pane-process-wait-single (window-pane)
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (let* ((x (* x char-width))
             (y (* y char-height))
             (width (- (capi:simple-pane-visible-width window-pane) x)))
        (with-drawing (window-pane)
          (gp:clear-rectangle window-pane
                              x y width char-height))))))

(defun clear-eob (window-pane x y)
  (with-apply-in-pane-process-wait-single (window-pane)
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (let ((x (* x char-width))
            (y (* y char-height))
            (width (capi:simple-pane-visible-width window-pane))
            (height (capi:simple-pane-visible-height window-pane)))
        (with-drawing (window-pane)
          (gp:clear-rectangle window-pane
                              x y (- width x) char-height)
          (gp:clear-rectangle window-pane 0 (+ y char-height)
                              width
                              (- height
                                 y char-height char-height)))))))

(defun clear (window-pane)
  (with-apply-in-pane-process-wait-single (window-pane)
    (with-drawing (window-pane)
      (gp:clear-graphics-port window-pane))))

(defun change-foreground (window-pane color)
  (with-apply-in-pane-process-wait-single (window-pane)
    (when-let (color (convert-color color))
      (setf (capi:simple-pane-foreground window-pane) color))))

(defun change-background (window-pane color)
  (with-apply-in-pane-process-wait-single (window-pane)
    (when-let (color (convert-color color))
      (setf (capi:simple-pane-background window-pane) color))))

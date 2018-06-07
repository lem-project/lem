(in-package :lem-capi)

(defclass lem-pane (capi:output-pane)
  ((normal-font
    :initform nil
    :accessor lem-pane-normal-font)
   (bold-font
    :initform nil
    :accessor lem-pane-bold-font)
   (string-extent
    :initform nil
    :accessor lem-pane-string-extent)
   (pixmap
    :initform nil
    :accessor lem-pane-pixmap)
   (resized
    :initform nil
    :accessor lem-pane-resized))
  (:default-initargs
   :foreground :black
   :background :white
   :input-model '((:gesture-spec input-key)
                  . #-win32 ()
                  #+win32 #.(loop :for code :from 1 :to 127
                                  :for char := (code-char code)
                                  :collect `((,char :press :meta) input-key)
                                  :collect `((,char :press :meta :control) input-key)
                                  :collect `((,char :press :meta :control :shift) input-key)))
   :resize-callback 'resize-callback))

(defmethod capi:interface-keys-style ((lem-pane lem-pane)) :emacs)

(defun resize-callback (lem-pane &rest args)
  (declare (ignore args))
  (unless (lem-pane-resized lem-pane)
    (setf (lem-pane-resized lem-pane) t)
    (mp:schedule-timer-relative (mp:make-timer 'resize-if-required lem-pane) 0.1)))

(defun resize-if-required (lem-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (when (lem-pane-resized lem-pane)
      (setf (lem-pane-resized lem-pane) nil)
      (lem:send-event :resize)
      (reinitialize-pixmap lem-pane)
      (gp:clear-graphics-port (lem-pane-pixmap lem-pane)))))

(defun update-font-if-required (lem-pane)
  (unless (lem-pane-normal-font lem-pane)
    (change-font lem-pane (car *default-font-spec*) (cdr *default-font-spec*))))

(defun change-font (lem-pane family size)
  (let ((normal-font (gp:find-best-font
                      lem-pane
                      (gp:make-font-description :family family
                                                :size size
                                                :weight :normal)))
        (bold-font (gp:find-best-font
                    lem-pane
                    (gp:make-font-description :family family
                                              :size size
                                              :weight :bold))))
    (setf (lem-pane-normal-font lem-pane) normal-font)
    (setf (lem-pane-bold-font lem-pane) bold-font)
    (setf (lem-pane-string-extent lem-pane)
          (multiple-value-list (gp:get-string-extent lem-pane "a" normal-font)))))

(defun reinitialize-pixmap (lem-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (when-let (pixmap (lem-pane-pixmap lem-pane))
      (gp:destroy-pixmap-port pixmap))
    (setf (lem-pane-pixmap lem-pane)
          (gp:create-pixmap-port lem-pane
                                 (capi:simple-pane-visible-width lem-pane)
                                 (capi:simple-pane-visible-height lem-pane)
                                 :background (capi:simple-pane-background lem-pane)))))

(defun update-display (lem-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (gp:copy-pixels lem-pane (lem-pane-pixmap lem-pane)
                    0 0
                    (capi:simple-pane-visible-width lem-pane)
                    (capi:simple-pane-visible-height lem-pane)
                    0 0)))

(defun lem-pane-char-size (lem-pane)
  (update-font-if-required lem-pane)
  (destructuring-bind (left top right bottom)
      (lem-pane-string-extent lem-pane)
    (values (- right left)
            (+ (abs top) bottom))))

(defun lem-pane-char-width (lem-pane)
  (nth-value 0 (lem-pane-char-size lem-pane)))

(defun lem-pane-char-height (lem-pane)
  (nth-value 1 (lem-pane-char-size lem-pane)))

(defun lem-pane-width (lem-pane)
  (values
   (floor (capi:simple-pane-visible-width lem-pane)
          (lem-pane-char-width lem-pane))))

(defun lem-pane-height (lem-pane)
  (values
   (floor (capi:simple-pane-visible-height lem-pane)
          (lem-pane-char-height lem-pane))))

(defun convert-color (color &optional default-color)
  (if-let (rgb (lem:parse-color color))
    (let ((n (/ 1.0 255)))
      (destructuring-bind (r g b) rgb
        (color:make-rgb (* r n) (* g n) (* b n))))
    default-color))

(defun draw-string (lem-pane string x y foreground background &key underline bold reverse)
  (when reverse (rotatef foreground background))
  (update-font-if-required lem-pane)
  (let ((font (if bold
                  (lem-pane-bold-font lem-pane)
                  (lem-pane-normal-font lem-pane))))
    (destructuring-bind (left top right bottom)
        (lem-pane-string-extent lem-pane)
      (let* ((char-width (- right left))
             (char-height (+ (abs top) bottom))
             (x (* x char-width))
             (y (* y char-height)))
        (let ((x1 x)
              (y1 (+ y char-height (- bottom))))
          (let ((w (* char-width (lem:string-width string)))
                (h char-height))
            (gp:draw-rectangle (lem-pane-pixmap lem-pane) x y w h
                               :foreground background
                               :filled t))
          (loop :for c :across string
                :do (gp:draw-character (lem-pane-pixmap lem-pane) c x1 y1
                                       :font font
                                       :foreground foreground
                                       :background background
                                       :block t)
                    (incf x1 (* char-width (if (lem:wide-char-p c) 2 1)))
                :finally (when underline
                           (gp:draw-line (lem-pane-pixmap lem-pane)
                                         x
                                         (+ y char-height -4)
                                         x1
                                         (+ y char-height -4)
                                         :foreground foreground))))))))

(defun draw-text (lem-pane string x y attribute)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (if attribute
        (let ((foreground (convert-color (lem:attribute-foreground attribute)
                                         (capi:simple-pane-foreground lem-pane)))
              (background (convert-color (lem:attribute-background attribute)
                                         (capi:simple-pane-background lem-pane)))
              (underline-p (lem:attribute-underline-p attribute))
              (bold-p (lem:attribute-bold-p attribute))
              (reverse-p (lem:attribute-reverse-p attribute)))
          (draw-string lem-pane string x y foreground background
                       :underline underline-p
                       :bold bold-p
                       :reverse reverse-p))
        (draw-string lem-pane string x y
                     (capi:simple-pane-foreground lem-pane)
                     (capi:simple-pane-background lem-pane)))))

(defun draw-rectangle (lem-pane x y width height &optional color)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (multiple-value-bind (char-width char-height)
        (lem-pane-char-size lem-pane)
      (let ((x (* x char-width))
            (y (* y char-height))
            (w (* width char-width))
            (h (* height char-height)))
        (if color
            (gp:draw-rectangle (lem-pane-pixmap lem-pane)
                               x y w h
                               :filled t
                               :foreground color)
            (gp:clear-rectangle (lem-pane-pixmap lem-pane)
                                x y w h))))))

(defun change-foreground (lem-pane color)
  (when-let (color (convert-color color nil))
    (setf (capi:simple-pane-foreground lem-pane) color)))

(defun change-background (lem-pane color)
  (when-let (color (convert-color color nil))
    (setf (capi:simple-pane-background lem-pane) color)))

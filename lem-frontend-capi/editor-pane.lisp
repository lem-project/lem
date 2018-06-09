(in-package :lem-capi)

(defclass editor-pane (capi:output-pane)
  ((normal-font
    :initform nil
    :accessor editor-pane-normal-font)
   (bold-font
    :initform nil
    :accessor editor-pane-bold-font)
   (string-extent
    :initform nil
    :accessor editor-pane-string-extent)
   (pixmap
    :initform nil
    :accessor editor-pane-pixmap)
   (resized
    :initform nil
    :accessor editor-pane-resized))
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
   :resize-callback 'resize-callback
   :display-callback 'display-callback))

(defmethod capi:interface-keys-style ((editor-pane editor-pane)) :emacs)

(defun display-callback (editor-pane &rest args)
  (declare (ignore args))
  (update-display editor-pane))

(defun resize-callback (editor-pane &rest args)
  (declare (ignore args))
  (unless (editor-pane-resized editor-pane)
    (setf (editor-pane-resized editor-pane) t)
    (mp:schedule-timer-relative (mp:make-timer 'resize-if-required editor-pane) 0.1)))

(defun resize-if-required (editor-pane)
  (with-apply-in-pane-process-wait-single (editor-pane)
    (when (editor-pane-resized editor-pane)
      (setf (editor-pane-resized editor-pane) nil)
      (lem:send-event :resize)
      (reinitialize-pixmap editor-pane)
      (gp:clear-graphics-port (editor-pane-pixmap editor-pane)))))

(defun update-font-if-required (editor-pane)
  (unless (editor-pane-normal-font editor-pane)
    (change-font editor-pane (car *default-font-spec*) (cdr *default-font-spec*))))

(defun change-font (editor-pane family size)
  (let ((normal-font (gp:find-best-font
                      editor-pane
                      (gp:make-font-description :family family
                                                :size size
                                                :weight :normal)))
        (bold-font (gp:find-best-font
                    editor-pane
                    (gp:make-font-description :family family
                                              :size size
                                              :weight :bold))))
    (setf (editor-pane-normal-font editor-pane) normal-font)
    (setf (editor-pane-bold-font editor-pane) bold-font)
    (setf (editor-pane-string-extent editor-pane)
          (multiple-value-list (gp:get-string-extent editor-pane "a" normal-font)))))

(defun reinitialize-pixmap (editor-pane)
  (with-apply-in-pane-process-wait-single (editor-pane)
    (when-let (pixmap (editor-pane-pixmap editor-pane))
      (gp:destroy-pixmap-port pixmap))
    (setf (editor-pane-pixmap editor-pane)
          (gp:create-pixmap-port editor-pane
                                 (capi:simple-pane-visible-width editor-pane)
                                 (capi:simple-pane-visible-height editor-pane)
                                 :background (capi:simple-pane-background editor-pane)))))

(defun update-display (editor-pane)
  (with-apply-in-pane-process-wait-single (editor-pane)
    (gp:copy-pixels editor-pane (editor-pane-pixmap editor-pane)
                    0 0
                    (capi:simple-pane-visible-width editor-pane)
                    (capi:simple-pane-visible-height editor-pane)
                    0 0)))

(defun editor-pane-char-size (editor-pane)
  (update-font-if-required editor-pane)
  (destructuring-bind (left top right bottom)
      (editor-pane-string-extent editor-pane)
    (values (- right left)
            (+ (abs top) bottom))))

(defun editor-pane-char-width (editor-pane)
  (nth-value 0 (editor-pane-char-size editor-pane)))

(defun editor-pane-char-height (editor-pane)
  (nth-value 1 (editor-pane-char-size editor-pane)))

(defun editor-pane-width (editor-pane)
  (values
   (floor (capi:simple-pane-visible-width editor-pane)
          (editor-pane-char-width editor-pane))))

(defun editor-pane-height (editor-pane)
  (values
   (floor (capi:simple-pane-visible-height editor-pane)
          (editor-pane-char-height editor-pane))))

(defun convert-color (color &optional default-color)
  (if-let (rgb (lem:parse-color color))
    (let ((n (/ 1.0 255)))
      (destructuring-bind (r g b) rgb
        (color:make-rgb (* r n) (* g n) (* b n))))
    default-color))

(defun draw-string (editor-pane string x y foreground background &key underline bold reverse)
  (when reverse (rotatef foreground background))
  (update-font-if-required editor-pane)
  (let ((font (if bold
                  (editor-pane-bold-font editor-pane)
                  (editor-pane-normal-font editor-pane))))
    (destructuring-bind (left top right bottom)
        (editor-pane-string-extent editor-pane)
      (let* ((char-width (- right left))
             (char-height (+ (abs top) bottom))
             (x (* x char-width))
             (y (* y char-height)))
        (let ((x1 x)
              (y1 (+ y char-height (- bottom))))
          (let ((w (* char-width (lem:string-width string)))
                (h char-height))
            (gp:draw-rectangle (editor-pane-pixmap editor-pane) x y w h
                               :foreground background
                               :filled t))
          (loop :for c :across string
                :do (gp:draw-character (editor-pane-pixmap editor-pane) c x1 y1
                                       :font font
                                       :foreground foreground
                                       :background background
                                       :block t)
                    (incf x1 (* char-width (if (lem:wide-char-p c) 2 1)))
                :finally (when underline
                           (gp:draw-line (editor-pane-pixmap editor-pane)
                                         x
                                         (+ y char-height -4)
                                         x1
                                         (+ y char-height -4)
                                         :foreground foreground))))))))

(defun draw-text (editor-pane string x y attribute)
  (with-apply-in-pane-process-wait-single (editor-pane)
    (if attribute
        (let ((foreground (convert-color (lem:attribute-foreground attribute)
                                         (capi:simple-pane-foreground editor-pane)))
              (background (convert-color (lem:attribute-background attribute)
                                         (capi:simple-pane-background editor-pane)))
              (underline-p (lem:attribute-underline-p attribute))
              (bold-p (lem:attribute-bold-p attribute))
              (reverse-p (lem:attribute-reverse-p attribute)))
          (draw-string editor-pane string x y foreground background
                       :underline underline-p
                       :bold bold-p
                       :reverse reverse-p))
        (draw-string editor-pane string x y
                     (capi:simple-pane-foreground editor-pane)
                     (capi:simple-pane-background editor-pane)))))

(defun draw-rectangle (editor-pane x y width height &optional color)
  (with-apply-in-pane-process-wait-single (editor-pane)
    (multiple-value-bind (char-width char-height)
        (editor-pane-char-size editor-pane)
      (let ((x (* x char-width))
            (y (* y char-height))
            (w (* width char-width))
            (h (* height char-height)))
        (if color
            (gp:draw-rectangle (editor-pane-pixmap editor-pane)
                               x y w h
                               :filled t
                               :foreground color)
            (gp:clear-rectangle (editor-pane-pixmap editor-pane)
                                x y w h))))))

(defun change-foreground (editor-pane color)
  (when-let (color (convert-color color nil))
    (setf (capi:simple-pane-foreground editor-pane) color)))

(defun change-background (editor-pane color)
  (when-let (color (convert-color color nil))
    (setf (capi:simple-pane-background editor-pane) color)))

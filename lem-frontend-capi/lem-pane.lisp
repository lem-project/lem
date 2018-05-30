(defpackage :lem-capi.lem-pane
  (:add-use-defaults t)
  (:use :cl :lem-capi.util)
  (:export
   :reinitialize-pixmap
   :lem-pane
   :lem-pane-width
   :lem-pane-height
   :update-display
   :draw-rectangle
   :draw-text
   :clear
   :change-foreground
   :change-background))
(in-package :lem-capi.lem-pane)

(defvar *default-font-family* #+win32 "Consolas" #-win32 "DejaVu Sans Mono")
(defvar *default-font-size* 10)

(defvar *default-font*
  (gp:make-font-description :size *default-font-size*
                            :family *default-font-family*))

(defvar *default-bold-font*
  (gp:make-font-description :size *default-font-size*
                            :family *default-font-family*
                            :weight :bold))

(defclass lem-pane (capi:output-pane)
  ((font
    :initform *default-font*
    :accessor lem-pane-font)
   (bold-font
    :initform *default-bold-font*
    :accessor lem-pane-bold-font)
   (pixmap
    :initform nil
    :accessor lem-pane-pixmap))
  (:default-initargs
   :font *default-font*
   :foreground :black
   :background :white
   :input-model '((:gesture-spec key-press)
                  . #-win32 ()
                  #+win32 #.(loop :for code :from 1 :to 127
                                  :for char := (code-char code)
                                  :collect `((,char :press :meta) key-press)
                                  :collect `((,char :press :meta :control) key-press)
                                  :collect `((,char :press :meta :control :shift) key-press)))
   :resize-callback 'resize-callback))

(defmethod capi:interface-keys-style ((lem-pane lem-pane)) :emacs)

(defmethod (setf capi:simple-pane-font) :after (font (lem-pane lem-pane))
  (setf (lem-pane-font lem-pane) font)
  (setf (lem-pane-bold-font lem-pane)
        (gp:make-font-description :size (gp:font-description-attribute-value font :size)
                                  :family (gp:font-description-attribute-value font :family)
                                  :weight :bold)))

(defun resize-callback (lem-pane &rest args)
  (declare (ignore args))
  (lem:send-event :resize)
  (reinitialize-pixmap lem-pane))

(defun reinitialize-pixmap (lem-pane)
  (capi:apply-in-pane-process-wait-single
   lem-pane
   nil
   (lambda (lem-pane)
     (when-let (pixmap (lem-pane-pixmap lem-pane))
       (gp:destroy-pixmap-port pixmap))
     (setf (lem-pane-pixmap lem-pane)
           (gp:create-pixmap-port lem-pane
                                  (capi:simple-pane-visible-width lem-pane)
                                  (capi:simple-pane-visible-height lem-pane)
                                  :background (capi:simple-pane-background lem-pane))))
   lem-pane))

(defun update-display (lem-pane)
  (capi:apply-in-pane-process-wait-single
   lem-pane
   nil
   (lambda ()
     (gp:copy-pixels lem-pane (lem-pane-pixmap lem-pane)
                     0 0
                     (capi:simple-pane-visible-width lem-pane)
                     (capi:simple-pane-visible-height lem-pane)
                     0 0))))

(defun lem-pane-char-size (lem-pane)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent lem-pane "a")
    (values (- right left)
            (+ (abs top) bottom))))

(defun lem-pane-char-width (lem-pane)
  (nth-value 0 (lem-pane-char-size lem-pane)))

(defun lem-pane-char-height (lem-pane)
  (nth-value 1 (lem-pane-char-size lem-pane)))

(defun shift-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-shift-bit)))

(defun control-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-control-bit)))

(defun meta-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-meta-bit)))

(defun gesture-spec-to-key (gesture-spec)
  (when (sys:gesture-spec-p gesture-spec)
    (let* ((data (sys:gesture-spec-data gesture-spec))
           (modifiers (sys:gesture-spec-modifiers gesture-spec))
           (shiftp (shift-bit-p modifiers))
           (ctrlp (control-bit-p modifiers))
           (metap (meta-bit-p modifiers))
           (sym (typecase data
                  (string data)
                  (keyword (string-capitalize data))
                  (integer
                   (let ((char (code-char data)))
                     (cond ((char= char #\Return)
                            "Return")
                           ((char= char #\Tab)
                            "Tab")
                           ((char= char #\Escape)
                            "Escape")
                           ((char= char #\Backspace)
                            "Backspace")
                           (t
                            (string char))))))))
      (when sym
        (cond ((and (not metap) ctrlp (not shiftp) (string= sym "i"))
               (lem:make-key :sym "Tab"))
              (t
               (lem:make-key :meta metap
                             :ctrl ctrlp
                             :shift shiftp
                             :sym sym)))))))

(defun key-press (self x y gesture-spec)
  (declare (ignore self x y))
  (with-error-handler ()
    (alexandria:when-let ((key (gesture-spec-to-key gesture-spec)))
      (lem:send-event key))))

(defun lem-pane-width (lem-pane)
  (values
   (floor (capi:simple-pane-visible-width lem-pane)
          (lem-pane-char-width lem-pane))))

(defun lem-pane-height (lem-pane)
  (values
   (floor (capi:simple-pane-visible-height lem-pane)
          (lem-pane-char-height lem-pane))))

(defun convert-color (color default-color)
  (alexandria:if-let ((rgb (lem:parse-color color)))
      (let ((n (/ 1.0 255)))
        (destructuring-bind (r g b) rgb
          (color:make-rgb (* r n) (* g n) (* b n))))
    default-color))

(defun draw-string (lem-pane string x y foreground background &key underline bold reverse)
  (when reverse (rotatef foreground background))
  (let ((font (gp:find-best-font
               lem-pane
               (if bold
                   (lem-pane-bold-font lem-pane)
                   (lem-pane-font lem-pane)))))
    (multiple-value-bind (left top right bottom)
        (gp:get-string-extent lem-pane "a" font)
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
                    (incf x1 (* char-width (if (lem:wide-char-p c) 2 1)))))
        (when underline
          (gp:draw-line (lem-pane-pixmap lem-pane)
                        x
                        (+ y char-height -4)
                        (+ x (* char-width (length string)))
                        (+ y char-height -4)
                        :foreground foreground))))))

(defun draw-text (lem-pane string x y attribute)
  (capi:apply-in-pane-process-wait-single
   lem-pane
   nil
   (lambda ()
     (if attribute
         (let ((foreground (or (convert-color (lem:attribute-foreground attribute) nil)
                               (capi:simple-pane-foreground lem-pane)))
               (background (or (convert-color (lem:attribute-background attribute) nil)
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
                      (capi:simple-pane-background lem-pane))))))

(defun draw-rectangle (lem-pane x y width height &optional color)
  (capi:apply-in-pane-process-wait-single
   lem-pane
   nil
   (lambda ()
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
                                 x y w h)))))))

(defun clear (lem-pane)
  (capi:apply-in-pane-process-wait-single
   lem-pane
   nil
   (lambda ()
     (gp:clear-graphics-port (lem-pane-pixmap lem-pane)))))

(defun change-foreground (lem-pane color)
  (alexandria:when-let ((color (convert-color color nil)))
    (setf (capi:simple-pane-foreground lem-pane) color)))

(defun change-background (lem-pane color)
  (alexandria:when-let ((color (convert-color color nil)))
    (setf (capi:simple-pane-background lem-pane) color)))

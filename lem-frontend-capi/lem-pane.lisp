(defpackage :lem-capi.lem-pane
  (:use :cl :lem-capi.util)
  (:export
   :lem-pane
   :lem-pane-width
   :lem-pane-height
   :draw-rectangle
   :draw-text
   :clear))
(in-package :lem-capi.lem-pane)

(defvar *default-font-family* "DejaVu Sans Mono")
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
    :accessor lem-pane-bold-font))
  (:default-initargs
   :font *default-font*
   :foreground :black
   :background :white
   :input-model '((:gesture-spec key-press))
   :resize-callback 'resize-callback))

(defmethod (setf capi:simple-pane-font) :after (font (lem-pane lem-pane))
  (setf (lem-pane-font lem-pane) font)
  (setf (lem-pane-bold-font lem-pane)
        (gp:make-font-description :size (gp:font-description-attribute-value font :size)
                                  :family (gp:font-description-attribute-value font :family)
                                  :weight :bold)))

(defun resize-callback (&rest args)
  (lem:send-event :resize))

(defun lem-pane-char-width (lem-pane)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent lem-pane "a")
    (declare (ignore top bottom))
    (- right left)))

(defun lem-pane-char-height (lem-pane)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent lem-pane "a")
    (declare (ignore left right))
    (+ (abs top) bottom)))

(defun shift-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-shift-bit)))

(defun control-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-control-bit)))

(defun meta-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-meta-bit)))

(defun gesture-spec-to-key (self gesture-spec)
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
        (lem:make-key :meta metap
                      :ctrl ctrlp
                      :shift shiftp
                      :sym sym)))))

(defun key-press (self x y gesture-spec)
  (declare (ignore self x y))
  (with-error-handler ()
    (alexandria:when-let ((key (gesture-spec-to-key self gesture-spec)))
      (lem:send-event key))))

(defun lem-pane-width (lem-pane)
  (floor (capi:simple-pane-visible-width lem-pane)
         (lem-pane-char-width lem-pane)))

(defun lem-pane-height (lem-pane)
  (floor (capi:simple-pane-visible-height lem-pane)
         (lem-pane-char-height lem-pane)))

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
        (gp:draw-rectangle lem-pane
                           x
                           y
                           (* char-width (lem:string-width string))
                           char-height
                           :filled t
                           :foreground background)
        (gp:draw-string lem-pane string x (+ y char-height (- bottom))
                        :font font
                        :foreground foreground :background background
                        :block t)
        (when underline
          (gp:draw-line lem-pane x (+ y char-height) (+ x char-width) (+ y char-height)
                        :foreground foreground))))))

(defun draw-text (lem-pane string x y attribute)
  (capi:apply-in-pane-process
   lem-pane
   (lambda ()
     (if attribute
         (let ((foreground (or (convert-color (lem:attribute-foreground attribute) nil)
                               (color:make-rgb 0.0 0.0 0.0)))
               (background (or (convert-color (lem:attribute-background attribute) nil)
                               (color:make-rgb 1.0 1.0 1.0)))
               (underline-p (lem:attribute-underline-p attribute))
               (bold-p (lem:attribute-bold-p attribute))
               (reverse-p (lem:attribute-reverse-p attribute)))
           (draw-string lem-pane string x y foreground background
                        :underline underline-p
                        :bold bold-p
                        :reverse reverse-p))
         (draw-string lem-pane string x y :black :white)))))

(defun draw-rectangle (lem-pane x y width height color)
  (capi:apply-in-pane-process
   lem-pane
   (lambda ()
     (let ((char-width (lem-pane-char-width lem-pane))
           (char-height (lem-pane-char-height lem-pane)))
       (gp:draw-rectangle lem-pane
                          (* x char-width)
                          (* y char-height)
                          (* width char-width)
                          (* height char-height)
                          :foreground color
                          :filled t)))))

(defun clear (lem-pane)
  (capi:apply-in-pane-process
   lem-pane
   (lambda ()
     (gp:clear-graphics-port lem-pane))))

(in-package :lem-lispworks)

(defvar *default-font-spec*
  #+win32 (cons "Consolas" 10)
  #+linux (cons "DejaVu Sans Mono" 10.5)
  #+macosx (cons "Osaka" 10))

(defun convert-color (color &optional default-color)
  (if-let (rgb (lem:parse-color color))
    (let ((n (/ 1.0 255)))
      (destructuring-bind (r g b) rgb
        (color:make-rgb (* r n) (* g n) (* b n))))
    default-color))

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
   (window
    :initform nil
    :initarg :window
    :reader window-pane-window)
   (lem-pane
    :initarg :lem-pane
    :reader window-pane-lem-pane))
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
   :resize-callback 'window-pane-resize-callback
   :display-callback 'window-pane-display-callback))

(defmacro with-drawing ((window-pane) &body body)
  (check-type window-pane symbol)
  `(push (lambda (,window-pane) ,@body)
         (window-pane-drawing-queue ,window-pane)))

(defun update-window (window-pane)
  (with-apply-in-pane-process-wait-single (window-pane)
    (dolist (fn (nreverse (window-pane-drawing-queue window-pane)))
      (funcall fn window-pane))
    (setf (window-pane-drawing-queue window-pane) nil)))

(defun window-pane-display-callback (window-pane &rest args)
  (declare (ignore window-pane args)))

(defun window-pane-resize-callback (window-pane &rest args)
  (declare (ignore args))
  (with-error-handler ()
    (let ((lem-pane (window-pane-lem-pane window-pane)))
      (when (lem-pane-initialized lem-pane)
        (lem-pane-resize-callback lem-pane)))))

(defun destroy-window-pane (window-pane)
  (declare (ignore window-pane)))

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
          (let ((string-width (* (lem:string-width string) char-width)))
            (cond ((= (length string) string-width)
                   (gp:draw-rectangle window-pane pixel-x pixel-y string-width char-height
                                      :foreground background
                                      :filled t)
                   (gp:draw-string window-pane string pixel-x (+ pixel-y char-height (- bottom))
                                   :font font
                                   :foreground foreground
                                   :background background
                                   :block t)
                   (when underline
                     (gp:draw-line window-pane
                                   pixel-x
                                   (+ pixel-y char-height -4)
                                   (+ pixel-x string-width)
                                   (+ pixel-y char-height -4)
                                   :foreground foreground)))
                  (t
                   (loop :with x1 := pixel-x :and y1 := (+ pixel-y char-height (- bottom))
                         :for c :across string
                         :do (gp:draw-character window-pane c x1 y1
                                                :font font
                                                :foreground foreground
                                                :background background
                                                :block t)
                         (incf x1 (* char-width (if (lem:wide-char-p c) 2 1)))
                         :finally (when underline
                                    (gp:draw-line window-pane
                                                  pixel-x
                                                  (+ pixel-y char-height -4)
                                                  x1
                                                  (+ pixel-y char-height -4)
                                                  :foreground foreground)))))))))))

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

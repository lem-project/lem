(defpackage :lem-ncurses/text-buffer-impl
  (:use :cl)
  (:import-from :lem-core
                :control-character-object
                :cursor-attribute-p
                :emoji-object
                :eol-cursor-object
                :eol-cursor-object-color
                :extend-to-eol-object
                :extend-to-eol-object-color
                :folder-object
                :icon-object
                :image-object
                :image-object-height
                :image-object-image
                :image-object-width
                :line-end-object
                :line-end-object-offset
                :text-object
                :text-object-attribute
                :text-object-string
                :text-object-surface
                :text-object-type
                :void-object
                :window-view-height
                :window-view-width
                :text-object))
(in-package :lem-ncurses/text-buffer-impl)

(defgeneric object-width (drawing-object))

(defmethod object-width ((drawing-object void-object))
  0)

(defmethod object-width ((drawing-object text-object))
  (lem-core:string-width (text-object-string drawing-object)))

(defmethod object-width ((drawing-object eol-cursor-object))
  0)

(defmethod object-width ((drawing-object extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object line-end-object))
  0)

(defmethod object-width ((drawing-object image-object))
  0)

(defmethod lem-if:view-width ((implementation lem-ncurses::ncurses) view)
  (lem-ncurses::ncurses-view-width view))

(defmethod lem-if:view-height ((implementation lem-ncurses::ncurses) view)
  (lem-ncurses::ncurses-view-height view))

(defgeneric draw-object (object x y window))

(defmethod draw-object ((object void-object) x y window)
  (values))

(defmethod draw-object ((object text-object) x y window)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (when (and attribute (cursor-attribute-p attribute))
      (lem-core::set-last-print-cursor window x y))
    (lem-if:print (lem-core:implementation)
                  (lem-core::window-view window)
                  x
                  y
                  string
                  attribute)))

(defun color-to-hex-string (color)
  (format nil "#~2,'0X~2,'0X~2,'0X"
          (lem:color-red color)
          (lem:color-green color)
          (lem:color-blue color)))

(defmethod draw-object ((object eol-cursor-object) x y window)
  (lem-core::set-last-print-cursor window x y)
  (lem-if:print (lem:implementation)
                (lem:window-view window)
                x
                y
                " "
                (lem:make-attribute :foreground
                                    (color-to-hex-string (eol-cursor-object-color object)))))

(defmethod draw-object ((object extend-to-eol-object) x y window)
  (lem-if:print (lem:implementation)
                (lem:window-view window)
                x
                y
                (make-string (- (lem:window-width window) x) :initial-element #\space)
                (lem:make-attribute :background
                                    (color-to-hex-string (extend-to-eol-object-color object)))))

(defmethod draw-object ((object line-end-object) x y window)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (lem-if:print (lem-core:implementation)
                  (lem-core::window-view window)
                  (+ x (line-end-object-offset object))
                  y
                  string
                  attribute)))

(defmethod draw-object ((object image-object) x y window)
  (values))

(defmethod lem-if:render-line ((implementation lem-ncurses::ncurses) window x y objects height)
  (let ((view (lem:window-view window)))
    (charms/ll:wmove (lem-ncurses::ncurses-view-scrwin view) y x)
    (charms/ll:wclrtoeol (lem-ncurses::ncurses-view-scrwin view))
    (loop :for object :in objects
          :do (draw-object object x y window)
              (incf x (object-width object)))))

(defmethod lem-if:object-width ((implementation lem-ncurses::ncurses) drawing-object)
  (object-width drawing-object))

(defmethod lem-if:object-height ((implementation lem-ncurses::ncurses) drawing-object)
  1)

(defmethod lem-if:clear-to-end-of-window ((implementation lem-ncurses::ncurses) window y)
  (let* ((view (lem-core::window-view window))
         (win (lem-ncurses::ncurses-view-scrwin view)))
    (unless (= y (lem-if:view-height (lem:implementation) view))
      (charms/ll:wmove win y 0)
      (charms/ll:wclrtobot win))))

(defmethod lem-if:get-char-width ((implementation lem-ncurses::ncurses))
  1)

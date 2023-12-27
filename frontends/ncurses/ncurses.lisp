(uiop:define-package :lem-ncurses
  (:use :cl)
  (:use-reexport :lem-ncurses/internal))
(in-package :lem-ncurses)

(defmethod lem-if:invoke ((implementation lem-ncurses/internal::ncurses) function)
  (lem-ncurses/internal::invoke function))

(defmethod lem-if:get-background-color ((implementation lem-ncurses/internal::ncurses))
  (lem-ncurses/internal::get-background-color))

(defmethod lem-if:update-foreground ((implementation lem-ncurses/internal::ncurses) color-name)
  (lem-ncurses/internal::update-foreground-color color-name))

(defmethod lem-if:update-cursor-shape ((implementation lem-ncurses/internal::ncurses) cursor-type)
  (lem-ncurses/internal::update-cursor-shape cursor-type))

(defmethod lem-if:update-background ((implementation lem-ncurses/internal::ncurses) color-name)
  (lem-ncurses/internal::update-background-color color-name))

(defmethod lem-if:display-width ((implementation lem-ncurses/internal::ncurses))
  (lem-ncurses/internal::get-display-width))

(defmethod lem-if:display-height ((implementation lem-ncurses/internal::ncurses))
  (lem-ncurses/internal::get-display-height))

(defmethod lem-if:make-view
    ((implementation lem-ncurses/internal::ncurses) window x y width height use-modeline)
  (lem-ncurses/internal::make-view window x y width height use-modeline))

(defmethod lem-if:delete-view ((implementation lem-ncurses/internal::ncurses) view)
  (lem-ncurses/internal::delete-view view))

(defmethod lem-if:clear ((implementation lem-ncurses/internal::ncurses) view)
  (lem-ncurses/internal::clear view))

(defmethod lem-if:set-view-size ((implementation lem-ncurses/internal::ncurses) view width height)
  (lem-ncurses/internal::set-view-size view width height))

(defmethod lem-if:set-view-pos ((implementation lem-ncurses/internal::ncurses) view x y)
  (lem-ncurses/internal::set-view-pos view x y))

(defmethod lem-if:redraw-view-after ((implementation lem-ncurses/internal::ncurses) view)
  (lem-ncurses/internal::redraw-view-after view))

(defmethod lem-if:update-display ((implementation lem-ncurses/internal::ncurses))
  (lem-ncurses/internal::update-display))

(defmethod lem-if:clipboard-paste ((implementation lem-ncurses/internal::ncurses))
  (lem-ncurses/clipboard:paste))

(defmethod lem-if:clipboard-copy ((implementation lem-ncurses/internal::ncurses) text)
  (lem-ncurses/clipboard:copy text))

(defmethod lem-if:view-width ((implementation lem-ncurses/internal::ncurses) view)
  (lem-ncurses/internal::ncurses-view-width view))

(defmethod lem-if:view-height ((implementation lem-ncurses/internal::ncurses) view)
  (lem-ncurses/internal::ncurses-view-height view))

(defmethod lem-if:render-line ((implementation lem-ncurses/internal::ncurses)
                               view x y objects height)
  (lem-ncurses/drawing::render-line view x y objects))

(defmethod lem-if:render-line-on-modeline ((implementation lem-ncurses/internal::ncurses)
                                           view
                                           left-objects
                                           right-objects
                                           default-attribute
                                           height)
  (lem-ncurses/drawing::render-line-on-modeline view left-objects right-objects default-attribute))

(defmethod lem-if:object-width ((implementation lem-ncurses/internal::ncurses) drawing-object)
  (lem-ncurses/drawing::object-width drawing-object))

(defmethod lem-if:object-height ((implementation lem-ncurses/internal::ncurses) drawing-object)
  (lem-ncurses/drawing::object-height drawing-object))

(defmethod lem-if:clear-to-end-of-window ((implementation lem-ncurses/internal::ncurses) view y)
  (lem-ncurses/drawing::clear-to-end-of-window view y))

(defmethod lem-if:get-char-width ((implementation lem-ncurses/internal::ncurses))
  (lem-ncurses/drawing::get-char-width))

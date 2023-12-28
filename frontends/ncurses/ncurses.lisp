(uiop:define-package :lem-ncurses
  (:use :cl)
  (:use-reexport :lem-ncurses/internal))
(in-package :lem-ncurses)

(pushnew :lem-ncurses *features*)

(defclass ncurses (lem:implementation)
  ()
  (:default-initargs
   :name :ncurses
   :redraw-after-modifying-floating-window t))

(defmethod lem-if:invoke ((implementation ncurses) function)
  (lem-ncurses/internal::invoke function))

(defmethod lem-if:get-background-color ((implementation ncurses))
  (lem-ncurses/internal::get-background-color))

(defmethod lem-if:update-background ((implementation ncurses) color-name)
  (lem-ncurses/internal::update-background-color color-name))

(defmethod lem-if:update-foreground ((implementation ncurses) color-name)
  (lem-ncurses/internal::update-foreground-color color-name))

(defmethod lem-if:update-cursor-shape ((implementation ncurses) cursor-type)
  (lem-ncurses/internal::update-cursor-shape cursor-type))

(defmethod lem-if:update-background ((implementation ncurses) color-name)
  (lem-ncurses/internal::update-background-color color-name))

(defmethod lem-if:display-width ((implementation ncurses))
  (lem-ncurses/internal::get-display-width))

(defmethod lem-if:display-height ((implementation ncurses))
  (lem-ncurses/internal::get-display-height))

(defmethod lem-if:make-view
    ((implementation ncurses) window x y width height use-modeline)
  (lem-ncurses/view:make-view window x y width height use-modeline))

(defmethod lem-if:delete-view ((implementation ncurses) view)
  (lem-ncurses/view:delete-view view))

(defmethod lem-if:clear ((implementation ncurses) view)
  (lem-ncurses/view:clear view))

(defmethod lem-if:set-view-size ((implementation ncurses) view width height)
  (lem-ncurses/view:set-view-size view width height))

(defmethod lem-if:set-view-pos ((implementation ncurses) view x y)
  (lem-ncurses/view:set-view-pos view x y))

(defmethod lem-if:redraw-view-after ((implementation ncurses) view)
  (lem-ncurses/view:redraw-view-after view))

(defmethod lem-if:update-display ((implementation ncurses))
  (lem-ncurses/internal::update-display))

(defmethod lem-if:clipboard-paste ((implementation ncurses))
  (lem-ncurses/clipboard:paste))

(defmethod lem-if:clipboard-copy ((implementation ncurses) text)
  (lem-ncurses/clipboard:copy text))

(defmethod lem-if:view-width ((implementation ncurses) view)
  (lem-ncurses/view:view-width view))

(defmethod lem-if:view-height ((implementation ncurses) view)
  (lem-ncurses/view:view-height view))

(defmethod lem-if:render-line ((implementation ncurses)
                               view x y objects height)
  (lem-ncurses/view:render-line view x y objects))

(defmethod lem-if:render-line-on-modeline ((implementation ncurses)
                                           view
                                           left-objects
                                           right-objects
                                           default-attribute
                                           height)
  (lem-ncurses/view:render-line-on-modeline view left-objects right-objects default-attribute))

(defmethod lem-if:object-width ((implementation ncurses) drawing-object)
  (lem-ncurses/drawing-object:object-width drawing-object))

(defmethod lem-if:object-height ((implementation ncurses) drawing-object)
  (lem-ncurses/drawing-object:object-height drawing-object))

(defmethod lem-if:clear-to-end-of-window ((implementation ncurses) view y)
  (lem-ncurses/view:clear-to-end-of-window view y))

(defmethod lem-if:get-char-width ((implementation ncurses))
  (lem-ncurses/internal::get-char-width))

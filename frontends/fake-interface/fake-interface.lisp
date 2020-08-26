(defpackage :lem-fake-interface
  (:use :cl :lem))
(in-package :lem-fake-interface)

(defclass fake-interface (lem:implementation)
  ((foreground
    :initform nil
    :accessor fake-interface-foreground)
   (background
    :initform nil
    :accessor fake-interface-background)
   (display-width
    :initform 80
    :reader fake-interface-display-width)
   (display-height
    :initform 24
    :reader fake-interface-display-height))
  (:default-initargs
   :native-scroll-support nil
   :redraw-after-modifying-floating-window t))

(setf *implementation* (make-instance 'fake-interface))

(defstruct view
  x
  y
  width
  height
  modeline)

(defmethod lem-if:invoke ((implementation fake-interface) function)
  (funcall function))

(defmethod lem-if:display-background-mode ((implementation fake-interface))
  :dark)

(defmethod lem-if:update-foreground ((implementation fake-interface) color-name)
  (setf (fake-interface-foreground implementation) color-name))

(defmethod lem-if:update-background ((implementation fake-interface) color-name)
  (setf (fake-interface-background implementation) color-name))

(defmethod lem-if:display-width ((implementation fake-interface))
  (fake-interface-display-width implementation))

(defmethod lem-if:display-height ((implementation fake-interface))
  (fake-interface-display-height implementation))

(defmethod lem-if:make-view ((implementation fake-interface) window x y width height use-modeline)
  (make-view
   :x x
   :y y
   :width width
   :height height
   :modeline use-modeline))

(defmethod lem-if:delete-view ((implementation fake-interface) view)
  nil)

(defmethod lem-if:clear ((implementation fake-interface) view)
  nil)

(defmethod lem-if:set-view-size ((implementation fake-interface) view width height)
  (setf (view-width view) width
        (view-height view) height))

(defmethod lem-if:set-view-pos ((implementation fake-interface) view x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod lem-if:print ((implementation fake-interface) view x y string attribute)
  )

(defmethod lem-if:print-modeline ((implementation fake-interface) view x y string attribute)
  )

(defmethod lem-if:clear-eol ((implementation fake-interface) view x y)
  )

(defmethod lem-if:clear-eob ((implementation fake-interface) view x y)
  )

(defmethod lem-if:redraw-view-after ((implementation fake-interface) view focus-window-p))
(defmethod lem-if:update-display ((implementation fake-interface)))
(defmethod lem-if:scroll ((implementation fake-interface) view n))

;; (defmethod lem-if:display-popup-menu ((implementation fake-interface) items &key action-callback print-spec
;;                                                                                  focus-attribute non-focus-attribute))
;; (defmethod lem-if:popup-menu-update ((implementation fake-interface) items))
;; (defmethod lem-if:popup-menu-quit ((implementation fake-interface)))
;; (defmethod lem-if:popup-menu-down ((implementation fake-interface)))
;; (defmethod lem-if:popup-menu-up ((implementation fake-interface)))
;; (defmethod lem-if:popup-menu-first ((implementation fake-interface)))
;; (defmethod lem-if:popup-menu-last ((implementation fake-interface)))
;; (defmethod lem-if:popup-menu-select ((implementation fake-interface)))
;; (defmethod lem-if:display-popup-message ((implementation fake-interface) text timeout))
;; (defmethod lem-if:display-popup-buffer ((implementation fake-interface) buffer width height timeout))
;; (defmethod lem-if:delete-popup-message ((implementation fake-interface) popup-message))
;; (defmethod lem-if:display-menu ((implementation fake-interface) menu name))
;; (defmethod lem-if:update-menu ((implementation fake-interface) menu items))

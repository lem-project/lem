(defpackage :lem-fake-interfake
  (:use :cl :lem))
(in-package :lem-fake-interfake)

(defclass fake-interfake (lem:implementation)
  ((foreground
    :initform nil
    :accessor fake-interfake-foreground)
   (background
    :initform nil
    :accessor fake-interfake-background)
   (display-width
    :initform 80
    :reader fake-interfake-display-width)
   (display-height
    :initform 24
    :reader fake-interfake-display-height))
  (:default-initargs
   :native-scroll-support nil
   :redraw-after-modifying-floating-window t))

(setf *implementation* (make-instance 'fake-interfake))

(defstruct view
  x
  y
  width
  height
  modeline)

(defmethod lem-if:invoke ((implementation fake-interfake) function)
  (funcall function))

(defmethod lem-if:display-background-mode ((implementation fake-interfake))
  :dark)

(defmethod lem-if:update-foreground ((implementation fake-interfake) color-name)
  (setf (fake-interfake-foreground implementation) color-name))

(defmethod lem-if:update-background ((implementation fake-interfake) color-name)
  (setf (fake-interfake-background implementation) color-name))

(defmethod lem-if:display-width ((implementation fake-interfake))
  (fake-interfake-display-width implementation))

(defmethod lem-if:display-height ((implementation fake-interfake))
  (fake-interfake-display-height implementation))

(defmethod lem-if:make-view ((implementation fake-interfake) window x y width height use-modeline)
  (make-view
   :x x
   :y y
   :width width
   :height height
   :modeline use-modeline))

(defmethod lem-if:delete-view ((implementation fake-interfake) view)
  nil)

(defmethod lem-if:clear ((implementation fake-interfake) view)
  nil)

(defmethod lem-if:set-view-size ((implementation fake-interfake) view width height)
  (setf (view-width view) width
        (view-height view) height))

(defmethod lem-if:set-view-pos ((implementation fake-interfake) view x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod lem-if:print ((implementation fake-interfake) view x y string attribute)
  )

(defmethod lem-if:print-modeline ((implementation fake-interfake) view x y string attribute)
  )

(defmethod lem-if:clear-eol ((implementation fake-interfake) view x y)
  )

(defmethod lem-if:clear-eob ((implementation fake-interfake) view x y)
  )

(defmethod lem-if:redraw-view-after ((implementation fake-interfake) view focus-window-p))
(defmethod lem-if:update-display ((implementation fake-interfake)))
(defmethod lem-if:scroll ((implementation fake-interfake) view n))

;; (defmethod lem-if:display-popup-menu ((implementation fake-interfake) items &key action-callback print-spec
;;                                                                                  focus-attribute non-focus-attribute))
;; (defmethod lem-if:popup-menu-update ((implementation fake-interfake) items))
;; (defmethod lem-if:popup-menu-quit ((implementation fake-interfake)))
;; (defmethod lem-if:popup-menu-down ((implementation fake-interfake)))
;; (defmethod lem-if:popup-menu-up ((implementation fake-interfake)))
;; (defmethod lem-if:popup-menu-first ((implementation fake-interfake)))
;; (defmethod lem-if:popup-menu-last ((implementation fake-interfake)))
;; (defmethod lem-if:popup-menu-select ((implementation fake-interfake)))
;; (defmethod lem-if:display-popup-message ((implementation fake-interfake) text timeout))
;; (defmethod lem-if:display-popup-buffer ((implementation fake-interfake) buffer width height timeout))
;; (defmethod lem-if:delete-popup-message ((implementation fake-interfake) popup-message))
;; (defmethod lem-if:display-menu ((implementation fake-interfake) menu name))
;; (defmethod lem-if:update-menu ((implementation fake-interfake) menu items))

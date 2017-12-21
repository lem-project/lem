(in-package :xcb)
;;/usr/include/xcb/xproto.h
(defconstant SIZE-HINT-US-POSITION #x001)
(defconstant SIZE-HINT-US-SIZE     #x002)
(defconstant SIZE-HINT-P-POSITION  #x004)
(defconstant SIZE-HINT-P-SIZE      #x008)
(defconstant SIZE-HINT-P-MIN-SIZE  #x010)
(defconstant SIZE-HINT-P-MAX-SIZE  #x020)
(defconstant SIZE-HINT-P-RESIZE-INC  #x040)
(defconstant SIZE-HINT-P-ASPECT    #x080)
(defconstant SIZE-HINT-BASE-SIZE   #x100)
(defconstant SIZE-HINT-P-WIN-GRAVITY #x200)

;; size-hints is an array
;; 00 flags
;; 04 x y
;; 0C width height         ;program-specified size
;; 14 min-width min-height ;program min size
;; 1C max-width max-height ;pr max
;; 24 width-inc height-inc ;increment
;; 2C min aspect num, denom
;; 34 max aspect num, denom
;; 3C base-width base-height
;; 44 grav
;; 48

(defxcb ("xcb_icccm_set_wm_normal_hints" set-wm-normal-hints)
  (window window-t)
  (hints :pointer))

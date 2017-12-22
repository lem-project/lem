(in-package :xcb)

;; key and button bitmaps for state in events
(defconstant IN_MOD_SHIFT  1)
(defconstant IN_MOD_LOCK  2)
(defconstant IN_MOD_CONTROL  4)
(defconstant IN_MOD_1  8)
(defconstant IN_MOD_2  16)
(defconstant IN_MOD_3  32)
(defconstant IN_MOD_4  64)
(defconstant IN_MOD_5  128)
(defconstant IN_BUTTON_1  256)
(defconstant IN_BUTTON_2  512)
(defconstant IN_BUTTON_3  1024)
(defconstant IN_BUTTON_4  2048)
(defconstant IN_BUTTON_5  4096)
(defconstant IN_ANY  32768)


(defconstant EVENT-MASK-NO-EVENT  0)
(defconstant EVENT-MASK-KEY-PRESS  1)
(defconstant EVENT-MASK-KEY-RELEASE  2)
(defconstant EVENT-MASK-BUTTON-PRESS  4)
(defconstant EVENT-MASK-BUTTON-RELEASE  8)
(defconstant EVENT-MASK-ENTER-WINDOW  16)
(defconstant EVENT-MASK-LEAVE-WINDOW  32)
(defconstant EVENT-MASK-POINTER-MOTION  64)
(defconstant EVENT-MASK-POINTER-MOTION-HINT  128)
(defconstant EVENT-MASK-BUTTON-1-MOTION  256)
(defconstant EVENT-MASK-BUTTON-2-MOTION  512)
(defconstant EVENT-MASK-BUTTON-3-MOTION  1024)
(defconstant EVENT-MASK-BUTTON-4-MOTION  2048)
(defconstant EVENT-MASK-BUTTON-5-MOTION  4096)
(defconstant EVENT-MASK-BUTTON-MOTION  8192)
(defconstant EVENT-MASK-KEYMAP-STATE  16384)
(defconstant EVENT-MASK-EXPOSURE  32768)
(defconstant EVENT-MASK-VISIBILITY-CHANGE  65536)
(defconstant EVENT-MASK-STRUCTURE-NOTIFY  131072)
(defconstant EVENT-MASK-RESIZE-REDIRECT  262144)
(defconstant EVENT-MASK-SUBSTRUCTURE-NOTIFY  524288)
(defconstant EVENT-MASK-SUBSTRUCTURE-REDIRECT  1048576)
(defconstant EVENT-MASK-FOCUS-CHANGE  2097152)
(defconstant EVENT-MASK-PROPERTY-CHANGE  4194304)
(defconstant EVENT-MASK-COLOR-MAP-CHANGE  8388608)
(defconstant EVENT-MASK-OWNER-GRAB-BUTTON  16777216)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant EVENT-LAST-Event		36)
  (defparameter events
  #(nil nil
    EVENT-Key-Press		;;    2
    EVENT-Key-Release		;;3
    EVENT-Button-Press		;;4
    EVENT-Button-Release	;;5
    EVENT-Motion-Notify	;;6
    EVENT-Enter-Notify		;;7
    EVENT-Leave-Notify		;;8
    EVENT-Focus-In		;;9
    EVENT-Focus-Out		;;10
    EVENT-Keymap-Notify	;;11
    EVENT-Expose		;;12
    EVENT-Graphics-Expose	;;13
    EVENT-No-Expose		;;14
    EVENT-Visibility-Notify	;;15
    EVENT-Create-Notify	;;16
    EVENT-Destroy-Notify	;;17
    EVENT-Unmap-Notify		;;18
    EVENT-Map-Notify		;;19
    EVENT-Map-Request		;;20
    EVENT-Reparent-Notify	;;21
    EVENT-Configure-Notify	;;22
    EVENT-Configure-Request	;;23
    EVENT-Gravity-Notify	;;24
    EVENT-Resize-Request	;;25
    EVENT-Circulate-Notify	;;26
    EVENT-Circulate-Request	;;27
    EVENT-Property-Notify	;;28
    EVENT-Selection-Clear	;;29
    EVENT-Selection-Request	;;30
    EVENT-Selection-Notify	;;31
    EVENT-Colormap-Notify	;;32
    EVENT-Client-Message	;;33
    EVENT-Mapping-Notify	;;34
    EVENT-Generic-Event	;;35
    ))

  (defmacro create-event-names ()
    `(progn
       ,@(loop for event-name across events
	    for i from 2 below EVENT-LAST-EVENT 
	    collecting `(xdefconstant ,(aref events i) ,i ))))
  
  (create-event-names))


;;==============================================================================
;; EVENT STRUCTURES - ES prefix
;;
(defcstruct ES-generic
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (pad1           :uint32 :count 7)
  (full-sequence  :uint32))
;;------------------------------------------------------------------------------
(defcstruct ES-input
  (response-type  :uint8)
  (detail         :uint8) ;button-t, keycode-t, etc.
  (sequence       :uint16)
  (times          timestamp-t) ;;32
  (root           window-t)
  (event          window-t)
  (child          window-t)
  (root-x :uint16)  (root-y :uint16)
  (event-x :uint16)  (event-y :uint16)
  (state :uint16)
  (data1 :uint8) ;;
  (data2 :uint8))

;;------------------------------------------------------------------------------
(defcstruct ES-focus
  (response-type  :uint8)
  (detail         :uint8) ;button-t
  (sequence       :uint16)
  (event          window-t)
  (mode           :uint8))

(defcstruct ES-keymap-notify
  (response-type  :uint8)
  (keys           :uint8 :count 31))


(defcstruct ES-expose
  (response-type  :uint8)
  (pad0           :uint8) 
  (sequence       :uint16)
  (window         window-t)
  (x :uint16)   (y :uint16)
  (width :uint16)   (height :uint16)
  (count :uint16))

(defcstruct ES-graphic-expose
  (response-type  :uint8)
  (detail         :uint8)
  (sequence       :uint16)
  (drawable       :uint32)
  (x :uint16)   (y :uint16)
  (width :uint16)   (height :uint16)
  (minor-opcode :uint16)
  (count        :uint16)
  (major-opcode :uint8)
  )

(defcstruct ES-NO-EXPOSE
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (drawable       :uint32)
  (minor-opcode :uint16)
  (major-opcode :uint8))

(defcstruct ES-VISIBILITY-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (window         window-t)
  (state         :uint8))

(defcstruct ES-CREATE-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (parent         window-t)
  (window         window-t)
  (x :uint16)   (y :uint16)
  (width :uint16)   (height :uint16)
  (border-width   :uint16)
  (override-redirect :uint8))

(defcstruct ES-DESTROY-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (event          window-t)
  (window         window-t))

(defcstruct ES-UNMAP-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (event          window-t)
  (window         window-t)
  (data           :uint8))

(defcstruct ES-REPARENT-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (event          window-t)
  (window         window-t)
  (parent         window-t)
  (x :uint16)   (y :uint16)
  (override-redirect :uint8))

(defcstruct ES-CONFIGURE
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (event          window-t)
  (window         window-t)
  (above-sibling  window-t)
  (x :uint16)   (y :uint16)
  (width :uint16)   (height :uint16)
  (border-width   :uint16)
  (data           :uint8))

(defcstruct ES-GRAVITY-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (event          window-t)
  (window         window-t)
  (x :uint16)   (y :uint16))

(defcstruct ES-RESIZE-REQUEST
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (window         window-t)
  (width :uint16)   (height :uint16))

(defcstruct ES-CIRCULATE-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (event          window-t)
  (window         window-t)
  (pad1           :uint32)
  (place          :uint8))

(defcstruct ES-PROPERTY-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (window         window-t)
  (atom           atom-t)
  (time           timestamp-t)
  (state          :uint8))

(defcstruct ES-SELECTION-CLEAR
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (time           timestamp-t)
  (owner          window-t)
  (selection      atom-t))

(defcstruct ES-SELECTION-REQUEST
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (time           timestamp-t)
  (owner          window-t)
  (requestor      window-t)
  (selection      atom-t)
  (target         atom-t)
  (property       atom-t))

(defcstruct ES-SELECTION-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (time           timestamp-t)
  (requestor      window-t)
  (selection      atom-t)
  (target         atom-t)
  (property       atom-t))

(defcstruct ES-COLORMAP-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (window         window-t)
  (colormap       :uint32)
  (new            :uint8)
  (state          :uint8))

(defcstruct ES-CLIENT-MESSAGE
  (response-type  :uint8)   
  (format         :uint8) ;; 32 for ICCM
  (sequence       :uint16)
  (window         window-t);;@4:
  (type           atom-t) ;; @8: WM-PROTOCOLS
  (data           atom-t ;;:uint8 :count 20
   ));; hmm

(defcstruct ES-MAPPING-NOTIFY
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (request        :uint8)
  (first-keycode  :uint8)
  (count          :uint8))

(defcstruct ES-GE-GENERIC
  (response-type  :uint8)
  (extension      :uint8)
  (sequence       :uint16)
  (length         :uint32)
  (event-type     :uint16)
  (pad0           :uint8 :count 22)
  (full-sequence  :uint32))


;; A lispier layer...
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant EVENT-LAST-Event		36)
  (defparameter events
  #(nil nil
    EVENT-Key-Press		;;    2
    EVENT-Key-Release		;;3
    EVENT-Button-Press		;;4
    EVENT-Button-Release	;;5
    EVENT-Motion-Notify	;;6
    EVENT-Enter-Notify		;;7
    EVENT-Leave-Notify		;;8
    EVENT-Focus-In		;;9
    EVENT-Focus-Out		;;10
    EVENT-Keymap-Notify	;;11
    EVENT-Expose		;;12
    EVENT-Graphics-Expose	;;13
    EVENT-No-Expose		;;14
    EVENT-Visibility-Notify	;;15
    EVENT-Create-Notify	;;16
    EVENT-Destroy-Notify	;;17
    EVENT-Unmap-Notify		;;18
    EVENT-Map-Notify		;;19
    EVENT-Map-Request		;;20
    EVENT-Reparent-Notify	;;21
    EVENT-Configure-Notify	;;22
    EVENT-Configure-Request	;;23
    EVENT-Gravity-Notify	;;24
    EVENT-Resize-Request	;;25
    EVENT-Circulate-Notify	;;26
    EVENT-Circulate-Request	;;27
    EVENT-Property-Notify	;;28
    EVENT-Selection-Clear	;;29
    EVENT-Selection-Request	;;30
    EVENT-Selection-Notify	;;31
    EVENT-Colormap-Notify	;;32
    EVENT-Client-Message	;;33
    EVENT-Mapping-Notify	;;34
    EVENT-Generic-Event	;;35
    ))

  (defmacro create-event-names ()
    `(progn
       ,@(loop for event-name across events
	    for i from 2 below EVENT-LAST-EVENT 
	    collecting `(xdefconstant ,(aref events i) ,i ))))
  
  (create-event-names))



(defcfun ("xcb_wait_for_event" wait-for-event) :pointer
  (c        :pointer))

(defcfun ("xcb_poll_for_event" poll-for-event) :pointer
  (c        :pointer))



(defcfun ("xcb_key_symbols_alloc" key-symbols-alloc) :pointer
  (c        :pointer))


(defxcb ("xcb_send_event" send-event)
  (propagate :uint8)
  (dest      :uint32)
  (mask      :uint32)
  (data      :pointer))

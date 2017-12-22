(in-package :xcb)
;; pictformats
(defconstant PICT-STANDARD-ARGB-32 0)
(defconstant PICT-STANDARD-RGB-24  1)
(defconstant PICT-STANDARD-A-8     2)
(defconstant PICT-STANDARD-A-4     3)
(defconstant PICT-STANDARD-A1      4)
;; render stuff:
(defconstant CP-REPEAT  1)
(defconstant CP-ALPHA-MAP  2)
(defconstant CP-ALPHA-X-ORIGIN  4)
(defconstant CP-ALPHA-Y-ORIGIN  8)
(defconstant CP-CLIP-X-ORIGIN  16)
(defconstant CP-CLIP-Y-ORIGIN  32)
(defconstant CP-CLIP-MASK  64)
(defconstant CP-GRAPHICS-EXPOSURE  128)
(defconstant CP-SUBWINDOW-MODE  256)
(defconstant CP-POLY-EDGE  512)
(defconstant CP-POLY-MODE  1024)
(defconstant CP-DITHER  2048)
(defconstant CP-COMPONENT-ALPHA  4096);;!

(defconstant REPEAT-NORMAL 1)
;; op
(defconstant OP-CLEAR 0)
(defconstant OP-SRC 1)
(defconstant OP-DST 2)
(defconstant OP-OVER 3)
(defconstant OP-OVER-REVERSE 4)
(defconstant OP-IN 5)
(defconstant OP-IN-REVERSE 6)
(defconstant OP-OUT 7)
(defconstant OP-OUT-REVERSE 8)
(defconstant OP-ATOP 9)
(defconstant OP-ATOP-REVERSE 10)
(defconstant OP-XOR 11)
(defconstant OP-ADD 12)

(defconstant POLY-EDGE-SHARP  0)
(defconstant POLY-EDGE-SMOOTH 1)

(defconstant POLY-MODE-PRECISE 0)
(defconstant POLY-MODE-IMPRECISE 1)

(defcstruct pict-formats-reply-t
  (response-type   :uint8)
  (pad             :uint8)
  (sequence        :uint16)
  (length          :uint32)
  (num-formats     :uint32)
  (num-screens     :uint32)
  (num-depths      :uint32)
  (num-visuals     :uint32)
  (num-subpixel    :uint32))
;; returned by util-find-standard-format
(defcstruct pictform-info-t
  (id              :uint32);xcb_render_pictformat_t
  (type            :uint8)
  (depth           :uint8)
  (pad0            :uint16)
  (colormap        :uint32)
  )
(defcstruct query-version-reply-t
  (response-type   :uint8)
  (pad             :uint8)
  (sequence        :uint16)
  (length          :uint32)
  (major-version   :uint32)
  (minor-version   :uint32))

(defcstruct color-t
  (red :uint16)
  (green :uint16)
  (blue  :uint16)
  (alpha :uint16))


;; !free the returned pointer
(defcfun ("xcb_render_util_query_version" util-query-version) :pointer
  (c         :pointer))

(defcfun   ("xcb_render_util_query_formats" util-query-formats) :pointer
  (c         :pointer))

(defcfun ("xcb_render_util_find_standard_format" util-find-standard-format) :pointer
  (formats :pointer)
  (format  :int))

;; defxcb simplifies xcb binding definitions and declares connection parameter
(defxcb ("xcb_render_create_picture" create-picture)
  (pid       :uint32);xrender_picture_t
  (drawable  :uint32)
  (format    :uint32)
  (value-mask :uint32)
  (value-list :pointer))

(defxcb ("xcb_render_free_picture" free-picture)
  (picture       :uint32))


;;==============================================================================
;; glyphs
(defcstruct glyphinfo-t
  (width     :uint16)
  (height    :uint16)
  (x         :int16)
  (y         :int16)
  (x-off     :int16)
  (y-off     :int16))


(defxcb ("xcb_render_composite_glyphs_8" composite-glyphs-8)
  (op        :uint8)
  (src       :uint32);picture
  (dst       :uint32);picture
  (mask-format :uint32);pictformat_t
  (glyphset  :uint32);glyphset_t
  (src-x     :uint16)
  (src-y     :uint16)
  (glyphcmds-len :uint32)
  (glyphcmds :pointer) ;; :uint8 commands 
  )

(defxcb ("xcb_render_composite_glyphs_32" composite-glyphs-32)
  (op        :uint8)
  (src       :uint32);picture
  (dst       :uint32);picture
  (mask-format :uint32);pictformat_t
  (glyphset  :uint32);glyphset_t
  (src-x     :uint16)
  (src-y     :uint16)
  (glyphcmds-len :uint32)
  (glyphcmds :pointer) ;; :uint32 commands 
  )

(defxcb ("xcb_render_create_glyph_set" create-glyph-set)
  (id        :uint32)
  (format    :uint32))

(defxcb ("xcb_render_free_glyph_set" free-glyph-set)
  (glyphset   :uint32))

(defxcb ("xcb_render_add_glyphs" add-glyphs)
  (glyphset   :uint32)
  (glyphs-cnt :uint32)
  (glyphids   :pointer)
  (glyphinfos :pointer) ;;glyphinfo-t array
  (data-len   :uint32)  ;;bitmaps...
  (data       :pointer))

(defxcb ("xcb_render_fill_rectangles" fill-rectangles)
  (op        :uint8)
  (dst       :uint32);picture
  (color     :uint64)
  (rects_len :uint32)
  (rects     :pointer))


(defxcb ("xcb_render_composite" composite)
  (op        :uint8)
  (src       :uint32)
  (mask      :uint32)
  (dst       :uint32)

  (src-x     :int16)
  (src-y     :int16)
  (mask-x    :int16)
  (mask-y    :int16)

  (dst-x     :int16)
  (dst-y     :int16)
  (width     :uint16)
  (height    :uint16) )

(defcstruct pointfix-t
  (x         :int32)
  (y         :int32))

(defxcb ("xcb_render_create_linear_gradient"
	  create-linear-gradient) 
  (picture   :uint32)
  (p1        :int64)
  (p2        :int64)
  (num-stops :uint32)
  (stops     :pointer)
  (colors    :pointer))


;; composite
(defxcb ("xcb_composite_redirect_window" redirect-window)
  (window    :uint32)
  (update    :uint8))
(defxcb ("xcb_composite_redirect_subwindows" redirect-subwindows)
  (window    :uint32)
  (update    :uint8))

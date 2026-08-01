(defpackage :lem-sdl2/drawing
  (:use :cl
        :lem-core/display)
  (:local-nicknames (:display :lem-sdl2/display)
                    (:view :lem-sdl2/view)))
(in-package :lem-sdl2/drawing)

(defgeneric get-surface (drawing-object display))

(defmethod get-surface :around (drawing-object display)
  (or (text-object-surface drawing-object)
      (setf (text-object-surface drawing-object)
            (call-next-method))))

(defun make-text-surface (display string attribute type)
  (cffi:with-foreign-string (c-string string)
    (let ((foreground (lem-core:attribute-foreground-with-reverse attribute)))
      (sdl2-ttf:render-utf8-blended
       (or (lem-core:attribute-font attribute)
           (display:get-display-font display
                                     :type type
                                     :bold (and attribute (lem:attribute-bold attribute))
                                     :character (and (plusp (length string))
                                                     (char string 0))))
       c-string
       (lem:color-red foreground)
       (lem:color-green foreground)
       (lem:color-blue foreground)
       0))))

(defun make-text-surface-with-cache (display string attribute type)
  (or (lem-sdl2/text-surface-cache:get-text-surface-cache string attribute type)
      (let ((surface (make-text-surface display string attribute type)))
        (lem-sdl2/text-surface-cache:register-text-surface-cache string attribute type surface)
        surface)))

(defmethod get-surface ((drawing-object text-object) display)
  (let ((string (text-object-string drawing-object))
        (attribute (text-object-attribute drawing-object))
        (type (text-object-type drawing-object)))
    (when (and (lem-core:cursor-attribute-p attribute)
               (eq :box (display:display-cursor-type display)))
      (setf attribute
            (lem-core:merge-attribute
             attribute (lem-core:make-attribute
                        :foreground (lem-if:get-background-color (lem:implementation))))))
    (make-text-surface-with-cache display string attribute type)))

;;; icon-object: no specialized get-surface needed.
;;; Falls through to the text-object method which uses make-text-surface-with-cache.
;;; make-text-surface now passes :character to get-display-font, which finds the
;;; correct icon font via icon-font:icon-font. The surface is cached by
;;; (string, attribute, :icon) key, preventing orphaned textures.

(defmethod get-surface ((drawing-object folder-object) display)
  "Cache the folder PNG surface in the text-surface-cache.
Uses a sentinel key so it participates in the normal cache lifecycle
(cleared on font change, cleared by sweep-if-oversize)."
  (or (lem-sdl2/text-surface-cache:get-text-surface-cache
       "__folder_icon__" nil :folder)
      (let ((surface (sdl2-image:load-image
                      (lem-sdl2/resource:get-resource-pathname
                       "resources/open-folder.png"))))
        (lem-sdl2/text-surface-cache:register-text-surface-cache
         "__folder_icon__" nil :folder surface)
        surface)))

(defmethod lem-if:image-natural-size ((implementation lem-sdl2/sdl2:sdl2) image)
  (values (sdl2:surface-width image) (sdl2:surface-height image)))

(defmethod lem-if:object-width ((implementation lem-sdl2/sdl2:sdl2)
                                (drawing-object folder-object))
  (display:with-display (display)
    (* 2 (display:display-char-width display))))

(defmethod lem-if:object-width ((implementation lem-sdl2/sdl2:sdl2)
                                (drawing-object emoji-object))
  (display:with-display (display)
    (* (display:display-char-width display) 2 (length (text-object-string drawing-object)))))

(defgeneric draw-object (drawing-object x top display view)
  (:documentation "Draw DRAWING-OBJECT into VIEW with its top-left corner at (X, TOP).
Returns the pixel width it occupied.
`lem-core/display:layout-row' already chose TOP as the row's baseline minus this object's ascent."))

(defmethod draw-object ((drawing-object void-object) x top display view)
  0)

(defun draw-rect (display x y width height color)
  (display:with-scratch-rect (rect display x y width height)
    (display:set-render-color display color)
    (sdl2:render-fill-rect (display:display-renderer display) rect)))

(defun draw-cursor (display x y surface-width surface-height background)
  (ecase (display:display-cursor-type display)
    (:box
     (draw-rect display x y surface-width surface-height background))
    (:bar
     (draw-rect display x y 1 surface-height background))
    (:underline
     (draw-rect display x (+ y surface-height -1) surface-width 1 background))))

(defun draw-text-glyph-surface (drawing-object x top display view cell-width
                                &key clip (phase :both))
  "Draws DRAWING-OBJECT's cached SDL surface in a (CELL-WIDTH × cell-height) slot
at (X, TOP). Cell-height is taken from the drawing-object's OBJECT-HEIGHT so
non-text surfaces (folder PNG, icon font, emoji font) get scaled to the editor's
character row height instead of being placed at their natural pixel height.

CLIP / PHASE control how the glyph and its background are composited:

* CLIP NIL (icon / folder / emoji): the surface is rendered into the cell with
  `render-copy-ex' allowed to scale it down to fit, preventing oversized glyph
  surfaces from spilling into adjacent columns or rows.

* CLIP T (text characters): the surface is rendered at its natural pixel size.
  At small font sizes the NotoSansMono rasterizer produces some glyphs (`p',
  `g', `T', ...) whose blended surface-width is one pixel wider than the
  advance width — that overhang is anti-aliasing tail that must not be
  squished by `render-copy-ex' (which is what made `P' and the other exact-
  width letters look visually different from their neighbours).

PHASE selects what to draw:
  :BG     — background fill + cursor box + underline only (no glyph).
  :GLYPH  — glyph blit only (no background / cursor / underline).
  :BOTH   — bg first, then glyph, then underline. Default.

The two-phase split lets a multi-character text run paint all backgrounds
first and then all glyphs, so a 1-pixel right-edge AA overhang from one glyph
is not erased by the next glyph's background fill."
  (let* ((surface (get-surface drawing-object display))
         (surface-width (sdl2:surface-width surface))
         (surface-height (sdl2:surface-height surface))
         (cell-height (object-height drawing-object))
         (attribute (text-object-attribute drawing-object))
         (background (lem-core:attribute-background-with-reverse attribute))
         (bottom-y (+ top cell-height))
         (draw-width (if clip surface-width (min surface-width cell-width)))
         (draw-height (min surface-height cell-height)))
    (when (member phase '(:bg :both))
      (cond ((and attribute (lem-core:cursor-attribute-p attribute))
             (lem-sdl2/view:set-cursor-position view x top)
             (draw-cursor display x top cell-width cell-height background))
            (t
             (draw-rect display x top cell-width cell-height background))))
    (when (member phase '(:glyph :both))
      (let ((texture (lem-sdl2/text-surface-cache:get-or-create-texture
                      (display:display-renderer display)
                      surface)))
        (cond
          (clip
           ;; Natural-size render with explicit baseline anchoring.  The
           ;; `render-utf8-blended' surface places the glyph baseline at
           ;; (ascent) pixels down from the surface top, regardless of the
           ;; specific glyph's bounding box.  We anchor the surface so the
           ;; baseline lands at the row's intrinsic baseline:
           ;;     screen_baseline_y = bottom_y - |descent|
           ;;     glyph_dst_y       = screen_baseline_y - ascent
           ;;                       = bottom_y - ascent - |descent|
           ;; This is equivalent to the SDL_ttf docs' per-glyph formula
           ;;     dst.y = targetBaselineY - TTF_FontAscent + glyph.maxy
           ;; once the (ascent - maxy) per-glyph offset already baked into
           ;; the blended surface is accounted for, but it uses font-level
           ;; metrics rather than the surface dimensions — so the baseline
           ;; locks even if a particular glyph's surface comes back with a
           ;; non-uniform height.  Any 1-px right-edge AA overhang is
           ;; allowed to land in the next cell column; the preceding
           ;; phase :bg sweep ensures the next cell's background does not
           ;; erase it.
           (let* ((ascent (display:display-font-ascent display))
                  (descent (display:display-font-descent display))
                  (glyph-y (- bottom-y ascent (abs descent))))
             (display:with-scratch-rect (dst-rect display x glyph-y
                                                  surface-width surface-height)
               (sdl2:render-copy-ex (display:display-renderer display)
                                    texture
                                    :source-rect nil
                                    :dest-rect dst-rect
                                    :flip (list :none)))))
          (t
           (display:with-scratch-rect (dst-rect display x top draw-width draw-height)
             (sdl2:render-copy-ex (display:display-renderer display)
                                  texture
                                  :source-rect nil
                                  :dest-rect dst-rect
                                  :flip (list :none)))))))
    (when (and (member phase '(:bg :both))
               attribute
               (lem:attribute-underline attribute))
      (display:render-line display
                           x
                           (1- bottom-y)
                           (+ x cell-width)
                           (1- bottom-y)
                           :color (let ((underline (lem:attribute-underline attribute)))
                                    (if (eq underline t)
                                        (lem-core:attribute-foreground-color attribute)
                                        (or (lem:parse-color underline)
                                            (lem-core:attribute-foreground-color attribute))))))))

(defun text-object-letter-objects-and-widths (drawing-object)
  "Return two parallel lists: per-character letter-objects and their cell
widths, for the multi-character text run DRAWING-OBJECT."
  (let ((attribute (text-object-attribute drawing-object)))
    (loop :for c :across (text-object-string drawing-object)
          :for letter := (make-letter-object c attribute)
          :collect letter :into letters
          :collect (object-width letter) :into widths
          :finally (return (values letters widths)))))

(defun draw-text-object-phase (drawing-object x top display view phase)
  "Render the text-object DRAWING-OBJECT for one of the two-pass phases
(:BG or :GLYPH).  PHASE :BG paints backgrounds, cursors and underlines for
every cell of the run; PHASE :GLYPH blits each glyph at its natural surface
width so the rasterizer's right-edge anti-aliasing tail is preserved."
  (let ((string (text-object-string drawing-object)))
    (cond ((<= (length string) 1)
           (draw-text-glyph-surface drawing-object x top display view
                                    (object-width drawing-object)
                                    :clip t :phase phase))
          (t
           (multiple-value-bind (letter-objects letter-widths)
               (text-object-letter-objects-and-widths drawing-object)
             (loop :with current-x := x
                   :for letter-object :in letter-objects
                   :for letter-width :in letter-widths
                   :do (draw-text-glyph-surface letter-object current-x top
                                                display view letter-width
                                                :clip t :phase phase)
                       (incf current-x letter-width)))))))

(defmethod draw-object ((drawing-object text-object) x top display view)
  ;; Render each character individually on the cell grid. SDL_ttf's blended
  ;; surface for a multi-character string has metrics that do not equal the
  ;; sum of its per-character metrics, so a glyph would otherwise land on a
  ;; different pixel column when the cursor (or any overlay) splits the run
  ;; into shorter segments. Per-character rendering eliminates that drift —
  ;; the same character always lands on the same pixel column regardless of
  ;; how the line is partitioned, matching ncurses semantics.
  ;;
  ;; This per-text-object two-pass preserves the AA overhang within the run.
  ;; The cross-text-object equivalent (an adjacent text-object's :bg erasing
  ;; the previous text-object's AA tail) is handled by `draw-row-objects',
  ;; which lifts the two-pass to span the entire row.
  (let ((total-width (object-width drawing-object)))
    (draw-text-object-phase drawing-object x top display view :bg)
    (draw-text-object-phase drawing-object x top display view :glyph)
    total-width))

(defmethod draw-object ((drawing-object icon-object) x top display view)
  ;; Icon font glyphs typically render much wider than the 2-cell column the
  ;; layout reserves for them; draw-text-glyph-surface scales them down to fit.
  (let ((cell-width (object-width drawing-object)))
    (draw-text-glyph-surface drawing-object x top display view cell-width)
    cell-width))

(defmethod draw-object ((drawing-object folder-object) x top display view)
  ;; Folder PNG surface is much wider than 2 cells; render once and scale to fit.
  ;; Overrides the text-object per-character loop so the icon stays atomic.
  (let ((cell-width (object-width drawing-object)))
    (draw-text-glyph-surface drawing-object x top display view cell-width)
    cell-width))

(defmethod draw-object ((drawing-object emoji-object) x top display view)
  ;; Emoji strings may span multiple codepoints (base + variation selector,
  ;; ZWJ sequences, ...) that must be rendered as one composed glyph. Use the
  ;; single-surface path with cell-aligned scaling so we neither split the
  ;; sequence per-codepoint nor let an oversized emoji surface spill over.
  (let ((cell-width (object-width drawing-object)))
    (draw-text-glyph-surface drawing-object x top display view cell-width)
    cell-width))

(defmethod draw-object ((drawing-object eol-cursor-object) x top display view)
  (display:set-render-color display (eol-cursor-object-color drawing-object))
  (lem-sdl2/view:set-cursor-position view x top)
  (draw-cursor display
               x
               top
               (display:display-char-width display)
               (object-height drawing-object)
               (eol-cursor-object-color drawing-object))
  (object-width drawing-object))

(defmethod draw-object ((drawing-object line-end-object) x top display view)
  (call-next-method drawing-object
                    (+ x
                       (* (line-end-object-offset drawing-object)
                          (display:display-char-width display)))
                    top
                    display
                    view))

(defmethod draw-object ((drawing-object image-object) x top display view)
  (let* ((draw-width (max 1 (image-draw-width (lem-core:implementation) drawing-object)))
         (visible-width (object-width drawing-object))
         (surface-height (object-height drawing-object))
         (surface (image-object-image drawing-object))
         (texture (sdl2:create-texture-from-surface (display:display-renderer display) surface)))
    (if (< visible-width draw-width)
        ;; copy the leading fraction of the source into a dest that wide
        (sdl2:with-rects ((dest-rect x top visible-width surface-height)
                          (source-rect 0
                                       0
                                       (max 1 (round (* (sdl2:surface-width surface)
                                                        visible-width)
                                                     draw-width))
                                       (sdl2:surface-height surface)))
          (sdl2:render-copy-ex (display:display-renderer display)
                               texture
                               :source-rect source-rect
                               :dest-rect dest-rect
                               :flip (list :none)))
        (display:with-scratch-rect (dest-rect display x top visible-width surface-height)
          (sdl2:render-copy-ex (display:display-renderer display)
                               texture
                               :source-rect nil
                               :dest-rect dest-rect
                               :flip (list :none))))
    (sdl2:destroy-texture texture)
    visible-width))

(defun plain-text-object-p (object)
  "True when OBJECT is an instance of the base `text-object' class (and not
one of its specialised subclasses `icon-object', `folder-object', or
`emoji-object', which need their own scale-to-fit `draw-object' method
rather than the row-wide two-pass)."
  (eq (class-of object) (find-class 'text-object)))

(defun draw-row-objects (display view row)
  ;; Two-pass over the whole row: paint every plain text-object's backgrounds
  ;; first, then blit every plain text-object's glyphs.  This preserves the
  ;; 1-pixel right-edge AA tail at attribute boundaries (e.g. on the
  ;; dashboard's highlighted row, where a `p' or `g' at the end of one
  ;; attribute run would otherwise be eroded by the next text-object's
  ;; full-width background fill).  Everything else (icon / folder / emoji
  ;; text-object subclasses, images, eol-cursor) draws fully in the first pass
  ;; via its own `draw-object' method — those don't have AA overhang to
  ;; preserve and need their bespoke rendering (scale-to-fit for
  ;; icon/folder/emoji surfaces, etc.).
  (let* ((display-width (round (* (display:display-window-width display)
                                  (first (display:display-scale display)))))
         (placements (remove-if (lambda (placement)
                                  (<= display-width (placement-x placement)))
                                (row-placements row))))
    (flet ((draw-text-pass (placement phase)
             (let* ((object (placement-object placement))
                    (x (placement-x placement))
                    (top (placement-top placement)))
               (cond ((< display-width (+ x (object-width object)))
                      ;; a run reaching past the edge is drawn letter by letter, as far as it fits.
                      (loop :with current-x := x
                            :for c :across (text-object-string object)
                            :while (< current-x display-width)
                            :for letter := (make-letter-object
                                            c (text-object-attribute object))
                            :for letter-width := (object-width letter)
                            :do (draw-text-glyph-surface letter current-x top
                                                         display view letter-width
                                                         :clip t :phase phase)
                                (incf current-x letter-width)))
                     (t
                      (draw-text-object-phase object x top display view phase))))))
      ;; Pass 1: plain-text backgrounds + everything else (subclassed text-
      ;; objects and non-text objects) in normal order.
      (loop :for placement :in placements
            :do (if (plain-text-object-p (placement-object placement))
                    (draw-text-pass placement :bg)
                    (draw-object (placement-object placement)
                                 (placement-x placement)
                                 (placement-top placement)
                                 display
                                 view)))
      ;; Pass 2: plain-text glyphs.
      (loop :for placement :in placements
            :when (plain-text-object-p (placement-object placement))
              :do (draw-text-pass placement :glyph)))))

(defun fill-row (display view row x color)
  "Paint COLOR from X to the right edge of VIEW, over ROW's full height."
  (display:with-scratch-rect (rect display
                              x
                              (row-top row)
                              (- (lem-if:view-width (lem-core:implementation) view) x)
                              (row-height row))
    (display:set-render-color display color)
    (sdl2:render-fill-rect (display:display-renderer display) rect)))

(defun draw-row (display view row background)
  "Blank ROW to BACKGROUND, paint whatever fill it carries, then draw everything placed on it.
Both fills cover the row's full height, which may exceed a single text line's height when a
tall object (e.g. an image) sits on the row."
  (fill-row display view row 0 background)
  (when (row-fill-color row)
    (fill-row display view row (row-fill-x row) (row-fill-color row)))
  (draw-row-objects display view row))

(defmethod lem-if:render-row ((implementation lem-sdl2/sdl2:sdl2) view row)
  (display:with-display (display)
    (draw-row display view row (lem-core:attribute-background-color nil))))

(defmethod lem-if:render-modeline-row ((implementation lem-sdl2/sdl2:sdl2) view row
                                       default-attribute)
  (display:with-display (display)
    (draw-row display
              view
              (translate-row row (- (lem-if:view-height implementation view) (row-height row)))
              (lem-core:attribute-background-color default-attribute))))

(defmethod lem-if:clear-to-end-of-window ((implementation lem-sdl2/sdl2:sdl2) view y)
  (display:with-display (display)
    (display:set-render-color display
                              (display:display-background-color display))
    (display:with-scratch-rect (rect display
                                0
                                y
                                (lem-if:view-width implementation view)
                                (- (lem-if:view-height implementation view) y))
      (sdl2:render-fill-rect (display:display-renderer display) rect))))

(defmethod lem-core:redraw-buffer :before ((implementation lem-sdl2/sdl2:sdl2) buffer window force)
  (display:with-display (display)
    (sdl2:set-render-target (display:display-renderer display)
                            (view:view-texture (lem:window-view window)))))

(defmethod lem-core:redraw-buffer :around ((implementation lem-sdl2/sdl2:sdl2) buffer window force)
  (sdl2:in-main-thread ()
    (call-next-method)))

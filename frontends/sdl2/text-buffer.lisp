(in-package :lem-sdl2)

(defclass graphical-text-buffer (lem:text-buffer) ())

(defun view-width-by-pixel (window)
  (* (char-width) (view-width (lem:window-view window))))

(defun view-height-by-pixel (window)
  (* (char-height) (view-height (lem:window-view window))))

(defun set-cursor-position (window x y)
  (let ((view (lem:window-view window)))
    (setf (view-last-cursor-x view) x
          (view-last-cursor-y view) y)))

(defun drawing-cache (window)
  (lem:window-parameter window 'redrawing-cache))

(defun (setf drawing-cache) (value window)
  (setf (lem:window-parameter window 'redrawing-cache) value))

(defun char-type (char)
  (guess-font-type *display* (char-code char)))

(defun get-font (&key attribute type bold)
  (or (alexandria:when-let (attribute (and attribute (lem:ensure-attribute attribute)))
        (attribute-font attribute))
      (get-display-font *display* :type type :bold bold)))

(defun cursor-attribute-p (attribute)
  (lem:attribute-value attribute :cursor))

(defun set-cursor-attribute (attribute)
  (setf (lem:attribute-value attribute :cursor) t))

(defun attribute-font (attribute)
  (let ((attribute (lem:ensure-attribute attribute nil)))
    (when attribute
      (lem:attribute-value attribute 'font))))

(defun attribute-image (attribute)
  (let ((attribute (lem:ensure-attribute attribute nil)))
    (when attribute
      (lem:attribute-value attribute 'image))))

(defun attribute-width (attribute)
  (let ((attribute (lem:ensure-attribute attribute nil)))
    (when attribute
      (lem:attribute-value attribute :width))))

(defun attribute-height (attribute)
  (let ((attribute (lem:ensure-attribute attribute nil)))
    (when attribute
      (lem:attribute-value attribute :height))))

(defun attribute-foreground-with-reverse (attribute)
  (if (and attribute (lem:attribute-reverse attribute))
      (attribute-background-color attribute)
      (attribute-foreground-color attribute)))

(defun attribute-background-with-reverse (attribute)
  (if (and attribute (lem:attribute-reverse attribute))
      (attribute-foreground-color attribute)
      (attribute-background-color attribute)))

(defun overlay-cursor-p (overlay)
  (lem:overlay-get overlay :cursor))

(defstruct string-with-attribute-item
  string
  attribute)

(defstruct cursor-item
  attribute
  string)

(defstruct eol-cursor-item
  attribute)

(defstruct extend-to-eol-item
  color)

(defstruct line-end-item
  text
  attribute
  offset)

(defmethod item-string ((item string-with-attribute-item))
  (string-with-attribute-item-string item))

(defmethod item-string ((item cursor-item))
  (cursor-item-string item))

(defmethod item-string ((item eol-cursor-item))
  " ")

(defmethod item-string ((item extend-to-eol-item))
  "")

(defmethod item-attribute ((item string-with-attribute-item))
  (string-with-attribute-item-attribute item))

(defmethod item-attribute ((item cursor-item))
  (cursor-item-attribute item))

(defmethod item-attribute ((item eol-cursor-item))
  (eol-cursor-item-attribute item))

(defmethod item-attribute ((item extend-to-eol-item))
  nil)

(defun make-cursor-overlay (point)
  (let ((overlay
          (lem-core::make-overlay point
                                  (lem:with-point ((p point))
                                    (lem:character-offset p 1)
                                    p)
                                  (if (typep point 'lem:fake-cursor)
                                      'lem:fake-cursor
                                      'lem:cursor)
                                  :temporary t)))
    (lem:overlay-put overlay :cursor t)
    overlay))

(defun collect-overlays (window)
  (let ((overlays (lem-core::get-window-overlays window)))
    (if (and (eq window (lem:current-window))
             (not (lem:window-cursor-invisible-p window)))
        (append overlays
                (mapcar #'make-cursor-overlay
                        (lem:buffer-cursors (lem:window-buffer window))))
        overlays)))

(defun overlay-within-point-p (overlay point)
  (or (lem:point<= (lem:overlay-start overlay)
                   point
                   (lem:overlay-end overlay))
      (lem:same-line-p (lem:overlay-start overlay)
                       point)
      (lem:same-line-p (lem:overlay-end overlay)
                       point)))

(defun overlay-start-charpos (overlay point)
  (if (lem:same-line-p point (lem:overlay-start overlay))
      (lem:point-charpos (lem:overlay-start overlay))
      0))

(defun overlay-end-charpos (overlay point)
  (cond ((and (overlay-cursor-p overlay)
              (lem:point= (lem:overlay-start overlay) (lem:overlay-end overlay)))
         ;; cursor is end-of-buffer
         nil)
        ((lem:same-line-p point (lem:overlay-end overlay))
         (lem:point-charpos (lem:overlay-end overlay)))
        (t
         nil)))

(defstruct logical-line
  string
  attributes
  end-of-line-cursor-attribute
  extend-to-end
  line-end-overlay)

(defun attribute-equal-careful-null-and-symbol (a b)
  (if (or (null a) (null b))
      (and (null a) (null b))
      (lem-core::attribute-equal (lem:ensure-attribute a)
                                 (lem:ensure-attribute b))))

(defun logical-line-equal (a b)
  (and (string= (logical-line-string a) (logical-line-string b))
       (= (length (logical-line-attributes a))
          (length (logical-line-attributes b)))
       (every (lambda (elt1 elt2)
                (and (equal (first elt1) (first elt2))
                     (equal (second elt1) (second elt2))
                     (attribute-equal-careful-null-and-symbol (third elt1) (third elt2))))
              (logical-line-attributes a)
              (logical-line-attributes b))
       (attribute-equal-careful-null-and-symbol (logical-line-end-of-line-cursor-attribute a)
                                                (logical-line-end-of-line-cursor-attribute b))
       (attribute-equal-careful-null-and-symbol (logical-line-extend-to-end a)
                                                (logical-line-extend-to-end b))))

(defun create-logical-line (point overlays)
  (let ((end-of-line-cursor-attribute nil)
        (extend-to-end-attribute nil)
        (line-end-overlay nil))
    (destructuring-bind (string . attributes)
        (lem-base::line-string/attributes (lem-base::point-line point))
      (loop :for overlay :in overlays
            :when (overlay-within-point-p overlay point)
            :do (cond ((typep overlay 'lem-core::overlay-line-endings)
                       (setf line-end-overlay overlay))
                      ((typep overlay 'lem-core::overlay-line)
                       (setf attributes
                             (lem-core::overlay-attributes attributes
                                                           0
                                                           (length string)
                                                           (lem:overlay-attribute overlay)))
                       (setf extend-to-end-attribute (lem:overlay-attribute overlay)))
                      (t
                       (let ((overlay-start-charpos (overlay-start-charpos overlay point))
                             (overlay-end-charpos (overlay-end-charpos overlay point))
                             (overlay-attribute (lem:overlay-attribute overlay)))
                         (cond ((overlay-cursor-p overlay)
                                (set-cursor-attribute overlay-attribute)
                                (unless overlay-end-charpos
                                  (setf end-of-line-cursor-attribute overlay-attribute)))
                               ((null overlay-end-charpos)
                                (setf extend-to-end-attribute
                                      (lem:overlay-attribute overlay))))
                         (setf attributes
                               (lem-core::overlay-attributes
                                attributes
                                overlay-start-charpos
                                (or overlay-end-charpos (length string))
                                overlay-attribute))))))
      (make-logical-line :string string
                         :attributes attributes
                         :extend-to-end extend-to-end-attribute
                         :end-of-line-cursor-attribute end-of-line-cursor-attribute
                         :line-end-overlay line-end-overlay))))

(defun compute-items-from-string-and-attributes (logical-line)
  (let ((items '()))
    (flet ((add (item)
             (if (null items)
                 (push item items)
                 (let ((last-item (first items)))
                   (if (and (string-with-attribute-item-p last-item)
                            (string-with-attribute-item-p item)
                            (equal (string-with-attribute-item-attribute last-item)
                                   (string-with-attribute-item-attribute item)))
                       (setf (string-with-attribute-item-string (first items))
                             (str:concat (string-with-attribute-item-string last-item)
                                         (string-with-attribute-item-string item)))
                       (push item items))))))
      (let ((string (logical-line-string logical-line)))
        (loop :for last-pos := 0 :then end
              :for (start end attribute) :in (logical-line-attributes logical-line)
              :do (unless (= last-pos start)
                    (add (make-string-with-attribute-item :string (subseq string last-pos start))))
                  (add (if (and attribute
                                (lem:attribute-p attribute)
                                (cursor-attribute-p attribute))
                           (make-cursor-item :string (subseq string start end) :attribute attribute)
                           (make-string-with-attribute-item
                            :string (subseq string start end)
                            :attribute attribute)))
              :finally (push (make-string-with-attribute-item :string (subseq string last-pos))
                             items))))
    (alexandria:when-let (attribute
                          (logical-line-extend-to-end logical-line))
      (push (make-extend-to-eol-item :color (attribute-background-color attribute))
            items))
    (alexandria:when-let (attribute
                          (logical-line-end-of-line-cursor-attribute logical-line))
      (push (make-eol-cursor-item :attribute attribute)
            items))
    (values (nreverse items)
            (alexandria:when-let (overlay
                                  (logical-line-line-end-overlay logical-line))
              (make-line-end-item :text (lem:overlay-get overlay :text)
                                  :attribute (lem:overlay-attribute overlay)
                                  :offset (lem-core::overlay-line-endings-offset overlay))))))

(defclass drawing-object ()
  ())

(defclass void-object (drawing-object) ())

(defclass text-object (drawing-object)
  ((surface :initarg :surface :reader text-object-surface)
   (string :initarg :string :reader text-object-string)
   (attribute :initarg :attribute :reader text-object-attribute)
   (type :initarg :type :reader text-object-type)))

(defclass eol-cursor-object (drawing-object)
  ((color :initarg :color
          :reader eol-cursor-object-color)))

(defclass extend-to-eol-object (drawing-object)
  ((color :initarg :color
          :reader extend-to-eol-object-color)))

(defclass line-end-object (text-object)
  ((offset :initarg :offset
           :reader line-end-object-offset)))

(defclass image-object (drawing-object)
  ((surface :initarg :surface :reader image-object-surface)
   (width :initarg :width :reader image-object-width)
   (height :initarg :height :reader image-object-height)
   (attribute :initarg :attribute :reader image-object-attribute)))

;;; draw-object
(defmethod draw-object ((drawing-object void-object) x bottom-y window)
  nil)

(defmethod draw-object ((drawing-object text-object) x bottom-y window)
  (let* ((surface-width (object-width drawing-object))
         (surface-height (object-height drawing-object))
         (attribute (text-object-attribute drawing-object))
         (background (attribute-background-with-reverse attribute))
         (texture (sdl2:create-texture-from-surface
                   (current-renderer)
                   (text-object-surface drawing-object)))
         (y (- bottom-y surface-height)))
    (when (and attribute (cursor-attribute-p attribute))
      (set-cursor-position window x y))
    (sdl2:with-rects ((rect x y surface-width surface-height))
      (set-color background)
      (sdl2:render-fill-rect (current-renderer) rect))
    (render-texture (current-renderer)
                    texture
                    x
                    y
                    surface-width
                    surface-height)
    (sdl2:destroy-texture texture)
    (when (and attribute
               (lem:attribute-underline attribute))
      (render-line x
                   (1- (+ y surface-height))
                   (+ x surface-width)
                   (1- (+ y surface-height))
                   :color (let ((underline (lem:attribute-underline attribute)))
                            (if (eq underline t)
                                (attribute-foreground-color attribute)
                                (or (lem:parse-color underline)
                                    (attribute-foreground-color attribute))))))))

(defmethod draw-object ((drawing-object eol-cursor-object) x bottom-y window)
  (set-color (eol-cursor-object-color drawing-object))
  (let ((y (- bottom-y (object-height drawing-object))))
    (set-cursor-position window x y)
    (sdl2:with-rects ((rect x
                            y
                            (char-width)
                            (object-height drawing-object)))
      (sdl2:render-fill-rect (current-renderer) rect))))

(defmethod draw-object ((drawing-object extend-to-eol-object) x bottom-y window)
  (set-color (extend-to-eol-object-color drawing-object))
  (sdl2:with-rects ((rect x
                          (- bottom-y (char-height))
                          (- (view-width-by-pixel window) x)
                          (char-height)))
    (sdl2:render-fill-rect (current-renderer)
                           rect)))

(defmethod draw-object ((drawing-object line-end-object) x bottom-y window)
  (call-next-method drawing-object
                    (+ x
                       (* (line-end-object-offset drawing-object)
                          (char-width)))
                    bottom-y))

(defmethod draw-object ((drawing-object image-object) x bottom-y window)
  (let* ((surface-width (object-width drawing-object))
         (surface-height (object-height drawing-object))
         (texture (sdl2:create-texture-from-surface (current-renderer)
                                                    (image-object-surface drawing-object)))
         (y (- bottom-y surface-height)))
    (render-texture (current-renderer) texture x y surface-width surface-height)
    (sdl2:destroy-texture texture)))

;;; object-width
(defmethod object-width ((drawing-object void-object))
  0)

(defmethod object-width ((drawing-object text-object))
  (if (eq :emoji (text-object-type drawing-object))
      (* (char-width) 2 (length (text-object-string drawing-object)))
      (sdl2:surface-width (text-object-surface drawing-object))))

(defmethod object-width ((drawing-object eol-cursor-object))
  0)

(defmethod object-width ((drawing-object extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object line-end-object))
  (sdl2:surface-width (text-object-surface drawing-object)))

(defmethod object-width ((drawing-object image-object))
  (or (image-object-width drawing-object)
      (sdl2:surface-width (image-object-surface drawing-object))))

;;; object-height
(defmethod object-height ((drawing-object void-object))
  (char-height))

(defmethod object-height ((drawing-object text-object))
  (if (eq :emoji (text-object-type drawing-object))
      (char-height)
      (sdl2:surface-height (text-object-surface drawing-object))))

(defmethod object-height ((drawing-object eol-cursor-object))
  (char-height))

(defmethod object-height ((drawing-object extend-to-eol-object))
  (char-height))

(defmethod object-height ((drawing-object line-end-object))
  (char-height))

(defmethod object-height ((drawing-object image-object))
  (or (image-object-height drawing-object)
      (sdl2:surface-height (image-object-surface drawing-object))))

(defun split-string-by-character-type (string)
  (loop :with pos := 0 :and items := '()
        :while (< pos (length string))
        :for type := (char-type (char string pos))
        :do (loop :with start := pos
                  :while (and (< pos (length string))
                              (eq type (char-type (char string pos))))
                  :do (incf pos)
                  :finally (push (cons type (subseq string start pos)) items))
        :finally (return (nreverse items))))

(defun make-text-surface-with-attribute (string attribute &key (type :latin))
  (cffi:with-foreign-string (c-string string)
    (let* ((attribute (and attribute (lem:ensure-attribute attribute)))
           (bold (and attribute (lem:attribute-bold attribute)))
           (foreground (attribute-foreground-with-reverse attribute))
           (surface
             (sdl2-ttf:render-utf8-blended (get-font :attribute attribute
                                                     :type type
                                                     :bold bold)
                                           c-string
                                           (lem:color-red foreground)
                                           (lem:color-green foreground)
                                           (lem:color-blue foreground)
                                           0)))
      (values surface attribute))))

(defun create-drawing-object (item)
  (cond ((typep item 'eol-cursor-item)
         (list (make-instance 'eol-cursor-object
                              :color (lem:parse-color
                                      (lem:attribute-background
                                       (eol-cursor-item-attribute item))))))
        ((typep item 'extend-to-eol-item)
         (list (make-instance 'extend-to-eol-object :color (extend-to-eol-item-color item))))
        ((typep item 'line-end-item)
         (let ((string (line-end-item-text item))
               (attribute (line-end-item-attribute item)))
           (loop :for (type . string) :in (split-string-by-character-type string)
                 :unless (alexandria:emptyp string)
                 :collect (multiple-value-bind (surface attribute)
                              (make-text-surface-with-attribute string attribute :type type)
                            (make-instance 'line-end-object
                                           :offset (line-end-item-offset item)
                                           :surface surface
                                           :string string
                                           :attribute attribute
                                           :type type)))))
        (t
         (let ((string (item-string item))
               (attribute (item-attribute item)))
           (cond ((alexandria:emptyp string)
                  (list (make-instance 'void-object)))
                 ((and attribute (attribute-image attribute))
                  (list (make-instance 'image-object
                                       :surface (attribute-image attribute)
                                       :width (attribute-width attribute)
                                       :height (attribute-height attribute)
                                       :attribute attribute)))
                 (t
                  (loop :for (type . string) :in (split-string-by-character-type string)
                        :unless (alexandria:emptyp string)
                        :collect (multiple-value-bind (surface attribute)
                                     (make-text-surface-with-attribute string attribute :type type)
                                   (make-instance 'text-object
                                                  :surface surface
                                                  :string string
                                                  :attribute attribute
                                                  :type type)))))))))

(defun clear-to-end-of-line (window x y height)
  (sdl2:with-rects ((rect x y (- (view-width-by-pixel window) x) height))
    (set-render-color *display* (display-background-color *display*))
    (sdl2:render-fill-rect (current-renderer) rect)))

(defun create-drawing-objects (logical-line)
  (multiple-value-bind (items line-end-item)
      (compute-items-from-string-and-attributes logical-line)
    (append (loop :for item :in items
                  :append (create-drawing-object item))
            (when line-end-item
              (create-drawing-object line-end-item)))))

(defun make-letter-object (character attribute)
  (let* ((bold (and attribute (lem:attribute-bold attribute)))
         (foreground (attribute-foreground-with-reverse attribute))
         (type (char-type character)))
    (cffi:with-foreign-string (c-string (string character))
      (let ((surface
              (sdl2-ttf:render-utf8-blended
               (get-font :attribute attribute
                         :type type
                         :bold bold)
               c-string
               (lem:color-red foreground)
               (lem:color-green foreground)
               (lem:color-blue foreground)
               0)))
        (make-instance 'text-object
                       :surface surface
                       :string (string character)
                       :attribute attribute
                       :type type)))))


(defun explode-object (text-object)
  (check-type text-object text-object)
  (loop :for c :across (text-object-string text-object)
        :collect (make-letter-object c (text-object-attribute text-object))))

(defun separate-objects-by-width (objects view-width)
  (loop
    :until (null objects)
    :collect (loop :with total-width := 0
                   :and physical-line-objects := '()
                   :for object := (pop objects)
                   :while object
                   :do (cond ((<= view-width (+ total-width (object-width object)))
                              (cond ((and (typep object 'text-object)
                                          (< 1 (length (text-object-string object))))
                                     (setf objects (nconc (explode-object object) objects)))
                                    (t
                                     (push object objects)
                                     (push (make-letter-object #\\ nil)
                                           physical-line-objects)
                                     (return (nreverse physical-line-objects)))))
                             (t
                              (incf total-width (object-width object))
                              (push object physical-line-objects)))
                   :finally (return (nreverse physical-line-objects)))))

(defun redraw-physical-line (window y height objects)
  (clear-to-end-of-line window 0 y height)
  (loop :for x := 0 :then (+ x (object-width object))
        :for object :in objects
        :do (draw-object object x (+ y height) window)))

(defun validate-cache-p (window y height logical-line)
  (loop :for (cache-y cache-height cache-logical-line) :in (drawing-cache window)
        :when (and (= y cache-y)
                   (= height cache-height)
                   (logical-line-equal logical-line cache-logical-line))
        :return t))

(defun invalidate-cache (window y height)
  (setf (drawing-cache window)
        (remove-if (lambda (elt)
                     (destructuring-bind (cache-y cache-height cache-logical-line) elt
                       (declare (ignore cache-logical-line))
                       (not (or (<= (+ y height)
                                    cache-y)
                                (<= (+ cache-y cache-height)
                                    y)))))
                   (drawing-cache window))))

(defun update-and-validate-cache-p (window y height logical-line)
  (cond ((validate-cache-p window y height logical-line) t)
        (t
         (invalidate-cache window y height)
         (push (list y height logical-line)
               (drawing-cache window))
         nil)))

(defun max-height-of-objects (objects)
  (loop :for object :in objects
        :maximize (object-height object)))

(defvar *invalidate-cache* nil)

(defun redraw-logical-line (window y logical-line)
  (let ((objects-per-physical-line
          (separate-objects-by-width (create-drawing-objects logical-line)
                                     (view-width-by-pixel window))))
    (when (and (not (alexandria:length= 1 objects-per-physical-line))
               *invalidate-cache*)
      (setf (drawing-cache window) '()))
    (loop :for objects :in objects-per-physical-line
          :for height := (max-height-of-objects objects)
          :do (unless (update-and-validate-cache-p window y height logical-line)
                (setf *invalidate-cache* t)
                (redraw-physical-line window y height objects))
              (incf y height)
          :sum height)))

(defun redraw-lines (window)
  (lem:with-point ((point (lem:window-view-point window)))
    (let ((*invalidate-cache* nil)
          (overlays (collect-overlays window)))
      (loop :with y := 0 :and height := (view-height-by-pixel window)
            :do (incf y (redraw-logical-line window
                                             y
                                             (create-logical-line point overlays)))
            :while (and (lem:line-offset point 1)
                        (< y height))
            :finally (sdl2:with-rects ((rect 0
                                             y
                                             (view-width-by-pixel window)
                                             (- (view-height-by-pixel window)
                                                y)))
                       (set-render-color *display* (display-background-color *display*))
                       (sdl2:render-fill-rect (current-renderer) rect))))))

(defmethod lem-core::redraw-buffer ((buffer graphical-text-buffer) window force)
  (assert (eq buffer (lem:window-buffer window)))
  (when (or force
            (lem-core::screen-modified-p (lem:window-screen window)))
    (setf (drawing-cache window) '()))
  (sdl2:set-render-target (current-renderer) (view-texture (lem:window-view window)))
  (redraw-lines window)
  (lem-core::update-screen-cache (lem:window-screen window) buffer))

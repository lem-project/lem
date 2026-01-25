(in-package :lem/transient)

(defvar *transient-popup-window*
  nil)

(defvar *transient-popup-max-lines*
  15
  "max height of the transient buffer (measured in lines).")

(defparameter *transient-window-margin*
  4
  "margin in columns from the edge of the screen.")

(defparameter *transient-column-separator*
  " | "
  "string used to separate columns in row layout.")

(define-attribute transient-key-attribute
  (t
   :foreground (attribute-foreground (ensure-attribute 'syntax-function-name-attribute))))

(define-attribute transient-title-attribute
  (t
   :foreground (attribute-foreground (ensure-attribute 'document-header1-attribute))
   :bold (attribute-bold (ensure-attribute 'document-header1-attribute))))

(define-attribute transient-separator-attribute
  (t
   :foreground (attribute-foreground (ensure-attribute 'modeline-inactive))))

(define-attribute transient-bracket-attribute
  (t
   :foreground (attribute-foreground (ensure-attribute 'syntax-string-attribute))))

(define-attribute transient-inactive-attribute
  (t
   :foreground (attribute-foreground (ensure-attribute 'syntax-comment-attribute))
   :background (attribute-background (ensure-attribute 'syntax-comment-attribute))))

(define-attribute transient-value-attribute
  (t
   :foreground (attribute-foreground (ensure-attribute 'syntax-constant-attribute))
   :bold t))

;; custom floating window class that repositions on each redraw
(defclass transient-popup-window (floating-window)
  ((base-width :initarg :base-width :accessor transient-base-width)
   (base-height :initarg :base-height :accessor transient-base-height)))

(defun compute-bottom-offset ()
  "compute the offset from the bottom of the display where the transient popup should appear.

this accounts for the status line if present, the prompt window if active, and the bottom
completion interface if present."
  (let ((offset (if (window-use-modeline-p (current-window))
                    1
                    0)))
    ;; add height of prompt window if it exists
    (alexandria:when-let ((prompt-window (lem/prompt-window:current-prompt-window)))
      (incf offset (window-height prompt-window))
      ;; add height of completion window if it exists
      (when lem/completion-mode::*completion-context*
        (alexandria:when-let* ((context lem/completion-mode::*completion-context*)
                               (popup-menu (lem/completion-mode::context-popup-menu context))
                               (completion-window (lem/popup-menu::popup-menu-window popup-menu)))
          (incf offset (window-height completion-window)))))
    offset))

(defun compute-transient-position (width height)
  (let* ((bottom-offset (compute-bottom-offset))
         ;; position above minibuffer area: y = display-height - height - bottom-offset - border
         (y (max 0 (- (display-height) height bottom-offset 2)))
         (x (max 0 (- (display-width) width *transient-window-margin*))))
    (values x y)))

(defmethod window-redraw ((window transient-popup-window) force)
  "reposition the transient popup on each redraw to stay above the minibuffer/completion."
  (let ((width (transient-base-width window))
        (height (transient-base-height window)))
    (multiple-value-bind (x y) (compute-transient-position width height)
      (window-set-pos window x y)))
  (call-next-method))

(defstruct layout-separator
  "a visual separator between items.")

(defstruct layout-item
  "a single displayable item (prefix binding)"
  key
  description
  (key-attribute 'transient-key-attribute)
  description-attribute)

(defstruct layout-title
  "a title/header for a keymap section."
  text)

(defstruct layout-row
  "items arranged horizontally."
  items)

(defstruct layout-column
  "items arranged vertically."
  items
  ;; max key width for even spacing
  (key-width 0))

(defun get-description (prefix)
  "returns a description for an entry that could be a prefix or a keymap."
  (let ((desc (prefix-description prefix)))
    (if desc
        (princ-to-string desc)
        (let ((suffix (prefix-suffix prefix)))
          (cond ((typep suffix 'keymap)
                 (princ-to-string (or (keymap-name suffix) "+prefix")))
                ((typep suffix 'prefix)
                 (or (prefix-description suffix) "+prefix"))
                (t (princ-to-string suffix)))))))

(defgeneric prefix-render (prefix)
  (:documentation "return a layout item that should be displayed for the prefix in the popup."))

(defmethod prefix-render ((prefix prefix))
  (make-layout-item
   :key (princ-to-string (prefix-key prefix))
   :description (get-description prefix)))

(defmethod prefix-render ((prefix choice))
  (let* ((desc (get-description prefix))
         (value (prefix-value prefix))
         (value-str (princ-to-string value)))
    (let ((description-segments
            (list (cons desc nil)
                  (cons " " nil)
                  (cons "[" 'transient-bracket-attribute)
                  (cons value-str 'transient-value-attribute)
                  (cons "]" 'transient-bracket-attribute))))
      (make-layout-item
       :key (princ-to-string (prefix-key prefix))
       :description description-segments))))

(defmethod prefix-render :around ((prefix prefix))
  (let ((item (call-next-method)))
    (when item
      (unless (prefix-active-p prefix)
        (setf (layout-item-key-attribute item) 'transient-inactive-attribute)
        (setf (layout-item-description-attribute item) 'transient-inactive-attribute)))
    item))

(defun generate-layout (keymap)
  "generate layout from keymap structure.

prefixes always display vertically as items.
nested keymaps are arranged based on display-style (:row or :column)."
  (unless (keymap-show-p keymap)
    (return-from generate-layout nil))
  (let ((prefix-items)
        (keymap-layouts))
    ;; process children, separating prefixes from keymaps
    (dolist (child (keymap-children keymap))
      (cond
        ;; nested keymap: recurse and collect
        ((typep child 'keymap)
         (alexandria:when-let ((child-layout (generate-layout child)))
           (push child-layout keymap-layouts)))
        ;; prefix: create item if show-p
        ((typep child 'prefix)
         (when (prefix-show-p child)
           (let ((item (prefix-render child)))
             (push item prefix-items))))))
    ;; build result: title first, then content (prefixes + keymaps arranged by display-style)
    (setf prefix-items (nreverse prefix-items))
    (setf keymap-layouts (nreverse keymap-layouts))
    (let ((parts)
          (content-items))
      (let ((title (or (keymap-name keymap) "[unnamed keymap]")))
        (push (make-layout-title :text title) parts))
      ;; collect prefix column and keymap layouts as content items
      (when prefix-items
        (let ((max-key-width (reduce 'max
                                     prefix-items
                                     :key (lambda (item)
                                            (length (layout-item-key item)))
                                     :initial-value 0)))
          (push (make-layout-column :items prefix-items :key-width max-key-width)
                content-items)))
      (dolist (km keymap-layouts)
        (when content-items
          (push (make-layout-separator) content-items))
        (push km content-items))
      (setf content-items (nreverse content-items))
      ;; arrange content items based on display-style
      (when content-items
        (ecase (keymap-display-style keymap)
          (:row (push (make-layout-row :items content-items) parts))
          (:column (dolist (item content-items) (push item parts)))))
      ;; wrap everything in a column (separates title from content, may contain the rest of the items)
      (when parts
        (make-layout-column :items (nreverse parts))))))

(defun render-layout-to-segments (layout &optional (key-width 0))
  "pre-render layout to a list of lines, where each line is a list of (text . attribute) segments."
  (cond
    ((null layout) nil)
    ((layout-title-p layout)
     (list (list (cons (format nil "-- ~A --" (layout-title-text layout))
                       'transient-title-attribute))))
    ((layout-separator-p layout)
     (list (list (cons "----------------" 'transient-separator-attribute))))
    ((layout-item-p layout)
     (let* ((key (layout-item-key layout))
            (padding (max 0 (- key-width (length key))))
            (desc (layout-item-description layout))
            (inactive (eq (layout-item-key-attribute layout) 'transient-inactive-attribute))
            (base-segments
              (list (cons key (layout-item-key-attribute layout))
                    (cons (make-string padding :initial-element #\space) nil)
                    (cons " " nil))))
       ;; if desc is a list of segments, append them. otherwise treat as string.
       (list (append base-segments
                     (if (listp desc)
                         (if inactive
                             (mapcar
                              (lambda (seg)
                                (cons (car seg) 'transient-inactive-attribute))
                              desc)
                             desc)
                         (list (cons (or desc "")
                                     (layout-item-description-attribute layout))))))))
    ((layout-column-p layout)
     (let ((col-key-width (layout-column-key-width layout)))
       (loop for item in (layout-column-items layout)
             append (render-layout-to-segments item col-key-width))))
    ((layout-row-p layout)
     (render-row-as-grid-segments layout))))

(defun segment-line-width (segments)
  (reduce '+
          segments
          :key (lambda (seg) (length (car seg)))
          :initial-value 0))

(defun insert-segment-line (point segments)
  "insert a segment line at point, applying attributes."
  (dolist (seg segments)
    (let ((text (car seg))
          (attr (cdr seg)))
      (if attr
          (insert-string point text :attribute attr)
          (insert-string point text)))))

(defun render-row-as-grid-segments (row)
  "render row to segment lines (for nested rows in pre-rendering)."
  (let* ((items (layout-row-items row))
         ;; map items: for separator use :separator, otherwise generate segments
         (columns (mapcar (lambda (item)
                            (if (layout-separator-p item)
                                :separator
                                (render-layout-to-segments item)))
                          items))
         ;; calculate widths: separator -> length of separator, normal -> max segment line width
         (widths (mapcar (lambda (lines)
                           (if (eq lines :separator)
                               (length *transient-column-separator*)
                               (reduce 'max lines :key 'segment-line-width)))
                         columns))
         ;; max-height: max length of normal columns (ignore separators)
         (max-height (reduce 'max
                             columns
                             :key (lambda (col)
                                    (if (eq col :separator)
                                        0
                                        (length col)))
                             :initial-value 0))
         (result))
    (dotimes (row-idx max-height)
      (let ((line-segments))
        (loop for col-data in columns
              for col-width in widths
              do (cond
                   ((eq col-data :separator)
                    (push (cons *transient-column-separator* 'transient-separator-attribute)
                          line-segments))
                   (t
                    (let* ((seg-line (if (< row-idx (length col-data))
                                         (nth row-idx col-data)
                                         nil))
                           (line-width (if seg-line (segment-line-width seg-line) 0))
                           (padding (- col-width line-width)))
                      (when seg-line
                        (dolist (seg seg-line)
                          (push seg line-segments)))
                      (when (> padding 0)
                        (push (cons (make-string padding :initial-element #\space) nil)
                              line-segments))))))
        (push (nreverse line-segments) result)))
    (nreverse result)))

(defun render-layout-to-buffer (layout point &optional (key-width 0))
  "render layout to buffer at point.

key-width is used for even key spacing in items."
  (let ((lines (render-layout-to-segments layout key-width)))
    (loop for line in lines
          for first = t then nil
          do (unless first
               (insert-character point #\newline))
             (insert-segment-line point line))))

(defmethod show-transient ((keymap keymap))
  "show the transient popup. creates a window if it hasnt been created yet."
  (let* ((existing-window (and (not (deleted-window-p *transient-popup-window*))
                               *transient-popup-window*))
         (buffer (if existing-window
                     (window-buffer existing-window)
                     (make-buffer "*transient*" :temporary t :enable-undo-p nil)))
         (layout (generate-layout keymap)))
    (erase-buffer buffer)
    ;; we dont want lines to be cut off for now (no wrapping), until we have scrollbars or something
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (if layout
        (render-layout-to-buffer layout (buffer-point buffer))
        (insert-string (buffer-point buffer) "(no bindings)"))
    (buffer-start (buffer-point buffer))
    ;; (log:info "buffer text:~%~A" (buffer-text buffer))
    (let* ((width (min (lem/popup-window::compute-buffer-width buffer)
                       (- (display-width) (* 2 *transient-window-margin*))))
           (height (min (lem/popup-window::compute-buffer-height buffer)
                        *transient-popup-max-lines*)))
      (multiple-value-bind (x y) (compute-transient-position width height)
        (if existing-window
            (progn
              (setf (transient-base-width existing-window) width)
              (setf (transient-base-height existing-window) height)
              (window-set-pos existing-window x y)
              (window-set-size existing-window width height))
            (setf *transient-popup-window*
                  (make-instance 'transient-popup-window
                                 :buffer buffer
                                 :x x
                                 :y y
                                 :width width
                                 :height height
                                 :base-width width
                                 :base-height height
                                 :use-modeline-p nil
                                 :border 1))))))
  (redraw-display))
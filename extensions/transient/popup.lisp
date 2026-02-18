(in-package :lem/transient)

(defvar *transient-popup-window*
  nil)

(defvar *transient-shown-keymap*
  nil
  "the last keymap passed to show-transient. used to detect same-keymap redraws and preserve scroll position.")

(defvar *transient-popup-max-lines*
  15
  "max height of the transient buffer (measured in lines).")

(defvar *transient-vertical-scroll-amount*
  1
  "number of lines to scroll vertically per step.")

(defvar *transient-horizontal-scroll-amount*
  5
  "number of columns to scroll horizontally per step.")

(defvar *transient-content-dirty*
  nil
  "when T, show-transient re-renders even for the same keymap (e.g. after infix changes).")

(defparameter *transient-column-separator*
  " | "
  "string used to separate columns in row layout.")

(defvar *transient-always-show*
  t
  "whether to always show the transient buffer. by default only keymaps that have show-p set are shown.")

(define-attribute transient-matched-key-attribute
  (t
   :foreground (attribute-foreground (ensure-attribute 'syntax-string-attribute))))

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

;; this keymap has a special behavior. we're overriding its 'keymap-find' below.
(define-transient *transient-mode-keymap*
  :display-style :row
  (:key "M-Shift-Down"
   :suffix 'transient-scroll-down
   :behavior :drop
   :description "scroll down")
  (:key "M-Shift-Up"
   :suffix 'transient-scroll-up
   :behavior :drop
   :description "scroll up")
  (:key "M-Shift-Right"
   :suffix 'transient-scroll-right
   :behavior :drop
   :description "scroll right")
  (:key "M-Shift-Left"
   :suffix 'transient-scroll-left
   :behavior :drop
   :description "scroll left"))

(defmethod keymap-find ((keymap (eql *transient-mode-keymap*)) key)
  (let* ((keyseq (etypecase key
                   (lem-core::key (list key))
                   (list key))))
    ;; the keymap needs to work if any key we defined (e.g. M-S-Down) is the last one in our
    ;; current keymap sequence, because we want these keys to be available in any transient
    ;; keymap context
    (loop for prefix in (keymap-children keymap)
          when (equal (prefix-key prefix) (car (last keyseq)))
            return (normalize-binding prefix (prefix-suffix prefix)))))

(define-minor-mode transient-mode
    (:name "transient-mode"
     :global t
     :keymap *transient-mode-keymap*))

(defmethod prefix-invoke :after ((prefix infix))
  (setf *transient-content-dirty* t))

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
                 (princ-to-string (or (keymap-description suffix) "+prefix")))
                ((typep suffix 'prefix)
                 (or (prefix-description suffix) "+prefix"))
                (t (princ-to-string suffix)))))))

(defun prefix-effective-display-key (prefix)
  "return the display key for PREFIX, falling back to one returned by prefix-key."
  (or (prefix-display-key prefix)
      (princ-to-string (prefix-key prefix))))

(defun keymap-contains-via-intermediates-p (keymap target)
  "return T if TARGET is reachable from KEYMAP through a sequence of intermediate prefixes."
  (dolist (child (keymap-children keymap))
    (when (and (typep child 'prefix) (prefix-intermediate-p child))
      (let ((suffix (prefix-suffix child)))
        (when (and (typep suffix 'keymap)
                   (or (eq suffix target)
                       (keymap-contains-via-intermediates-p suffix target)))
          (return t))))))

;; TODO: this is hacky
(defun make-key-with-highlight (key-str matched-depth)
  "return KEY-STR as highlighted segments if MATCHED-DEPTH > 0.

MATCHED-DEPTH is the number of key parts (space-separated) to highlight."
  (if (and matched-depth (> matched-depth 0))
      (let ((pos 0)
            (parts-found 0))
        ;; walk through key-str counting space-separated parts
        (loop :for i :from 0 :below (length key-str)
              :while (< parts-found matched-depth)
              :do (if (char= (char key-str i) #\Space)
                      (incf parts-found)
                      (setf pos (1+ i))))
        (if (> pos 0)
            (let ((matched (subseq key-str 0 pos))
                  (unmatched (subseq key-str pos)))
              (list (cons matched 'transient-matched-key-attribute)
                    (cons unmatched 'transient-key-attribute)))
            key-str))
      key-str))

(defun make-value-description (prefix)
  "build description segments for a prefix that displays its value, e.g. 'desc [value]'."
  (let ((desc (get-description prefix))
        (value-str (princ-to-string (prefix-value prefix))))
    (list (cons desc nil)
          (cons " " nil)
          (cons "[" 'transient-bracket-attribute)
          (cons value-str 'transient-value-attribute)
          (cons "]" 'transient-bracket-attribute))))

(defgeneric prefix-render (prefix &optional matched-depth)
  (:documentation "return a layout item that should be displayed for the prefix in the popup.

MATCHED-DEPTH is the number of key parts (space-separated) to highlight."))

(defmethod prefix-render ((prefix prefix) &optional matched-depth)
  (let ((key-str (prefix-effective-display-key prefix)))
    (make-layout-item
     :key (make-key-with-highlight key-str matched-depth)
     :description (get-description prefix))))

(defun prefix-render-with-value (prefix matched-depth)
  (let ((key-str (prefix-effective-display-key prefix)))
    (make-layout-item
     :key (make-key-with-highlight key-str matched-depth)
     :description (make-value-description prefix))))

(defmethod prefix-render ((prefix choice) &optional matched-depth)
  (prefix-render-with-value prefix matched-depth))

(defmethod prefix-render ((prefix toggle) &optional matched-depth)
  (prefix-render-with-value prefix matched-depth))

(defun find-intermediate-root (active-keymap)
  "find the effective root keymap for ACTIVE-KEYMAP by searching from *root-keymap* tree.

returns the nearest ancestor keymap that reaches ACTIVE-KEYMAP through intermediate prefixes,
or ACTIVE-KEYMAP itself if no such ancestor exists."
  (labels ((find-root (keymap)
             ;; check if this keymap reaches active-keymap via intermediates
             (when (keymap-contains-via-intermediates-p keymap active-keymap)
               (return-from find-intermediate-root keymap))
             ;; recurse into child keymaps
             (dolist (child (keymap-children keymap))
               (cond ((typep child 'keymap)
                      (find-root child))
                     ((typep child 'prefix)
                      (let ((suffix (prefix-suffix child)))
                        (when (typep suffix 'keymap)
                          (find-root suffix))))))))
    (find-root *root-keymap*)
    active-keymap))

(defmethod prefix-render :around ((prefix prefix) &optional matched-depth)
  (let ((item (call-next-method)))
    (when item
      (unless (prefix-active-p prefix)
        (setf (layout-item-key-attribute item) 'transient-inactive-attribute)
        (setf (layout-item-description-attribute item) 'transient-inactive-attribute)))
    item))

(defun generate-layout (keymap &optional active-keymap)
  "generate layout from keymap structure.

prefixes always display vertically in their own column.
nested keymaps are arranged based on display-style (:row or :column).
prefixes marked as :intermediate-p are flattened and shown with concatenated keys."
  (let ((prefix-items)
        (keymap-layouts))
    (labels ((collect-items (node &optional (matched-depth 0))
               (cond
                 ;; nested keymap: recurse and collect
                 ((typep node 'keymap)
                  (alexandria:when-let ((child-layout (generate-layout node active-keymap)))
                    (push child-layout keymap-layouts)))
                 ;; prefix: create item if show-p
                 ((typep node 'prefix)
                  (when (prefix-show-p node)
                    (if (prefix-intermediate-p node)
                        (let* ((suffix (prefix-suffix node))
                               (new-depth (if (and active-keymap
                                                   (typep suffix 'keymap)
                                                   (or (eq suffix active-keymap)
                                                       (keymap-contains-via-intermediates-p
                                                        suffix active-keymap)))
                                              (1+ matched-depth)
                                              matched-depth)))
                          (if (typep suffix 'keymap)
                              (dolist (child (keymap-children suffix))
                                (collect-items child new-depth))
                              (push (prefix-render node new-depth) prefix-items)))
                        (push (prefix-render
                               node
                               (when (prefix-display-key node)
                                 matched-depth))
                              prefix-items)))))))
      ;; process children, separating prefixes from keymaps
      (let ((current keymap))
        (loop while current
              do (dolist (child (keymap-children current))
                   (collect-items child))
                 (setf current (keymap-base current)))))
    ;; build result: title first, then content (prefixes + keymaps arranged by display-style)
    (setf prefix-items (nreverse prefix-items))
    (setf keymap-layouts (nreverse keymap-layouts))
    (let ((parts)
          (content-items))
      (let ((title (keymap-description keymap)))
        (when title
          (push (make-layout-title :text title) parts)))
      ;; collect prefix column and keymap layouts as content items
      (when prefix-items
        (let ((max-key-width (reduce 'max
                                     prefix-items
                                     :key (lambda (item)
                                            (let ((key (layout-item-key item)))
                                              (if (listp key)
                                                  (segment-line-width key)
                                                  (length key))))
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
     (let ((text (princ-to-string (layout-title-text layout))))
       (list (list (cons "[" 'transient-bracket-attribute)
                   (cons text 'transient-title-attribute)
                   (cons "]" 'transient-bracket-attribute)))))
    ((layout-separator-p layout)
     (list (list (cons "----------------" 'transient-separator-attribute))))
    ((layout-item-p layout)
     (let* ((key (layout-item-key layout))
            (key-is-segments (listp key))
            (padding (if key-is-segments
                         (max 0 (- key-width (segment-line-width key)))
                         (max 0 (- key-width (length key)))))
            (desc (layout-item-description layout))
            (inactive (eq (layout-item-key-attribute layout) 'transient-inactive-attribute))
            (base-segments
              (append (if key-is-segments
                          key
                          (list (cons key (layout-item-key-attribute layout))))
                      (list (cons (make-string padding :initial-element #\space) nil)
                            (cons " " nil)))))
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
                    (let* ((seg-line (when (< row-idx (length col-data))
                                       (nth row-idx col-data)))
                           (line-width (if seg-line
                                           (segment-line-width seg-line)
                                           0))
                           (padding (- col-width line-width)))
                      (when seg-line
                        (dolist (seg seg-line)
                          (push seg line-segments)))
                      (when (> padding 0)
                        (push (cons (make-string padding :initial-element #\space) nil)
                              line-segments))))))
        (push (nreverse line-segments) result)))
    (nreverse result)))

(defun insert-segment-lines (point lines)
  "insert a list of segment lines into buffer at POINT."
  (loop :for line :in lines
        :for first := t :then nil
        :do (unless first
              (insert-character point #\newline))
            (insert-segment-line point line)))

(defmethod show-transient ((keymap keymap))
  "shows the transient buffer with the contents rendered."
  (let ((same-keymap-p (eq keymap *transient-shown-keymap*)))
    ;; skip re-render when same keymap, window alive, and no content changes
    (when (and same-keymap-p (transient-window-alive-p) (not *transient-content-dirty*))
      (return-from show-transient))
    (let* ((existing-window (and *transient-popup-window*
                                 (not (deleted-window-p *transient-popup-window*))
                                 *transient-popup-window*))
           (buffer (if existing-window
                       (window-buffer existing-window)
                       (make-buffer "*transient*" :temporary t :enable-undo-p nil)))
           ;; save vertical scroll position before erase (only for same-keymap re-renders)
           (saved-vp-line (when (and existing-window same-keymap-p)
                            (line-number-at-point (window-view-point existing-window))))
           (root (find-intermediate-root keymap))
           (layout (generate-layout root keymap)))
      (setf *transient-content-dirty* nil)
      (setf *transient-shown-keymap* keymap)
      (erase-buffer buffer)
      (setf (variable-value 'line-wrap :buffer buffer) nil)
      (if layout
          (insert-segment-lines (buffer-point buffer) (render-layout-to-segments layout))
          (insert-string (buffer-point buffer) "(no bindings)"))
      (buffer-start (buffer-point buffer))
      (let ((height (min (lem/popup-window::compute-buffer-height buffer)
                         *transient-popup-max-lines*)))
        (if existing-window
            (unless (= (window-height existing-window) height)
              (resize-bottomside-window existing-window height))
            (setf *transient-popup-window*
                  (make-bottomside-window buffer :height height))))
      ;; restore vertical scroll position for same-keymap re-renders
      (when (and saved-vp-line (> saved-vp-line 1))
        (move-to-line (window-view-point *transient-popup-window*) saved-vp-line))
      ;; reset horizontal scroll when switching to a different keymap
      (unless same-keymap-p
        (setf (window-parameter *transient-popup-window* 'lem-core::horizontal-scroll-start) 0))))
  (modeline-add-status-list 'transient-scroll-status)
  (transient-mode t)
  (redraw-display))

(defun transient-window-alive-p ()
  "return T if the transient popup window exists and is not deleted."
  (and *transient-popup-window*
       (not (deleted-window-p *transient-popup-window*))))

(defun transient-scroll-status (window)
  "modeline status function showing scroll position when the transient buffer overflows."
  (when (transient-window-alive-p)
    (let* ((tw *transient-popup-window*)
           (nlines (buffer-nlines (window-buffer tw)))
           (height (window-height tw)))
      (when (>= nlines height)
        (let ((pos (cond ((first-line-p (window-view-point tw))
                          "top")
                         ((null (line-offset (copy-point (window-view-point tw) :temporary)
                                             height))
                          "bot")
                         (t (format
                             nil
                             "~d%"
                             (floor (* 100
                                       (float (/ (line-number-at-point (window-view-point tw))
                                                 nlines)))))))))
          (values (format nil " transient[~a]" pos)
                  'transient-separator-attribute))))))

(define-command transient-scroll-down () ()
  "scroll the transient buffer down by `*transient-vertical-scroll-amount*' lines."
  (when (transient-window-alive-p)
    (window-scroll *transient-popup-window* *transient-vertical-scroll-amount*)
    (redraw-display)))

(define-command transient-scroll-up () ()
  "scroll the transient buffer up by `*transient-vertical-scroll-amount*' lines."
  (when (transient-window-alive-p)
    (window-scroll *transient-popup-window* (- *transient-vertical-scroll-amount*))
    (redraw-display)))

(define-command transient-scroll-right () ()
  "scroll the transient buffer to the right by `*transient-vertical-scroll-amount*' columns."
  (when (transient-window-alive-p)
    (let ((current (or (window-parameter *transient-popup-window*
                                         'lem-core::horizontal-scroll-start)
                       0)))
      (setf (window-parameter *transient-popup-window* 'lem-core::horizontal-scroll-start)
            (+ current *transient-horizontal-scroll-amount*)))
    (redraw-display)))

(define-command transient-scroll-left () ()
  "scroll the transient buffer to the left by `*transient-vertical-scroll-amount*' columns."
  (when (transient-window-alive-p)
    (let ((current (or (window-parameter *transient-popup-window*
                                         'lem-core::horizontal-scroll-start)
                       0)))
      (setf (window-parameter *transient-popup-window* 'lem-core::horizontal-scroll-start)
            (max 0 (- current *transient-horizontal-scroll-amount*))))
    (redraw-display)))

(defun hide-transient ()
  "hide (delete) the transient window."
  (when (and *transient-popup-window*
             (not (deleted-window-p *transient-popup-window*)))
    (modeline-remove-status-list 'transient-scroll-status)
    (delete-bottomside-window)
    (setf *transient-popup-window* nil)
    (setf *transient-shown-keymap* nil)
    (setf *transient-content-dirty* nil)
    (transient-mode nil)
    (redraw-display)))
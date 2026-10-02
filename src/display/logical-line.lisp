(in-package :lem-core)

(defvar *active-modes*)

(defun virtual-text-runs (spec)
  "An overlay's :before-string / :after-string / :display as a list of (string attribute) runs.
SPEC is a bare string, a single (string attribute) pair, or a list of such pairs"
  (cond ((stringp spec) (list (list spec nil)))
        ((not (consp spec)) nil)
        ((stringp (first spec)) (list (list (first spec) (second spec))))
        (t (loop :for (run-string run-attribute) :in spec
                 :collect (list run-string run-attribute)))))

;; it is important to distinguish between different kinds of lines in the codebase.
;; - line: a buffer line
;; - logical-line: a line that may contain folded text, including text that originally contains multiple lines. this structure doesnt concern itself with the width of the editor. one
;; - logical-line: may split into multiple physical-lines. physical-line: strictly one displayed row, a portion of a logical-line split by width (or a virtual newline).

;; the 'logical' in the following structs are in reference to the 'logical item' concept.
(defstruct logical-line
  items
  left-content
  end-of-line-cursor-attribute
  extend-to-end
  line-end-overlay)

(defstruct logical-item
  ;; the range (x . y) in the original buffer that this item corresponds to.
  ;; nil implies the item is a virtual item, it doesnt exist in the original buffer.
  source)

(defstruct (logical-string (:include logical-item))
  string
  attribute)

(defstruct (logical-cursor (:include logical-string)))

(defstruct (logical-eol-cursor (:include logical-item))
  attribute
  true-cursor-p)

(defstruct (logical-extend-to-eol (:include logical-item))
  color)

(defstruct (logical-line-end (:include logical-string))
  offset)

;; a newline inside virtual text (an overlay's :before-string / :after-string / :display).
(defstruct (logical-virtual-line-break (:include logical-item)))

(defmethod logical-item-virtual-p ((item logical-item))
  (null (logical-item-source item)))

(defmethod logical-item-pure-p ((item logical-item))
  "T when ITEM's display maps 1:1 onto its source: a string item whose string is as long as its
source range. used for resolving cursor click position."
  (let ((source (logical-item-source item)))
    (and (typep item 'logical-string)
         source
         (= (length (logical-string-string item))
            (- (cdr source) (car source))))))

(defmethod item-string ((item logical-string))
  (logical-string-string item))

(defmethod item-string ((item logical-eol-cursor))
  " ")

(defmethod item-string ((item logical-extend-to-eol))
  "")

(defmethod item-attribute ((item logical-string))
  (logical-string-attribute item))

(defmethod item-attribute ((item logical-eol-cursor))
  (logical-eol-cursor-attribute item))

(defmethod item-attribute ((item logical-extend-to-eol))
  nil)

(defun overlay-within-point-p (overlay point)
  (or (point<= (overlay-start overlay)
               point
               (overlay-end overlay))
      (same-line-p (overlay-start overlay)
                   point)
      (same-line-p (overlay-end overlay)
                   point)))

(defun invisible-overlay-covering (point &optional (overlays (buffer-overlays (point-buffer point))))
  "Return the :invisible overlay covering POINT."
  (loop :for overlay :in overlays
        :thereis (and (overlay-get overlay :invisible)
                      (point<= (overlay-start overlay) point)
                      (point< point (overlay-end overlay))
                      overlay)))

(defun move-point-out-of-overlay (point overlay direction)
  "Move POINT to the nearest edge of OVERLAY in DIRECTION (:forward or :backward), so it does not
rest inside. usable directly as a :cursor-enter-functions handler; `fold-region' installs it by
default so a cursor never appears stuck on the hidden text of a fold."
  (if (eq direction :backward)
      (progn
        (move-point point (overlay-start overlay))
        ;; step onto the last visible position before the overlay, unless that is the buffer
        ;; start, where there is nowhere further to go.
        (character-offset point -1))
      (move-point point (overlay-end overlay))))

(defun reveal-overlay-on-cursor-enter (point overlay direction)
  (overlay-put overlay :invisible nil)
  (overlay-put overlay :show-virtual-text nil))

(defun hide-overlay-on-cursor-leave (point overlay direction)
  (overlay-put overlay :invisible t)
  (overlay-put overlay :show-virtual-text t))

(defun overlay-show-virtual-text-p (overlay)
  "Whether OVERLAY's :before-string/:after-string should render. defaults to T."
  (let ((value (getf (overlay-plist overlay) :show-virtual-text :unset)))
    (if (eq value :unset) t value)))

(defun overlay-has-cursor-hooks-p (overlay)
  (or (overlay-get overlay :cursor-enter-functions)
      (overlay-get overlay :cursor-leave-functions)))

(defun overlays-with-cursor-hooks-covering (point)
  "The overlays covering POINT that take part in cursor enter/leave tracking."
  (loop :for overlay :in (buffer-overlays (point-buffer point))
        :when (and (overlay-has-cursor-hooks-p overlay)
                   (point<= (overlay-start overlay) point)
                   (point< point (overlay-end overlay)))
          :collect overlay))

;; but reveal behavior isnt relevant for line folding
(defun place-region-placeholder-overlay (start end &key (placeholder "...") (cursor-behavior :move-out) (is-line-fold t))
  "Hide the lines of the region [START, END), leaving START's line visible with a fold marker.
returns the fold overlay. CURSOR-BEHAVIOR decides how the cursor is kept off the hidden text:
- :move-out :: move the cursor to the nearest visible edge (see `move-point-out-of-overlay').
- :reveal   :: open the fold while the cursor is inside it and close it again on leave (see
               `reveal-overlay-on-cursor-enter' / `hide-overlay-on-cursor-leave').
- nil       :: install nothing; the cursor may rest on the hidden text.
callers can also set the overlay's :cursor-enter-functions / :cursor-leave-functions directly."
  (with-point ((s start)
               (e end))
    (when is-line-fold
      (line-end s)
      ;; dont hide the newline that terminates the folded region's last line, or the line after
      ;; the fold gets merged onto the header's visual line.
      (when (start-line-p e)
        (character-offset e -1)))
    (let ((overlay (make-overlay s e 'fold-attribute)))
      (overlay-put overlay :invisible t)
      (overlay-put overlay :fold t)
      (overlay-put overlay :before-string (list placeholder 'fold-attribute))
      (ecase cursor-behavior
        (:move-out
         (overlay-put overlay :cursor-enter-functions (list 'move-point-out-of-overlay)))
        (:reveal
         (overlay-put overlay :cursor-enter-functions (list 'reveal-overlay-on-cursor-enter))
         (overlay-put overlay :cursor-leave-functions (list 'hide-overlay-on-cursor-leave)))
        ((nil)))
      overlay)))

(defun line-continuation-p (point)
  "Whether POINT's line continues a previous visual line. meaning the newline preceding it is
hidden by an :invisible overlay, so the line is not a visual line of its own.
a folded region may hide arbitrary character ranges, including the newlines that join several
buffer lines into one displayed line."
  (and (not (first-line-p point))
       (with-point ((p point))
         (line-start p)
         (character-offset p -1)
         (invisible-overlay-covering p))))

(defun copy-string-item (item source string)
  "Copy string-carrying ITEM preserving its subtype, with SOURCE and STRING."
  (check-type item logical-string)
  (let ((copy (copy-structure item)))
    (setf (logical-item-source copy) source
          (logical-string-string copy) string)
    copy))

(defun split-items-at (items pos)
  "Split ITEMS at source position POS, returning (values before after).
BEFORE holds items entirely before POS, AFTER holds items at or after POS. a
`logical-string' strictly containing POS is split in two,
preserving its subtype. items with a null source are never split and stay on
the side where they are encountered."
  (let ((before-rev))
    (loop :for rest :on items
          :for item := (first rest)
          :for source := (logical-item-source item)
          :do (if (null source)
                  (push item before-rev)
                  (let ((lo (car source))
                        (hi (cdr source)))
                    (cond ((<= hi pos)
                           (push item before-rev))
                          ((<= pos lo)
                           (return (values (nreverse before-rev) rest)))
                          ((typep item 'logical-string)
                           (let* ((full (logical-string-string item))
                                  (offset (- pos lo))
                                  (left (copy-string-item item
                                                          (cons lo pos)
                                                          (subseq full 0 offset)))
                                  (right (copy-string-item item
                                                           (cons pos hi)
                                                           (subseq full offset))))
                             (return (values (nreverse (cons left before-rev))
                                             (cons right (cdr rest))))))
                          (t
                           (error "cannot split ~S at ~S: atomic item" item pos)))))
          :finally (return (values (nreverse before-rev) nil)))))

(defun splice-into-items (items pos item)
  "Insert ITEM into the list ITEMS of `logical-item's at position POS."
  (multiple-value-bind (before after) (split-items-at items pos)
    (append before (cons item after))))

(defun replace-into-items (items begin end new-items)
  "Replace all items (or parts of them) from BEGIN to END with NEW-ITEMS."
  (multiple-value-bind (before rest) (split-items-at items begin)
    (multiple-value-bind (middle after) (split-items-at rest end)
      (append before new-items after))))

(defun merge-item-attribute (attr1 attr2)
  "merge attributes ATTR2 onto ATTR1, tolerating a null ATTR1."
  (if attr1
      (alexandria:if-let ((u (ensure-attribute attr1 nil)))
        (merge-attribute u attr2)
        attr2)
      attr2))

(defun map-items-in-range (items start end func)
  "Run FUNC on each item of ITEMS in [START, END) and rejoin the list."
  (multiple-value-bind (before rest) (split-items-at items start)
    (multiple-value-bind (mid after) (split-items-at rest end)
      (append before (mapcar func mid) after))))

(defun apply-attribute-to-items (items start end attribute)
  "Merge ATTRIBUTE into each sourced string item of ITEMS in [START, END).
sourceless (virtual) items are left alone. returns the updated list."
  (map-items-in-range items
                      start
                      end
                      (lambda (it)
                        (when (and (not (logical-item-virtual-p it))
                                   (typep it 'logical-string))
                          (setf (logical-string-attribute it)
                                (merge-item-attribute (logical-string-attribute it)
                                                      attribute)))
                        it)))

(defun apply-attribute-to-all-items (items attribute)
  "Merge ATTRIBUTE into every string item of ITEMS, virtual ones included.
returns the updated list."
  (dolist (it items)
    (when (typep it 'logical-string)
      (setf (logical-string-attribute it)
            (merge-item-attribute (logical-string-attribute it)
                                  attribute))))
  items)

;; is it really useful to have it work over arbitrary ranges? technically its relevant for
;; multiple cursors but its not a realistic scenario.
(defun apply-cursor-to-items (items start end attribute)
  "Cover [START, END) of ITEMS with the cursor ATTRIBUTE, converting plain string items to
`logical-cursor'. sourceless items are left alone. returns the updated list."
  (map-items-in-range
   items
   start
   end
   (lambda (it)
     (cond ((logical-item-virtual-p it)
            it)
           ((typep it 'logical-cursor)
            (setf (logical-string-attribute it)
                  (merge-item-attribute (logical-string-attribute it)
                                        attribute))
            it)
           ((typep it 'logical-string)
            (make-logical-cursor
             :source (logical-item-source it)
             :string (logical-string-string it)
             :attribute (merge-item-attribute (logical-string-attribute it)
                                              attribute)))
           (t it)))))

(defun virtual-runs-to-items (runs)
  "Turn virtual-text RUNS into items: one sourceless `logical-string' per newline-free segment,
with a `logical-virtual-line-break' between segments."
  (let ((out))
    (dolist (run runs)
      (destructuring-bind (run-string run-attribute) run
        (loop :for segment :in (uiop:split-string run-string :separator '(#\newline))
              :for firstp := t :then nil
              :do (unless firstp
                    (push (make-logical-virtual-line-break) out))
                  (unless (string= segment "")
                    (push (make-logical-string
                           :string segment
                           :attribute run-attribute
                           :source nil)
                          out)))))
    (nreverse out)))

(defun expand-tabs-in-items (items tab-width)
  "Expand tabs in ITEMS to TAB-WIDTH columns. each tab becomes an item of its
own: single-char source, spaces string. returns the new list."
  (let ((col 0)
        (expanded))
    (dolist (it items)
      (cond ((typep it 'logical-virtual-line-break)
             (push it expanded)
             (setf col 0))
            ((typep it 'logical-string)
             (let ((s (logical-string-string it))
                   (src (logical-item-source it)))
               (if (not (find #\tab s))
                   (progn
                     (push it expanded)
                     (incf col (length s)))
                   (let ((lo (and src (car src)))
                         (run nil)
                         (run-lo nil))
                     (loop :for c :across s
                           :for idx :from 0
                           :for spos := (and lo (+ lo idx))
                           :do (if (char= c #\tab)
                                   (progn
                                     (when run
                                       (push (copy-string-item
                                              it
                                              (and lo (cons run-lo spos))
                                              (coerce (nreverse run) 'string))
                                             expanded)
                                       (setf run nil))
                                     (let ((n (- tab-width (mod col tab-width))))
                                       (push (copy-string-item
                                              it
                                              (and lo (cons spos (1+ spos)))
                                              (make-string n :initial-element #\space))
                                             expanded)
                                       (incf col n))
                                     (setf run-lo (and spos (1+ spos))))
                                   (progn
                                     (unless run (setf run-lo spos))
                                     (push c run)
                                     (incf col))))
                     (when run
                       (push (copy-string-item
                              it
                              (and lo (cons run-lo (+ lo (length s))))
                              (coerce (nreverse run) 'string))
                             expanded))))))
            (t
             (push it expanded))))
    (nreverse expanded)))

(defun items-from-string-and-attributes (string attributes &optional base-offset)
  "Split STRING into logical-string items at ATTRIBUTES boundaries, filling gaps with plain items.
when BASE-OFFSET is given, string index I maps to source position BASE-OFFSET+I, otherwise source
is nil (virtual)."
  (let ((items)
        (last 0))
    (dolist (entry attributes)
      (destructuring-bind (start end attribute &rest _) entry
        (when (< last start)
          (push (make-logical-string
                 :string (subseq string last start)
                 :attribute nil
                 :source (and base-offset
                              (cons (+ base-offset last) (+ base-offset start))))
                items))
        (push (make-logical-string
               :string (subseq string start end)
               :attribute attribute
               :source (and base-offset
                            (cons (+ base-offset start) (+ base-offset end))))
              items)
        (setf last end)))
    (when (< last (length string))
      (push (make-logical-string
             :string (subseq string last)
             :attribute nil
             :source (and base-offset
                          (cons (+ base-offset last)
                                (+ base-offset (length string)))))
            items))
    (nreverse items)))

(defun make-base-items (vstart vend)
  "The raw logical line from VSTART to VEND as string items in source
coordinates, newlines between joined buffer lines included."
  (with-point ((p vstart))
    (let ((all)
          (offset 0))
      (loop
        (destructuring-bind (str . attrs) (get-string-and-attributes-at-point p)
          (setf all (nconc all (items-from-string-and-attributes str attrs offset)))
          (when (same-line-p p vend)
            (return all))
          (setf all
                (nconc all
                       (list (make-logical-string
                              :string (string #\newline)
                              :attribute nil
                              :source (cons (+ offset (length str))
                                            (+ offset (length str) 1))))))
          (incf offset (1+ (length str)))
          (line-offset p 1))))))

(defun create-logical-line (point overlays active-modes)
  "Build a logical-line for the visual line starting at POINT, joining any following buffer lines
whose preceding newline is hidden by an :invisible overlay. a single displayed line may contain
several folds that each hide arbitrary character ranges across multiple buffer lines."
  (let ((invisible-overlays
          (remove-if-not (lambda (ov) (overlay-get ov :invisible)) overlays)))
    (with-point ((vstart point)
                 (vend point))
      (line-start vstart)
      (line-end vend)
      ;; extend VEND across every newline hidden by an invisible overlay so the
      ;; logical line reaches the next *visible* newline (or the buffer end).
      (loop :until (last-line-p vend)
            :while (invisible-overlay-covering vend invisible-overlays)
            :do (line-offset vend 1)
                (line-end vend))
      ;; restrict our attention to overlays that overlap with this logical line
      ;; (between vstart and vend)
      (let ((overlays (remove-if-not
                       (lambda (ov)
                         (and (point<= (overlay-start ov) vend)
                              (point<= vstart (overlay-end ov))))
                       overlays)))
        (flet ((overlay-start-charpos (overlay)
                 ;; column where the overlay starts on this logical line, clamped to
                 ;; 0 when it begins before VSTART.
                 (let ((s (overlay-start overlay)))
                   (if (point<= vstart s)
                       (count-characters vstart s)
                       0)))
               (overlay-end-charpos (overlay)
                 ;; column where the overlay ends, or NIL when it extends past VEND.
                 (let ((e (overlay-end overlay)))
                   (when (point<= e vend)
                     (count-characters vstart e))))
               (start-in-line-p (overlay)
                 ;; true when the overlay's start falls within this logical line.
                 (point<= vstart (overlay-start overlay)))
               (end-in-line-p (overlay)
                 ;; true when the overlay's end falls within this logical line.
                 (point<= (overlay-end overlay) vend)))
          (let* ((end-of-line-cursor-attribute)
                 (extend-to-end-attribute)
                 (line-end-overlay)
                 (left-content
                   (compute-left-display-area-content active-modes
                                                      (point-buffer point)
                                                      point))
                 (tab-width (variable-value 'tab-width :default point))
                 (raw-length (count-characters vstart vend))
                 (items (make-base-items vstart vend)))
            (loop :for overlay :in overlays
                  :for invisible := (overlay-get overlay :invisible)
                  :for display := (overlay-get overlay :display)
                  :when (or invisible display)
                    :do (let* ((ov-start (overlay-start-charpos overlay))
                               (ov-end (or (overlay-end-charpos overlay)
                                           raw-length))
                               (replacements
                                 (cond (display
                                        (virtual-runs-to-items (virtual-text-runs display)))
                                       ((eq invisible :ellipsis)
                                        (list (make-logical-string :string "...")))
                                       (t nil))))
                          (when (< ov-start ov-end)
                            ;; TODO: its not efficient to keep doing it in a loop
                            (setf items
                                  (replace-into-items items ov-start ov-end replacements)))))
            ;; process all overlays for attributes (virtual text handled separately below).
            (loop :for overlay :in overlays
                  :do (cond
                        ((typep overlay 'line-endings-overlay)
                         (when (end-in-line-p overlay)
                           (setf line-end-overlay overlay)))
                        ((typep overlay 'line-overlay)
                         (let ((raw (overlay-attribute overlay)))
                           (alexandria:when-let ((over (ensure-attribute raw)))
                             (setf items (apply-attribute-to-all-items items over)))
                           (setf extend-to-end-attribute raw)))
                        ((typep overlay 'cursor-overlay)
                         (let* ((ov-start (overlay-start-charpos overlay))
                                (raw (overlay-attribute overlay)))
                           (unless (cursor-overlay-fake-p overlay)
                             (set-cursor-attribute raw))
                           (if (>= ov-start raw-length)
                               (setf end-of-line-cursor-attribute raw)
                               (alexandria:when-let ((over (ensure-attribute raw)))
                                 (setf items
                                       (apply-cursor-to-items items
                                                              ov-start
                                                              (1+ ov-start)
                                                              over))))))
                        (t
                         (let ((ov-start (overlay-start-charpos overlay))
                               (ov-end (overlay-end-charpos overlay))
                               (raw (overlay-attribute overlay))
                               (invisible (overlay-get overlay :invisible))
                               (display (overlay-get overlay :display)))
                           ;; plain attribute (only when not replaced by invisible/display)
                           (when (and raw (not invisible) (not display))
                             (alexandria:when-let ((overlay1 (ensure-attribute raw)))
                               (unless ov-end
                                 (setf extend-to-end-attribute raw))
                               (setf items
                                     (apply-attribute-to-items items
                                                               ov-start
                                                               (or ov-end raw-length)
                                                               overlay1))))))))
            ;; virtual text from :before-string/:after-string/:display. emit each overlay's
            ;; :before then :after, visiting overlays in (end, start) order. at any shared
            ;; charpos an overlay closing there (smaller end) is emitted before one opening
            ;; there, so trailing :after-strings precede leading :before-strings, but a
            ;; zero-length overlay's own pair stays adjacent. a final stable sort by charpos
            ;; groups them without affecting this order. virtual items carry a null
            ;; source, so no remapping is needed after splices.
            (let ((inserts)
                  (sorted (stable-sort
                           (loop :for overlay :in overlays
                                 :when (and (overlay-show-virtual-text-p overlay)
                                            (or (overlay-get overlay :before-string)
                                                (overlay-get overlay :after-string)))
                                   :collect overlay)
                           (lambda (a b)
                             (let ((a-end (or (overlay-end-charpos a) raw-length))
                                   (b-end (or (overlay-end-charpos b) raw-length)))
                               (if (= a-end b-end)
                                   (< (overlay-start-charpos a)
                                      (overlay-start-charpos b))
                                   (< a-end b-end)))))))
              (loop :for overlay :in sorted
                    :for before-str := (overlay-get overlay :before-string)
                    :for after-str := (overlay-get overlay :after-string)
                    :do (when (and before-str (start-in-line-p overlay))
                          (let ((pos (overlay-start-charpos overlay)))
                            (dolist (vi (virtual-runs-to-items
                                         (virtual-text-runs before-str)))
                              (push (cons pos vi) inserts))))
                        (when (and after-str (end-in-line-p overlay))
                          (let ((pos (or (overlay-end-charpos overlay) raw-length)))
                            (dolist (vi (virtual-runs-to-items
                                         (virtual-text-runs after-str)))
                              (push (cons pos vi) inserts)))))
              (dolist (ins (stable-sort (nreverse inserts) #'< :key #'car))
                (setf items (splice-into-items items (car ins) (cdr ins)))))
            ;; the view can start mid-line, so drop everything before it.
            ;; but at the line start there is nothing to drop, and splitting would strand leading
            ;; virtual items in `before', discarding e.g. the image of a fully invisible line.
            (when (plusp (point-charpos point))
              (multiple-value-bind (before after)
                  (split-items-at items (point-charpos point))
                (setf items after)))
            ;; turn tabs into items that carry spaces as their strings.
            (setf items (expand-tabs-in-items items tab-width))
            (make-logical-line
             :items items
             :left-content left-content
             :extend-to-end extend-to-end-attribute
             :end-of-line-cursor-attribute end-of-line-cursor-attribute
             :line-end-overlay line-end-overlay)))))))

(defun compute-items-from-logical-line (logical-line)
  (let ((tail))
    (alexandria:when-let (attribute (logical-line-extend-to-end logical-line))
      (push (make-logical-extend-to-eol :color (attribute-background-color attribute))
            tail))
    (alexandria:when-let (attribute (logical-line-end-of-line-cursor-attribute logical-line))
      (push (make-logical-eol-cursor :attribute attribute
                                     :true-cursor-p (cursor-attribute-p attribute))
            tail))
    (values (append (logical-line-items logical-line) (nreverse tail))
            (alexandria:when-let (overlay
                                  (logical-line-line-end-overlay logical-line))
              (make-logical-line-end :string (line-endings-overlay-text overlay)
                                     :attribute (overlay-attribute overlay)
                                     :offset (line-endings-overlay-offset overlay))))))

(defun make-temporary-highlight-line-overlay (buffer)
  (when (and (variable-value 'highlight-line :default (current-buffer))
             (current-theme))
    (alexandria:when-let ((color (highlight-line-color)))
      (make-line-overlay (buffer-point buffer)
                         (make-attribute :background color)
                         :temporary t))))

(defgeneric make-region-overlays-using-global-mode (global-mode cursor))

(defmethod make-region-overlays-using-global-mode ((global-mode emacs-mode) cursor)
  (let ((mark (cursor-mark cursor)))
    (when (mark-active-p mark)
      (list (make-overlay cursor
                    (mark-point mark)
                    'region
                    :temporary t)))))

(defun make-cursor-overlay* (point)
  (make-cursor-overlay
   point
   (if (typep point 'fake-cursor)
       'fake-cursor
       'cursor)
   :fake (typep point 'fake-cursor)))

(defun get-window-overlays (window)
  (let* ((buffer (window-buffer window))
         (overlays (buffer-overlays buffer)))
    (when (eq (current-window) window)
      (dolist (cursor (buffer-cursors buffer))
        (alexandria:when-let ((region-overlays (make-region-overlays-using-global-mode (current-global-mode) cursor)))
          (dolist (ol region-overlays) (push ol overlays))))
      (if-push (make-temporary-highlight-line-overlay buffer)
               overlays))
    (if (and (eq window (current-window))
             (not (window-cursor-invisible-p window)))
        (append overlays
                (mapcar #'make-cursor-overlay*
                        (buffer-cursors (window-buffer window))))
        overlays)))

(defun call-do-logical-line (window function)
  (with-point ((point (window-view-point window)))
    (let* ((overlays (get-window-overlays window))
           (active-modes (get-active-modes-class-instance (window-buffer window)))
           (*active-modes* active-modes))
      (loop :for logical-line := (create-logical-line point overlays active-modes)
            :do (when logical-line
                  (funcall function logical-line point))
                (loop
                  (unless (line-offset point 1)
                    (return-from call-do-logical-line))
                  (unless (line-continuation-p point)
                    (return)))))))

(defmacro do-logical-line ((logical-line window &optional point) &body body)
  "Run BODY for each logical line of WINDOW, in draw order.
POINT, when named, is bound to the start of the line. It is one point reused for every line and
moved on to the next once BODY returns, so BODY must `copy-point' it to hold on to it."
  (let ((point-var (or point (gensym "POINT"))))
    `(call-do-logical-line ,window
                           (lambda (,logical-line ,point-var)
                             (declare (ignorable ,point-var))
                             ,@body))))

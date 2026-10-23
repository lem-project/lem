(in-package :lem-core)

(defun visual-line-beginning (point)
  "Move POINT to the start of its visual line. returns POINT."
  (line-start point)
  (loop :while (line-continuation-p point)
        :do (line-offset point -1))
  point)

(defun next-visual-line-beginning (point)
  "Move POINT to the start of the next visual line. returns POINT, or NIL without moving it."
  (with-point ((p point))
    (loop (unless (line-offset p 1)
            (return nil))
          (unless (line-continuation-p p)
            (return (move-point point p))))))

(defun previous-visual-line-beginning (point)
  "Move POINT, a visual line start, to the start of the one before. returns POINT, or NIL."
  (and (line-offset point -1)
       (visual-line-beginning point)))

(defun point-row-extent (rows offset)
  "Vertical span of the row containing OFFSET, as (values TOP BOTTOM).
ROWS is one visual line as (START . HEIGHT) pairs from `logical-line-rows'.
START is the buffer offset where the row's text begins, or NIL for a virtual-only row.
OFFSET is a character offset from the start of the line.
TOP and BOTTOM are measured down from the top of the visual line.
the row is the last one starting at or before OFFSET. virtual-only rows never contain OFFSET,
except the first row."
  (let ((y 0)
        (top 0)
        (bottom 0))
    (loop :for (start . height) :in rows
          :for first := t :then nil
          :do (when (or first (and start (<= start offset)))
                (setf top y
                      bottom (+ y height)))
              (incf y height))
    (values top bottom)))

(defun window-point-row-extent (window point)
  "Vertical span of POINT's row from the top of WINDOW's view, as (values TOP BOTTOM).
TOP and BOTTOM are measured down from the view's first row.
returns NIL when POINT is above the view. When POINT is past the bottom of the view, returns equal
TOP and BOTTOM past the view's height instead of the exact span."
  (let ((height (window-view-height window)))
    (with-point ((p (window-view-point window))
                 (line point))
      (visual-line-beginning p)
      (visual-line-beginning line)
      (when (point<= p line)
        (loop :with y := 0
              :until (point= p line)
              :do (incf y (reduce #'+ (visual-line-rows window p) :key #'cdr))
                  (when (or (< height y)
                            (not (next-visual-line-beginning p)))
                    (return (values y y)))
              :finally (multiple-value-bind (top bottom)
                           (point-row-extent (visual-line-rows window p) (count-characters p point))
                         (return (values (+ y top) (+ y bottom)))))))))

(defun window-first-clipped-row (window)
  "Point to the start of the first partly (in)visible row at the bottom of WINDOW.
returns NIL when all text from the top of the view to the end of the buffer is fully visible (which
happens when the cursor is close to the end of the buffer).
the top row is never treated as clipped. rows with only virtual text are skipped, as they have
no buffer position."
  (let ((height (window-view-height window))
        (p (copy-point (window-view-point window) :temporary)))
    (visual-line-beginning p)
    (loop :with y := 0
          :and clipped := nil
          :do (loop :for (start . row-height) :in (visual-line-rows window p)
                    :do (when (and (plusp y) (< height (+ y row-height)))
                          (setf clipped t))
                        (incf y row-height)
                        ;; a row of only virtual text holds no cursor, the clipped row is the next
                        ;; with text
                        (when (and clipped start)
                          (return-from window-first-clipped-row (character-offset p start))))
              (unless (next-visual-line-beginning p)
                (return nil))
              (when clipped
                (return p)))))

(defun move-back-visual-lines (window point continue-p)
  "Move POINT up by visual lines while CONTINUE-P allows it.
POINT must start at a visual line start and is moved in place.
before each step, CONTINUE-P is called with the distance moved so far and the height of the line
above. step only while it returns true.
returns POINT."
  (with-point ((p point))
    (loop :with distance := 0
          :while (previous-visual-line-beginning p)
          :do (let ((height (reduce #'+ (visual-line-rows window p) :key #'cdr)))
                (unless (funcall continue-p distance height)
                  (return))
                (incf distance height)
                (move-point point p))))
  point)

(defun height-below (window line height cap)
  "Add the heights of the visual lines after LINE to HEIGHT, stopping at CAP."
  (with-point ((p line))
    (loop :while (and (< height cap)
                      (next-visual-line-beginning p))
          :do (incf height (reduce #'+ (visual-line-rows window p) :key #'cdr))))
  (min height cap))

(defun set-window-view-start (window point)
  (unless (point= point (window-view-point window))
    (move-point (window-view-point window) point)
    (need-to-redraw window)
    (run-hooks *window-scroll-functions* window)))

(defun window-recenter (window &key line from-bottom)
  "Recenter WINDOW to the given LINE number.
LINE must be NIL or a positive number.
If LINE is NIL, recenter to the middle of the WINDOW.
Otherwise, recenter to the nth LINE (starting at 0), counted from the top.
If FROM-BOTTOM is T, start counting from the bottom."
  (check-type line (or null integer))
  (check-type from-bottom boolean)
  (let* ((row-height (text-row-height))
         (rows (floor (window-view-height window) row-height))
         (target (* row-height
                    (cond ((null line) (floor rows 2))
                          (from-bottom (- rows line 1))
                          (t line))))
         (point (window-buffer-point window)))
    (with-point ((view point))
      (visual-line-beginning view)
      (let ((row-top (point-row-extent (visual-line-rows window view)
                                       (count-characters view point))))
        (move-back-visual-lines window
                                view
                                (lambda (distance height)
                                  (<= (+ distance height row-top) target))))
      (set-window-view-start window view))))

(defun window-recenter-top-bottom (window)
  "In first call recenter WINDOW to the middle line.
If cursor is already in the middle of WINDOW then move cursor in the top position.
If cursor is on top then move move WINDOW to the bottom."
  (let* ((line (window-cursor-y window))
         (window-height (window-height-without-modeline window))
         (middle (floor window-height 2))
         (scrolloff (min (floor (/ (window-height window) 2)) 0))
         (top 0))
    (cond
      ((= line middle) (window-recenter window :line scrolloff :from-bottom nil))
      ((= line top) (window-recenter window :line scrolloff :from-bottom t))
      (t (window-recenter window :line nil :from-bottom nil)))))

(defun %calc-window-cursor-x (point window)
  "Return (values cur-x next). the 'next' is a flag if the cursor goes to
next line because it is at the end of width."
  (unless (variable-value 'line-wrap :default (window-buffer window))
    (return-from %calc-window-cursor-x (values (point-column point) nil)))
  (let* ((tab-size (variable-value 'tab-width :default (window-buffer window)))
         (charpos (point-charpos point))
         (line    (line-string point))
         (width   (1- (window-body-width window)))
         (cur-x   0)
         (add-x   (if (< charpos (length line))
                      (char-width (schar line charpos) 0 :tab-size tab-size)
                      1)))
    (loop :for i :from 0 :below charpos
          :for c := (schar line i)
          :do (setf cur-x (char-width c cur-x :tab-size tab-size))
              (when (< width cur-x)
                (setf cur-x (char-width c 0 :tab-size tab-size))))
    (if (< width (+ cur-x add-x))
        (values 0     t)
        (values cur-x nil))))

(defun window-cursor-x (window)
  (multiple-value-bind (x next)
      (%calc-window-cursor-x (window-buffer-point window) window)
    (declare (ignore next))
    x))

(defun map-wrapping-line (window string fn)
  (let ((tab-size (variable-value 'tab-width :default (window-buffer window))))
    (loop :with start := 0
          :and width := (1- (window-body-width window))
          :for i := (wide-index string width :start start :tab-size tab-size)
          :while i
          :do (funcall fn i)
              (setq start i))))

(defun visual-lines-height (window from to)
  "Total height of the visual lines from FROM up to, but not including, TO."
  (with-point ((p from))
    (loop :with height := 0
          :until (point= p to)
          :do (incf height (reduce #'+ (visual-line-rows window p) :key #'cdr))
              (unless (next-visual-line-beginning p)
                (return height))
          :finally (return height))))

;; TODO: we sum the heights of the rows above the cursor and floor the total to whole text rows, so
;; callers can add it to cell coordinates. this is lossy for pixel-based client. we should
;; be returning the exact height instead.
(defun window-cursor-y (window)
  "Distance from the top of WINDOW's view down to the cursor's row, in text row heights.
negative when the cursor is above the view."
  (let ((point (window-buffer-point window)))
    (with-point ((view (window-view-point window))
                 (line point))
      (visual-line-beginning view)
      (visual-line-beginning line)
      (let ((top (point-row-extent (visual-line-rows window line) (count-characters line point))))
        (floor (if (point<= view line)
                   (+ (visual-lines-height window view line) top)
                   (- top (visual-lines-height window line view)))
               (text-row-height))))))

(defun forward-line-wrap (point window)
  (assert (eq (point-buffer point) (window-buffer window)))
  (when (variable-value 'line-wrap :default (point-buffer point))
    (map-wrapping-line window
                       (line-string point)
                       (lambda (i)
                         (when (< (point-charpos point) i)
                           (line-offset point 0 i)
                           (return-from forward-line-wrap point))))))

(defun backward-line-wrap-1 (point window contain-same-line-p)
  (if (and contain-same-line-p (start-line-p point))
      point
      (let (previous-charpos)
        (map-wrapping-line window
                           (line-string point)
                           (lambda (i)
                             (cond ((and contain-same-line-p (= i (point-charpos point)))
                                    (line-offset point 0 i)
                                    (return-from backward-line-wrap-1 point))
                                   ((< i (point-charpos point))
                                    (setf previous-charpos i))
                                   (previous-charpos
                                    (line-offset point 0 previous-charpos)
                                    (return-from backward-line-wrap-1 point))
                                   ((or contain-same-line-p (= i (point-charpos point)))
                                    (line-start point)
                                    (return-from backward-line-wrap-1 point)))))
        (cond (previous-charpos
               (line-offset point 0 previous-charpos))
              (contain-same-line-p
               (line-start point))))))

(defun backward-line-wrap (point window contain-same-line-p)
  (assert (eq (point-buffer point) (window-buffer window)))
  (cond ((variable-value 'line-wrap :default (point-buffer point))
         (backward-line-wrap-1 point window contain-same-line-p))
        (contain-same-line-p
         (line-start point))))

(defun move-to-next-virtual-line-1 (point window)
  (assert (eq (point-buffer point) (window-buffer window)))
  (or (forward-line-wrap point window)
      (line-offset point 1)))

(defun move-to-previous-virtual-line-1 (point window)
  (assert (eq (point-buffer point) (window-buffer window)))
  (backward-line-wrap point window t)
  (or (backward-line-wrap point window nil)
      (progn
        (and (line-offset point -1)
             (line-end point)
             (backward-line-wrap point window t)))))

(defun move-to-next-virtual-line-n (point window n)
  (assert (eq (point-buffer point) (window-buffer window)))
  (when (<= n 0)
    (return-from move-to-next-virtual-line-n point))
  (unless (variable-value 'line-wrap :default (point-buffer point))
    (return-from move-to-next-virtual-line-n (line-offset point n)))
  (loop :with n1 := n
        :do (map-wrapping-line
             window
             (line-string point)
             (lambda (i)
               (when (< (point-charpos point) i)
                 (decf n1)
                 (when (<= n1 0)
                   ;; cursor-x offset is recovered by cursor-saved-column
                   (line-offset point 0 i)
                   (return-from move-to-next-virtual-line-n point)))))
            ;; go to next line
            (unless (line-offset point 1)
              (return-from move-to-next-virtual-line-n nil))
            (decf n1)
            (when (<= n1 0)
              (return-from move-to-next-virtual-line-n point))))

(defun move-to-previous-virtual-line-n (point window n)
  (assert (eq (point-buffer point) (window-buffer window)))
  (when (<= n 0)
    (return-from move-to-previous-virtual-line-n point))
  (unless (variable-value 'line-wrap :default (point-buffer point))
    (return-from move-to-previous-virtual-line-n (line-offset point (- n))))
  (let ((pos-ring  (make-array (1+ n))) ; ring buffer of wrapping position
        (pos-size  (1+ n))
        (pos-count 0)
        (pos-next  0)
        (pos-last  0))
    (flet ((pos-ring-push (pos)
             (setf (aref pos-ring pos-next) pos)
             (incf pos-next)
             (when (>= pos-next pos-size) (setf pos-next 0))
             (incf pos-count)
             (when (> pos-count pos-size)
               (setf pos-count pos-size)
               (incf pos-last)
               (when (>= pos-last pos-size) (setf pos-last 0)))))
      (loop :with n1 := n
            :with first-line := t
            :do (block outer
                  (pos-ring-push 0)
                  (map-wrapping-line
                   window
                   (line-string point)
                   (lambda (i)
                     (when (and first-line
                                (< (point-charpos point) i))
                       (return-from outer))
                     (pos-ring-push i))))
                (when (>= pos-count (1+ n1))
                  ;; cursor-x offset is recovered by cursor-saved-column
                  (line-offset point 0 (aref pos-ring pos-last))
                  (return-from move-to-previous-virtual-line-n point))
                ;; go to previous line
                (unless (line-offset point -1)
                  (return-from move-to-previous-virtual-line-n nil))
                (setf first-line nil)
                (decf n1 pos-count)
                (setf pos-size  (1+ n1)) ; shrink ring-buffer
                (setf pos-count 0)
                (setf pos-next  0)
                (setf pos-last  0)))))

(defun move-to-next-virtual-line (point &optional n (window (current-window)))
  (unless n (setf n 1))
  (unless (zerop n)

    ;; workaround for cursor movement problem
    (when (and *use-cursor-movement-workaround*
               (eq point (window-buffer-point window))
               (variable-value 'line-wrap :default (point-buffer point))
               (numberp (cursor-saved-column point))
               (>= (cursor-saved-column point) (- (window-body-width window) 3)))
      (setf (cursor-saved-column point) 0))

    (if *use-new-vertical-move-function*
        (if (plusp n)
            (move-to-next-virtual-line-n point window n)
            (move-to-previous-virtual-line-n point window (- n)))
        (multiple-value-bind (n f)
            (if (plusp n)
                (values n #'move-to-next-virtual-line-1)
                (values (- n) #'move-to-previous-virtual-line-1))
          (loop :repeat n
                :do (unless (funcall f point window)
                      (return-from move-to-next-virtual-line nil)))
          point))))

(defun move-to-previous-virtual-line (point &optional n (window (current-window)))
  (move-to-next-virtual-line point (if n (- n) -1) window))

(defun point-virtual-line-column (point &optional (window (current-window)))
  (if (variable-value 'line-wrap :default (point-buffer point))
      (let ((column (point-column point)))
        (with-point ((start point))
          (backward-line-wrap start window t)
          (- column (point-column start))))
      (point-column point)))

(defun move-to-virtual-line-column (point column &optional (window (current-window)))
  (backward-line-wrap point window t)
  (let ((w 0))
    (loop
      :while (< w column)
      :do (setf w (char-width (character-at point) w))
          (when (end-line-p point) (return nil))
          (character-offset point 1)
      :finally (return t))))

(defun window-scroll (window n)
  "Move WINDOW's view N visual lines down, up when N is negative. returns true when it moved all N."
  (with-point ((view (window-view-point window)))
    (visual-line-beginning view)
    (prog1 (loop :repeat (abs n)
                 :always (if (plusp n)
                             (next-visual-line-beginning view)
                             (previous-visual-line-beginning view)))
      (set-window-view-start window view))))

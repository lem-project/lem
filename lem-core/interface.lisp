(in-package :lem)

(export '(set-foreground
          set-background
          display-width
          display-height))

(defgeneric interface-invoke (implementation function))
(defgeneric interface-display-background-mode (implementation))
(defgeneric interface-update-foreground (implementation color-name))
(defgeneric interface-update-background (implementation color-name))
(defgeneric interface-display-width (implementation))
(defgeneric interface-display-height (implementation))
(defgeneric interface-make-view (implementation x y width height use-modeline))
(defgeneric interface-delete-view (implementation view))
(defgeneric interface-clear (implementation view))
(defgeneric interface-set-view-size (implementation view width height))
(defgeneric interface-set-view-pos (implementation view x y))
(defgeneric interface-print (implementation view x y string attribute))
(defgeneric interface-print-modeline (implementation view x y string attribute))
(defgeneric interface-clear-eol (implementation view x y))
(defgeneric interface-clear-eob (implementation view x y))
(defgeneric interface-move-cursor (implementation view x y))
(defgeneric interface-redraw-view-after (implementation view focus-window-p))
(defgeneric interface-update-display (implementation))
(defgeneric interface-scroll (implementation view n))

(defparameter *native-scroll-support* nil)

(defvar *implementation*)

(defvar *print-start-x* 0)
(defvar *cursor-x* 0)
(defvar *cursor-y* 0)
(defvar *redraw-start-y*)
(defvar *redraw-end-y*)

(defvar *display-background-mode* nil)

(defun display-background-mode ()
  (or *display-background-mode*
      (interface-display-background-mode *implementation*)))

(defun set-display-background-mode (mode)
  (check-type mode (or (eql :light) (eql :dark) null))
  (setf *display-background-mode* mode))

(defun set-foreground (name)
  (interface-update-foreground *implementation* name))

(defun set-background (name)
  (interface-update-background *implementation* name))

(defun display-width () (interface-display-width *implementation*))
(defun display-height () (interface-display-height *implementation*))

(defun invoke-frontend (function)
  (interface-invoke *implementation* function))

(defstruct (screen (:constructor %make-screen))
  view
  use-modeline
  x
  y
  left-lines
  left-width
  old-left-width
  lines
  old-lines
  wrap-lines
  width
  modified-p
  (horizontal-scroll-start 0))

(defun make-screen (x y width height use-modeline)
  (when use-modeline
    (decf height))
  (let ((view (interface-make-view *implementation* x y width height use-modeline)))
    (%make-screen :view view
                  :use-modeline use-modeline
                  :x x
                  :y y
                  :width width
                  :left-lines (make-array (max 0 height) :initial-element nil)
                  :lines (make-array (max 0 height) :initial-element nil)
                  :old-lines (make-array (max 0 height) :initial-element nil))))

(defun screen-delete (screen)
  (interface-delete-view *implementation* (screen-view screen)))

(defun screen-clear (screen)
  (screen-modify screen)
  (interface-clear *implementation* (screen-view screen)))

(defun screen-height (screen)
  (length (screen-lines screen)))

(defun screen-modify (screen)
  (setf (screen-modified-p screen) t))

(defun screen-set-size (screen width height)
  (screen-modify screen)
  (when (screen-use-modeline screen)
    (decf height))
  (interface-set-view-size *implementation* (screen-view screen) width height)
  (setf (screen-left-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-old-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-width screen)
        width))

(defun screen-set-pos (screen x y)
  (screen-modify screen)
  (setf (screen-x screen) x)
  (setf (screen-y screen) y)
  (interface-set-view-pos *implementation* (screen-view screen) x y))

(defun screen-print-string (screen x y string attribute)
  (when (eq attribute 'cursor)
    (setf *cursor-x* x)
    (setf *cursor-y* y))
  (let ((view (screen-view screen))
        (x0 x)
        (i -1)
        (pool-string (make-string (screen-width screen) :initial-element #\space)))
    (loop :for char :across string
          :do (cond
                ((char= char #\tab)
                 (loop :with size :=
                          (+ *print-start-x*
                             (* (tab-size) (floor (+ (tab-size) x) (tab-size))))
                       :while (< x size)
                       :do (setf (aref pool-string (incf i)) #\space)
                           (incf x)))
                ((char= char #\return)
                 (setf (aref pool-string (incf i)) #\^) (incf x)
                 (setf (aref pool-string (incf i)) #\R) (incf x))
                (t
                 (setf (aref pool-string (incf i)) char)
                 (setf x (char-width char x)))))
    (unless (= i -1)
      (interface-print *implementation* view x0 y
                       (subseq pool-string 0 (1+ i))
                       attribute))
    x))


(defun disp-print-line (screen y str/attributes do-clrtoeol
                        &key (start-x 0) (string-start 0) string-end)
  (when (and *redraw-start-y* (not (<= *redraw-start-y* y *redraw-end-y*)))
    (return-from disp-print-line nil))
  (destructuring-bind (str . attributes)
      str/attributes
    (when (null string-end)
      (setf string-end (length str)))
    (unless (and (= 0 string-start)
                 (= (length str) string-end))
      (setf str (subseq str
                        string-start
                        (if (null string-end)
                            nil
                            (min (length str) string-end))))
      (setf attributes (lem-base::subseq-elements attributes string-start string-end)))
    (let ((prev-end 0)
          (x start-x))
      (loop :for (start end attr) :in attributes
            :do (setf end (min (length str) end))
                (setf x (screen-print-string screen x y (subseq str prev-end start) nil))
                (setf x (screen-print-string screen x y (subseq str start end) attr))
                (setf prev-end end))
      (setf x (screen-print-string screen x y
                                   (if (= prev-end 0)
                                       str
                                       (subseq str prev-end))
                                   nil))
      (when do-clrtoeol
        (interface-clear-eol *implementation* (screen-view screen) x y)))))

#+(or)
(progn
  (defun overlay-line (elements start end attribute)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (declare (type fixnum start end))
    (let ((acc '()))
      (flet ((add (start end attribute)
               (when (< start end)
                 (push (list start end attribute) acc))))
        (if (null elements)
            (add start end attribute)
            (loop :for rest :on elements
                  :for firstp := t :then nil
                  :for prev := nil :then e
                  :for (s e value) :of-type (fixnum fixnum t) :in elements
                  :if firstp :do (add start (the fixnum (+ start s)) attribute)
                  :if (and prev) :do (add (the fixnum (+ start (the fixnum prev)))
                                          (the fixnum (+ start s))
                                          attribute)
                  :do (let ((src-attribute (ensure-attribute value nil)))
                        (add (the fixnum (+ start s))
                             (the fixnum (+ start e))
                             (merge-attribute src-attribute attribute)))
                  :if (null (cdr rest)) :do (add (the fixnum (+ start e)) end attribute))))
      acc))

  (defun disp-set-line (screen attribute screen-row start-charpos end-charpos)
    (when (and (<= 0 screen-row)
               (< screen-row (screen-height screen))
               (not (null (aref (screen-lines screen) screen-row)))
               (or (null end-charpos)
                   (< start-charpos end-charpos)))
      (destructuring-bind (string . attributes)
          (aref (screen-lines screen) screen-row)
        (declare (ignore string))
        (let ((end-charpos (or end-charpos (screen-width screen))))
          (let* ((range-elements
                   (lem-base::subseq-elements attributes
                                              start-charpos
                                              end-charpos)))
            #+(or)
            (when (< (length string) end-charpos)
              (setf (car (aref (screen-lines screen) screen-row))
                    (concatenate 'string
                                 string
                                 (make-string (1- (- end-charpos (length string)))
                                              :initial-element #\space))))
            (setf (cdr (aref (screen-lines screen) screen-row))
                  (lem-base::normalization-elements
                   (nconc (overlay-line range-elements
                                        start-charpos
                                        end-charpos
                                        attribute)
                          (lem-base::remove-elements attributes
                                                     start-charpos
                                                     end-charpos)))))))))
  )

(defun disp-set-line (screen attribute screen-row start-charpos end-charpos)
  (when (and (<= 0 screen-row)
             (< screen-row (screen-height screen))
             (not (null (aref (screen-lines screen) screen-row)))
             (or (null end-charpos)
                 (< start-charpos end-charpos)))
    (destructuring-bind (string . attributes)
        (aref (screen-lines screen) screen-row)
      (when (and end-charpos (<= (length string) end-charpos))
        (setf (car (aref (screen-lines screen) screen-row))
              (concatenate 'string string
                           (make-string (- end-charpos (length string))
                                        :initial-element #\space))))
      (setf (cdr (aref (screen-lines screen) screen-row))
            (lem-base::put-elements attributes
                                    start-charpos
                                    (or end-charpos (length string))
                                    attribute)))))

(defun disp-set-overlay (screen attribute screen-row start end)
  (disp-set-line screen attribute screen-row (point-charpos start) nil)
  (with-point ((point start))
    (line-offset point 1)
    (loop :for i :from (1+ screen-row)
          :do (cond
                ((same-line-p point end)
                 (disp-set-line screen attribute i 0 (point-charpos end))
                 (return))
                (t
                 (disp-set-line screen attribute i 0 nil)
                 (unless (line-offset point 1)
                   (return-from disp-set-overlay)))))))

(defun disp-set-overlays (screen overlays view-point)
  (flet ((calc-row (curr-point) (count-lines view-point curr-point)))
    (let ((left-width 0)
          (view-end-point (with-point ((view-point view-point))
                            (or (line-offset view-point (screen-height screen))
                                (buffer-end view-point)))))
      (loop :for overlay :in overlays
            :for start := (overlay-start overlay)
            :for end := (overlay-end overlay)
            :do (cond
                  ((overlay-get overlay :display-left)
                   (when (and (point<= view-point start)
                              (point<= end view-end-point))
                     (let ((i (calc-row start)))
                       (when (< i (screen-height screen))
                         (let ((str (overlay-get overlay :text)))
                           (setf left-width (max left-width (length str)))
                           (setf (aref (screen-left-lines screen) i)
                                 (cons str (overlay-attribute overlay))))))))
                  ((and (same-line-p start end)
                        (point<= view-point start)
                        (point< start view-end-point))
                   (disp-set-line screen
                                  (overlay-attribute overlay)
                                  (calc-row start)
                                  (point-charpos start)
                                  (point-charpos end)))
                  ((and (point<= view-point start)
                        (point< end view-end-point))
                   (disp-set-overlay screen
                                     (overlay-attribute overlay)
                                     (calc-row start)
                                     start
                                     end))
                  ((and (point<= start view-point)
                        (point<= view-point end)
                        (point<= end view-end-point))
                   (disp-set-overlay screen
                                     (overlay-attribute overlay)
                                     0
                                     view-point
                                     end))
                  ((point<= view-point start)
                   (disp-set-overlay screen
                                     (overlay-attribute overlay)
                                     (calc-row start)
                                     start
                                     view-end-point))))
      (setf (screen-left-width screen) left-width))))

(defun maybe-push-mark-overlay (window)
  (when (eq window (current-window))
    (let ((buffer (window-buffer window)))
      (when (buffer-mark-p buffer)
        (let ((start (buffer-point buffer))
              (end (buffer-mark buffer)))
          (when (point< end start)
            (rotatef start end))
          (lem::make-temporary-overlay start end 'region))))))

(defun maybe-set-cursor-attribute (window screen view-point)
  (when (eq window (current-window))
    (let* ((buffer (window-buffer window))
           (point (buffer-point buffer))
           (charpos (point-charpos point)))
      (disp-set-line screen
                     'cursor
                     (count-lines view-point point)
                     charpos
                     (1+ charpos)))))

(defun disp-reset-lines (window)
  (let ((screen (lem::window-screen window))
        (buffer (window-buffer window))
        (view-point (lem::window-view-point window)))
    (with-point ((point view-point))
      (loop :for i :from 0 :below (screen-height screen)
            :do (let* ((line (lem-base::point-line point))
                       (str/attributes (lem-base::line-string/attributes line)))
                  (setf (aref (screen-left-lines screen) i) nil)
                  (setf (aref (screen-lines screen) i) str/attributes))
                (unless (line-offset point 1)
                  (fill (screen-lines screen) nil :start (1+ i))
                  (return))))
    (let ((overlays (lem::overlays buffer))
          ov)
      (when (setf ov (maybe-push-mark-overlay window)) (push ov overlays))
      (disp-set-overlays screen overlays view-point)
      (maybe-set-cursor-attribute window screen view-point))))

(defvar *truncate-character* #\\)

(defun screen-display-line-wrapping (screen screen-width view-charpos cursor-y point-y str/attributes)
  (declare (ignore cursor-y))
  (when (and (< 0 view-charpos) (= point-y 0))
    (setf str/attributes
          (cons (subseq (car str/attributes) view-charpos)
                (lem-base::subseq-elements (cdr str/attributes)
                                           view-charpos
                                           (length (car str/attributes))))))
  (let ((start 0)
        (start-x (screen-left-width screen))
        (truncate-str/attributes
          (cons (string *truncate-character*)
                (list (list 0 1 'lem:truncate-attribute)))))
    (loop :for i := (wide-index (car str/attributes)
                                (1- screen-width)
                                :start start)
          :while (< point-y (screen-height screen))
          :do (cond ((null i)
                     (disp-print-line screen point-y str/attributes t
                                      :string-start start :start-x start-x)
                     (return))
                    (t
                     (disp-print-line screen point-y str/attributes t
                                      :string-start start :string-end i
                                      :start-x start-x)
                     (disp-print-line screen point-y
                                      truncate-str/attributes
                                      t
                                      :start-x (+ start-x (1- screen-width)))
                     (incf point-y)
                     (setf start i))))
    point-y))

(defun screen-display-line (screen screen-width view-charpos cursor-y point-y str/attributes)
  (declare (ignore view-charpos))
  (let ((start-x (screen-left-width screen))
        start
        end)
    (cond ((= cursor-y point-y)
           (setf start (or (wide-index (car str/attributes)
                                       (screen-horizontal-scroll-start screen))
                           0))
           (setf end (wide-index (car str/attributes)
                                 (+ (screen-horizontal-scroll-start screen)
                                    screen-width))))
          (t
           (setf start 0)
           (setf end (wide-index (car str/attributes) screen-width))))
    (interface-clear-eol *implementation* (screen-view screen) start-x point-y)
    (disp-print-line screen point-y str/attributes nil
                     :start-x start-x
                     :string-start start
                     :string-end end))
  point-y)

(defun adjust-horizontal-scroll (window)
  (let ((screen (lem::window-screen window))
        (buffer (window-buffer window)))
    (unless (variable-value 'truncate-lines :default buffer)
      (let ((point-column (point-column (buffer-point buffer)))
            (width (- (screen-width screen) (screen-left-width screen))))
        (cond ((<= (+ (screen-horizontal-scroll-start screen) width)
                   (1+ point-column))
               (setf (screen-horizontal-scroll-start screen)
                     (- (1+ point-column) width)))
              ((< point-column (screen-horizontal-scroll-start screen))
               (setf (screen-horizontal-scroll-start screen) point-column)))))))

(defun screen-display-lines (screen redraw-flag buffer view-charpos cursor-y)
  (let* ((lem-base::*tab-size* (variable-value 'tab-width :default buffer))
         (truncate-lines (variable-value 'truncate-lines :default buffer))
         (disp-line-function
           (if truncate-lines
               #'screen-display-line-wrapping
               #'screen-display-line))
         (wrap-lines (screen-wrap-lines screen))
         (screen-width (- (screen-width screen)
                          (screen-left-width screen)))
         (*print-start-x* (screen-left-width screen)))
    (setf (screen-wrap-lines screen) nil)
    (loop :for y :from 0
          :for i :from 0
          :for str/attributes :across (screen-lines screen)
          :for left-str/attr :across (screen-left-lines screen)
          :while (< y (screen-height screen))
          :do (cond
                ((and (null left-str/attr)
                      (not redraw-flag)
                      (not (null str/attributes))
                      #1=(aref (screen-old-lines screen) i)
                      (equal str/attributes #1#)
                      (/= cursor-y i))
                 (let ((n (count i wrap-lines)))
                   (incf y n)
                   (dotimes (_ n)
                     (push i (screen-wrap-lines screen)))))
                (str/attributes
                 (setf (aref (screen-old-lines screen) i) str/attributes)
                 (when (zerop (length (car str/attributes)))
                   (interface-clear-eol *implementation* (screen-view screen) 0 y))
                 (let (y2)
                   (when left-str/attr
                     (screen-print-string screen
                                          0
                                          y
                                          (car left-str/attr)
                                          (cdr left-str/attr)))
                   (setq y2
                         (funcall disp-line-function
                                  screen
                                  screen-width
                                  view-charpos
                                  cursor-y
                                  y
                                  str/attributes))
                   (cond
                     (truncate-lines
                      (let ((offset (- y2 y)))
                        (cond ((< 0 offset)
                               (setf redraw-flag t)
                               (dotimes (_ offset)
                                 (push i (screen-wrap-lines screen))))
                              ((and (= offset 0) (find i wrap-lines))
                               (setf redraw-flag t))))
                      (setf y y2))
                     (t
                      (setf (aref (screen-lines screen) i) nil)))))
                (t
                 (fill (screen-old-lines screen) nil :start i)
                 (interface-clear-eob *implementation* (screen-view screen) 0 y)
                 (return))))))

(defun screen-redraw-modeline (window)
  (let* ((screen (window-screen window))
         (view (screen-view screen))
         (default-attribute (if (eq window (current-window))
                                'modeline
                                'modeline-inactive))
         (left-x 0)
         (right-x (window-width window)))
    (interface-print-modeline *implementation* view 0 0
                              (make-string (window-width window)
                                           :initial-element #\space)
                              default-attribute)
    (modeline-apply window
                    (lambda (string attribute alignment)
                      (case alignment
                        ((:right)
                         (decf right-x (length string))
                         (interface-print-modeline *implementation*
                                                   view right-x 0 string attribute))
                        (otherwise
                         (interface-print-modeline *implementation*
                                                   view left-x 0 string attribute)
                         (incf left-x (length string)))))
                    default-attribute)))

(defun redraw-display-window (window force)
  (let ((focus-window-p (eq window (current-window)))
        (screen (lem::window-screen window)))
    (let ((scroll-n (when focus-window-p
                      (window-see window))))
      (when (or (not *native-scroll-support*)
                (and scroll-n (>= scroll-n (screen-height screen))))
        (setf scroll-n nil))
      (when scroll-n
        (interface-scroll *implementation* (screen-view screen) scroll-n))
      (multiple-value-bind (*redraw-start-y* *redraw-end-y*)
          (when scroll-n
            (if (plusp scroll-n)
                (values (- (screen-height screen) scroll-n) (screen-height screen))
                (values 0 (- scroll-n))))
        (lem::run-show-buffer-hooks window)
        (disp-reset-lines window)
        (adjust-horizontal-scroll window)
        (screen-display-lines screen
                              (or force
                                  (screen-modified-p screen)
                                  (not (eql (screen-left-width screen)
                                            (screen-old-left-width screen))))
                              (window-buffer window)
                              (point-charpos (window-view-point window))
                              (if focus-window-p
                                  (count-lines (window-view-point window)
                                               (lem::window-point window))
                                  0))
        (setf (screen-old-left-width screen)
              (screen-left-width screen))
        (when (lem::window-use-modeline-p window)
          (screen-redraw-modeline window))
        (interface-redraw-view-after *implementation* (screen-view screen) focus-window-p)
        (setf (screen-modified-p screen) nil)))))

(defun update-display ()
  (interface-update-display *implementation*))

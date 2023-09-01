(in-package :lem-core)

(define-editor-variable highlight-line nil)

(defvar *inactive-window-background-color* nil)

(defun call-with-display-error (function)
  (handler-bind ((error (lambda (e)
                          (log:error "~A"
                                     (with-output-to-string (out)
                                       (format out "~A~%" e)
                                       (uiop:print-backtrace :stream out :condition e)))
                          (message "~A" e)
                          (return-from call-with-display-error))))
    (funcall function)))

(defmacro with-display-error (() &body body)
  `(call-with-display-error (lambda () ,@body)))

(defgeneric compute-left-display-area-content (mode buffer point)
  (:method (mode buffer point) nil))

(defun overlay-attributes (under-attributes over-start over-end over-attribute)
  ;; under-attributes := ((start-charpos end-charpos attribute) ...)
  (let* ((over-attribute (ensure-attribute over-attribute))
         (under-part-attributes (lem-base::subseq-elements under-attributes
                                                           over-start
                                                           over-end))
         (merged-attributes (lem-base::remove-elements under-attributes
                                                       over-start
                                                       over-end)))
    (flet ((add-element (start end attribute)
             (when (< start end)
               (push (list start end (ensure-attribute attribute))
                     merged-attributes))))
      (if (null under-part-attributes)
          (add-element over-start over-end over-attribute)
          (loop :for prev-under := 0 :then under-end-offset
                :for (under-start-offset under-end-offset under-attribute)
                :in under-part-attributes
                :do (add-element (+ over-start prev-under)
                                 (+ over-start under-start-offset)
                                 over-attribute)
                    (add-element (+ over-start under-start-offset)
                                 (+ over-start under-end-offset)
                                 (alexandria:if-let (under-attribute
                                                     (ensure-attribute under-attribute nil))
                                   (merge-attribute under-attribute
                                                    over-attribute)
                                   over-attribute))
                :finally (add-element (+ over-start under-end-offset)
                                      over-end
                                      over-attribute))))
    (lem-base::normalization-elements merged-attributes)))

(defun draw-attribute-to-screen-line (screen attribute screen-row start-charpos end-charpos
                                      &key (transparency t))
  ;; transparencyがTのとき、オーバーレイがその下のテキストの属性を消さずに
  ;; 下のattributeを上のattributeとマージします。(透過する)
  ;; NILのとき、オーバーレイの下のテキスト属性はオーバーレイの色で置き換えられます。
  ;;
  ;; 常に透過せず真偽値で切り替えているのはカーソルもオーバーレイとして扱うため、マージすると
  ;; シンボルのcursorという値でattributeを保持できなくなってしまいeqで判別できなくなるためです。
  ;; cursorというシンボルのattributeには特別な意味があり、画面描画フェーズでカーソルに
  ;; 対応する箇所を表示するときcursorとeqならその(x, y)座標にカーソルがあることがわかります。
  ;; たとえばncursesでは、カーソル位置を物理的なカーソルにセットするためにCのwmove関数を呼びます。
  (when (and (<= 0 screen-row)
             (< screen-row (screen-height screen))
             (not (null (aref (screen-lines screen) screen-row)))
             (or (null end-charpos)
                 (< start-charpos end-charpos)))
    (destructuring-bind (string . attributes)
        (aref (screen-lines screen) screen-row)

      (unless end-charpos
        (let* ((width (string-width string))
               (n (1+ (floor width (screen-width screen)))))
          (setf end-charpos
                (+ (length string)
                   (- (- (* (screen-width screen) n) (1- n))
                      width)
                   (if (screen-left-width screen)
                       (- (screen-left-width screen))
                       0)
                   -1))))

      (when (and end-charpos (<= (length string) end-charpos))
        (setf (car (aref (screen-lines screen) screen-row))
              (concatenate 'string
                           string
                           (make-string (- end-charpos (length string))
                                        :initial-element #\space))))
      (setf (cdr (aref (screen-lines screen) screen-row))
            (if transparency
                (overlay-attributes attributes
                                    start-charpos
                                    end-charpos
                                    attribute)
                (lem-base::put-elements attributes
                                        start-charpos
                                        end-charpos
                                        attribute))))))

(defun draw-attribute-to-screen-region (screen attribute screen-row start end)
  (flet ((draw-line (row start-charpos &optional end-charpos)
           (draw-attribute-to-screen-line screen attribute row start-charpos end-charpos)))
    (with-point ((point start))
      (loop :for start-charpos := (point-charpos start) :then 0
            :for row :from screen-row
            :do (cond ((same-line-p point end)
                       (draw-line row start-charpos (point-charpos end))
                       (return))
                      (t
                       (draw-line row start-charpos)))
            :while (line-offset point 1)))))

(defun highlight-line-color ()
  (when (background-color)
    (let ((color (parse-color (background-color))))
      (multiple-value-bind (h s v)
          (rgb-to-hsv (color-red color)
                      (color-green color)
                      (color-blue color))
        (multiple-value-bind (r g b)
            (hsv-to-rgb h
                        s
                        (max 0 (- v 2)))
          (format nil "#~2,'0X~2,'0X~2,'0X" r g b))))))

(defun make-temporary-highlight-line-overlay (buffer)
  (when (and (variable-value 'highlight-line :default (current-buffer))
             (current-theme))
    (alexandria:when-let ((color (highlight-line-color)))
      (make-overlay-line (buffer-point buffer)
                         (make-attribute :background color)
                         :temporary t))))

(defun make-temporary-region-overlay-from-cursor (cursor)
  (let ((mark (cursor-mark cursor)))
    (when (mark-active-p mark)
      (make-overlay cursor
                    (mark-point mark)
                    'region
                    :temporary t))))

(defun get-window-overlays (window)
  (let* ((buffer (window-buffer window))
         (overlays (buffer-overlays buffer)))
    (when (eq (current-window) window)
      (dolist (cursor (buffer-cursors buffer))
        (if-push (make-temporary-region-overlay-from-cursor cursor)
                 overlays))
      (if-push (make-temporary-highlight-line-overlay buffer) overlays))
    overlays))

(defun draw-window-overlays-to-screen (window)
  (let ((screen (window-screen window))
        (view-point (window-view-point window)))
    (flet ((calc-row (curr-point)
             (count-lines view-point curr-point))
           (cover (str/attributes str attribute offset)
             (let ((space (make-string offset :initial-element #\space)))
               (cons (concatenate 'string (car str/attributes) space str)
                     (lem-base::put-elements (cdr str/attributes)
                                             (+ (length (car str/attributes)) (length space))
                                             (+ (length (car str/attributes))
                                                (length space)
                                                (length str))
                                             attribute)))))
      (let ((view-end-point (with-point ((view-point view-point))
                              (or (line-offset view-point (screen-height screen))
                                  (buffer-end view-point))))
            (overlays (get-window-overlays window)))
        (loop :for overlay :in overlays
              :for start := (overlay-start overlay)
              :for end := (overlay-end overlay)
              :do (when (typep overlay 'overlay-line-endings)
                    (when (and (point<= view-point start)
                               (point<= end view-end-point))
                      (let ((i (calc-row end)))
                        (when (< i (screen-height screen))
                          (let ((text (overlay-line-endings-text overlay)))
                            (setf (aref (screen-lines screen) i)
                                  (cover (aref (screen-lines screen) i)
                                         text
                                         (overlay-attribute overlay)
                                         (overlay-line-endings-offset overlay)))))))))
        (loop :for overlay :in overlays
              :for start := (overlay-start overlay)
              :for end := (overlay-end overlay)
              :do (cond
                    ((typep overlay 'overlay-line-endings))
                    ((and (same-line-p start end)
                          (point<= view-point start)
                          (point< start view-end-point))
                     (draw-attribute-to-screen-line screen
                                                    (overlay-attribute overlay)
                                                    (calc-row start)
                                                    (if (typep overlay 'overlay-line)
                                                        0
                                                        (point-charpos start))
                                                    (if (typep overlay 'overlay-line)
                                                        nil
                                                        (point-charpos end))))
                    ((and (point<= view-point start)
                          (point< end view-end-point))
                     (draw-attribute-to-screen-region screen
                                                      (overlay-attribute overlay)
                                                      (calc-row start)
                                                      start
                                                      end))
                    ((and (point<= start view-point)
                          (point<= view-point end)
                          (point<= end view-end-point))
                     (draw-attribute-to-screen-region screen
                                                      (overlay-attribute overlay)
                                                      0
                                                      view-point
                                                      end))
                    ((point<= view-point start)
                     (draw-attribute-to-screen-region screen
                                                      (overlay-attribute overlay)
                                                      (calc-row start)
                                                      start
                                                      view-end-point))))))))

(defun draw-point-to-screen (screen view-point cursor-point attribute)
  (let ((charpos (point-charpos cursor-point)))
    (draw-attribute-to-screen-line screen
                                   attribute
                                   (count-lines view-point cursor-point)
                                   charpos
                                   (1+ charpos)
                                   :transparency nil)))

(defun draw-cursor-to-screen (window)
  (when (eq (current-window) window)
    (let ((buffer (window-buffer window)))
      (dolist (point (buffer-fake-cursors buffer))
        (draw-point-to-screen (window-screen window)
                              (window-view-point window)
                              point
                              'fake-cursor))
      (draw-point-to-screen (window-screen window)
                            (window-view-point window)
                            (buffer-point buffer)
                            'cursor))))

(defun reset-screen-lines (screen view-point)
  (with-point ((point view-point))
    (let ((left-width 0)
          (active-modes (get-active-modes-class-instance (point-buffer view-point))))
      (loop :with buffer := (point-buffer point)
            :for row :from 0 :below (screen-height screen)
            :do (let* ((line (lem-base::point-line point))
                       (str/attributes (lem-base::line-string/attributes line)))
                  (setf (aref (screen-lines screen) row) str/attributes))
                (let ((content (compute-left-display-area-content active-modes buffer point)))
                  (cond (content
                         (setf left-width (max left-width (length (lem-base::content-string content))))
                         (setf (aref (screen-left-lines screen) row)
                               (cons (lem-base::content-string content)
                                     (third (first (lem-base::content-attributes content))))))
                        (t
                         (setf (aref (screen-left-lines screen) row) nil))))
                (unless (line-offset point 1)
                  (fill (screen-lines screen) nil :start (1+ row))
                  (fill (screen-left-lines screen) nil :start (1+ row))
                  (return)))
      (setf (screen-left-width screen) left-width))))

(defun draw-window-to-screen (window)
  (reset-screen-lines (window-screen window) (window-view-point window))
  (draw-window-overlays-to-screen window)
  (unless (window-cursor-invisible-p window)
    (draw-cursor-to-screen window)))


(defvar *printing-tab-size*)

(defun screen-margin-left (screen)
  (screen-left-width screen))

(defun screen-print-string (screen x y string attribute)
  (when (and (eq attribute 'cursor) (< 0 (length string)))
    (setf (screen-last-print-cursor-x screen) x
          (screen-last-print-cursor-y screen) y))
  (let ((view (screen-view screen))
        (x0 x)
        (i -1)
        (pool-string (make-string (screen-width screen) :initial-element #\space)))
    (loop :for char :across string
          :do (cond
                ((char= char #\tab)
                 (loop :with size :=
                          (+ (screen-margin-left screen)
                             (* *printing-tab-size*
                                (floor (+ *printing-tab-size* x) *printing-tab-size*)))
                       :while (< x size)
                       :do (setf (aref pool-string (incf i)) #\space)
                           (incf x)))
                ((alexandria:when-let ((control-char (control-char char)))
                   (loop :for c :across control-char
                         :do (setf (aref pool-string (incf i)) c
                                   x (char-width c x)))
                   t))
                (t
                 (setf (aref pool-string (incf i)) char)
                 (setf x (char-width char x)))))
    (unless (= i -1)
      (lem-if:print (implementation) view x0 y
                    (subseq pool-string 0 (1+ i))
                    attribute))
    x))

(defun disp-print-line (screen y str/attributes do-clrtoeol
                        &key (start-x 0) (string-start 0) string-end)
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
        (lem-if:clear-eol (implementation) (screen-view screen) x y)))))

(define-editor-variable truncate-character #\\)
(defvar *truncate-character*)

(defun screen-display-line-wrapping (screen screen-width view-charpos cursor-y point-y
                                     str/attributes)
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
                (list (list 0 1 'truncate-attribute)))))
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
    (lem-if:clear-eol (implementation) (screen-view screen) start-x point-y)
    (disp-print-line screen point-y str/attributes nil
                     :start-x start-x
                     :string-start start
                     :string-end end))
  point-y)

(defun screen-display-lines (screen redraw-flag buffer view-charpos cursor-y)
  (let* ((*truncate-character*
           (variable-value 'truncate-character :default buffer))
         (*printing-tab-size* (variable-value 'tab-width :default buffer))
         (line-wrap (variable-value 'line-wrap :default buffer))
         (disp-line-function
           (if line-wrap
               #'screen-display-line-wrapping
               #'screen-display-line))
         (wrap-lines (screen-wrap-lines screen))
         (screen-width (- (screen-width screen)
                          (screen-left-width screen))))
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
                      #+(or)(/= cursor-y i))
                 (let ((n (count i wrap-lines)))
                   (incf y n)
                   (dotimes (_ n)
                     (push i (screen-wrap-lines screen)))))
                (str/attributes
                 (setf (aref (screen-old-lines screen) i) str/attributes)
                 (when (zerop (length (car str/attributes)))
                   (lem-if:clear-eol (implementation) (screen-view screen) 0 y))
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
                     (line-wrap
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
                 (lem-if:clear-eob (implementation) (screen-view screen) 0 y)
                 (return))))))

(defun redraw-modeline (window force)
  (when (window-use-modeline-p window)
    (let* ((screen (window-screen window))
           (view (screen-view screen))
           (default-attribute (if (eq window (current-window))
                                  'modeline
                                  'modeline-inactive))
           (elements '())
           (left-x 0)
           (right-x (window-width window)))
      (modeline-apply window
                      (lambda (string attribute alignment)
                        (case alignment
                          ((:right)
                           (decf right-x (length string))
                           (push (list right-x string attribute) elements))
                          (otherwise
                           (push (list left-x string attribute) elements)
                           (incf left-x (length string)))))
                      default-attribute)
      (setf elements (nreverse elements))
      (when (or force (not (equal elements (screen-modeline-elements screen))))
        (setf (screen-modeline-elements screen) elements)
        (lem-if:print-modeline (implementation) view 0 0
                               (make-string (window-width window) :initial-element #\space)
                               default-attribute)
        (loop :for (x string attribute) :in elements
              :do (lem-if:print-modeline (implementation) view x 0 string attribute))))))

(defun adjust-horizontal-scroll (window)
  (let ((screen (window-screen window))
        (buffer (window-buffer window)))
    (unless (variable-value 'line-wrap :default buffer)
      (let ((point-column (point-column (buffer-point buffer)))
            (width (- (screen-width screen) (screen-left-width screen))))
        (cond ((<= (+ (screen-horizontal-scroll-start screen) width)
                   (1+ point-column))
               (setf (screen-horizontal-scroll-start screen)
                     (- (1+ point-column) width)))
              ((< point-column (screen-horizontal-scroll-start screen))
               (setf (screen-horizontal-scroll-start screen) point-column)))))))

(defgeneric redraw-buffer (buffer window force))

(defmethod redraw-buffer :around (buffer window force)
  (with-display-error ()
    (let ((lem-if:*background-color-of-drawing-window*
            (get-background-color-of-window window)))
      (call-next-method))))

(defun get-background-color-of-window (window)
  (cond ((typep window 'floating-window)
         (floating-window-background-color window))
        ((eq window (current-window))
         nil)
        ((eq window (window-parent (current-window)))
         nil)
        ((and *inactive-window-background-color*
              (eq 'window (type-of window)))
         *inactive-window-background-color*)
        (t nil)))

(defmethod redraw-buffer :before ((buffer text-buffer) window force)
  (lem-if:redraw-view-before (implementation)
                             (screen-view (window-screen window))))

(defmethod redraw-buffer :after ((buffer text-buffer) window force)
  (when (window-use-modeline-p window)
    (redraw-modeline window (or (screen-modified-p (window-screen window))
                                force)))
  (lem-if:redraw-view-after (implementation)
                            (screen-view (window-screen window))))

(defun get-cursor-y-on-screen (window)
  (if (eq window (current-window))
      (count-lines (window-view-point window)
                   (window-point window))
      -1))

(defmethod redraw-buffer ((buffer text-buffer) window force)
  (assert (eq buffer (window-buffer window)))
  (let ((screen (window-screen window)))
    (draw-window-to-screen window)
    (adjust-horizontal-scroll window)
    (screen-display-lines screen
                          (or force (required-whole-update-screen-p screen))
                          buffer
                          (point-charpos (window-view-point window))
                          (get-cursor-y-on-screen window))
    (when (or force (required-whole-update-screen-p screen))
      (lem-if:force-update-view (implementation) (screen-view screen)))
    (update-screen-cache screen buffer)))

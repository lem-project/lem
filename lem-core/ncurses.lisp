(uiop:define-package :lem-ncurses
  (:use :cl :lem :lem.term :lem-interface)
  (:reexport :lem-interface))
(in-package :lem-ncurses)

(defvar *print-start-x* 0)

(defun attribute-to-bits (attribute-or-name)
  (let ((attribute (ensure-attribute attribute-or-name nil)))
    (if (null attribute)
        0
        (or (lem::attribute-%internal-value attribute)
            (let ((bits (logior (get-color-pair (lem::attribute-foreground attribute)
                                                (lem::attribute-background attribute))
                                (if (lem::attribute-reverse-p attribute)
                                    charms/ll:a_reverse
                                    0)
                                (if (lem::attribute-bold-p attribute)
                                    charms/ll:a_bold
                                    0)
                                (if (lem::attribute-underline-p attribute)
                                    charms/ll:a_underline
                                    0))))
              (setf (lem::attribute-%internal-value attribute) bits)
              bits)))))

(defvar *display-background-mode* nil)

(define-implementation display-background-mode ()
  (or *display-background-mode*
      (lem.term:background-mode)))

(define-implementation set-display-background-mode (mode)
  (check-type mode (or (eql :light) (eql :dark) null))
  (setf *display-background-mode* mode))

(define-implementation set-foreground (name)
  (or (term-set-foreground name)
      (error "Undefined color: ~A" name)))

(define-implementation set-background (name)
  (or (term-set-background name)
      (error "Undefined color: ~A" name)))

(defun newwin (nlines ncols begin-y begin-x)
  (let ((win (charms/ll:newwin nlines ncols begin-y begin-x)))
    (charms/ll:keypad win 1)
    win))

(defun display-init ()
  (term-init))

(defun display-finalize ()
  (term-finalize))

(define-implementation display-width () (max 5 charms/ll:*cols*))
(define-implementation display-height () (max 3 charms/ll:*lines*))

(define-implementation call-with-screen (function)
  (unwind-protect (progn
                    (display-init)
                    (funcall function))
    (display-finalize)))

(defstruct (screen (:constructor %make-screen))
  %scrwin
  %modeline-scrwin
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

(define-implementation make-screen (x y width height use-modeline-p)
  (when use-modeline-p
    (decf height))
  (%make-screen :%scrwin (newwin height width y x)
                :%modeline-scrwin (when use-modeline-p
                                    (newwin 1 width (+ y height) x))
                :x x
                :y y
                :width width
                :left-lines (make-array (max 0 height) :initial-element nil)
                :lines (make-array (max 0 height) :initial-element nil)
                :old-lines (make-array (max 0 height) :initial-element nil)))

(define-implementation screen-delete (screen)
  (charms/ll:delwin (screen-%scrwin screen))
  (when (screen-%modeline-scrwin screen)
    (charms/ll:delwin (screen-%modeline-scrwin screen))))

(define-implementation screen-clear (screen)
  (screen-modify screen)
  (charms/ll:clearok (screen-%scrwin screen) 1)
  (when (screen-%modeline-scrwin screen)
    (charms/ll:clearok (screen-%modeline-scrwin screen) 1)))

(define-implementation screen-erase (screen)
  (charms/ll:werase (screen-%scrwin screen)))

(defun screen-height (screen)
  (length (screen-lines screen)))

(define-implementation screen-modify (screen)
  (setf (screen-modified-p screen) t))

(define-implementation screen-set-size (screen width height)
  (screen-modify screen)
  (when (screen-%modeline-scrwin screen)
    (decf height))
  (charms/ll:wresize (screen-%scrwin screen)
                     height
                     width)
  (when (screen-%modeline-scrwin screen)
    (charms/ll:mvwin (screen-%modeline-scrwin screen)
                     (+ (screen-y screen) height)
                     (screen-x screen))
    (charms/ll:wresize (screen-%modeline-scrwin screen)
                       (minibuffer-window-height)
                       width))
  (setf (screen-left-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-old-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-width screen)
        width))

(define-implementation screen-set-pos (screen x y)
  (screen-modify screen)
  (setf (screen-x screen) x)
  (setf (screen-y screen) y)
  (charms/ll:mvwin (screen-%scrwin screen) y x)
  (when (screen-%modeline-scrwin screen)
    (charms/ll:mvwin (screen-%modeline-scrwin screen)
                     (+ y (screen-height screen))
                     x)))

(defun scrwin-print-string (scrwin x y string attr)
  (setf attr (attribute-to-bits attr))
  (charms/ll:wattron scrwin attr)
  (loop :for char :across string
        :do (cond ((char= char #\tab)
                   (loop :with size := (+ *print-start-x*
                                          (* (tab-size) (floor (+ (tab-size) x) (tab-size))))
                         :while (< x size)
                         :do
                         (charms/ll:mvwaddch scrwin y x #.(char-code #\space))
                         (incf x)))
                  (t
                   (charms/ll:mvwaddstr scrwin y x
                                        (if (char= char #\return)
                                            "^R"
                                            (string char)))
                   (setf x (char-width char x)))))
  (charms/ll:wattroff scrwin attr)
  x)

(define-implementation screen-print-string (screen x y string attribute)
  (scrwin-print-string (screen-%scrwin screen) x y string attribute))

(define-implementation screen-move-cursor (screen x y)
  (charms/ll:wmove (screen-%scrwin screen) y x))


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
            :do
            (setf end (min (length str) end))
            (setf x (screen-print-string screen x y (subseq str prev-end start) nil))
            (setf x (screen-print-string screen x y (subseq str start end) attr))
            (setf prev-end end))
      (screen-print-string screen x y
                           (if (= prev-end 0)
                               str
                               (subseq str prev-end))
                           nil))
    (when do-clrtoeol
      (charms/ll:wclrtoeol (screen-%scrwin screen)))))

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
            :do
            (let* ((line (lem-base::point-line point))
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
    (charms/ll:wmove (screen-%scrwin screen) point-y start-x)
    (charms/ll:wclrtoeol (screen-%scrwin screen))
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
  (let* ((truncate-lines (variable-value 'truncate-lines :default buffer))
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
                   (charms/ll:wmove (screen-%scrwin screen) y 0)
                   (charms/ll:wclrtoeol (screen-%scrwin screen)))
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
                 (charms/ll:wmove (screen-%scrwin screen) y 0)
                 (charms/ll:wclrtobot (screen-%scrwin screen))
                 (return))))))

(defun screen-redraw-separator (window)
  (let ((attr (attribute-to-bits 'modeline)))
    (charms/ll:attron attr)
    (when (< 0 (window-x window))
      (charms/ll:move (window-y window) (1- (window-x window)))
      (charms/ll:vline (char-code #\space) (window-height window)))
    (charms/ll:attroff attr)
    (charms/ll:wnoutrefresh charms/ll:*stdscr*)))

(defun screen-redraw-modeline (window)
  (let ((scrwin (screen-%modeline-scrwin
                 (lem::window-screen window)))
        (default-attribute (if (eq window (current-window))
                               'modeline
                               'modeline-inactive))
        (left-x 0)
        (right-x (window-width window)))
    (scrwin-print-string scrwin 0 0
                         (make-string (window-width window)
                                      :initial-element #\space)
                         default-attribute)
    (lem::modeline-apply window
                         (lambda (string attribute alignment)
                           (case alignment
                             ((:right)
                              (decf right-x (length string))
                              (scrwin-print-string scrwin right-x 0 string attribute))
                             (otherwise
                              (scrwin-print-string scrwin left-x 0 string attribute)
                              (incf left-x (length string)))))
                         default-attribute)
    (charms/ll:wnoutrefresh scrwin)))

(define-implementation redraw-display-window (window force)
  (let ((focus-window-p (eq window (current-window)))
        (screen (lem::window-screen window)))
    (when focus-window-p (window-see window))
    (lem::window-prompt-display window)
    (progn
      #+(or)without-interrupts
      (disp-reset-lines window)
      (adjust-horizontal-scroll window)
      (screen-display-lines screen
                            (or force
                                (screen-modified-p screen)
                                (not (eql (screen-left-width screen)
                                          (screen-old-left-width screen))))
                            (window-buffer window)
                            (point-charpos (lem::window-view-point window))
                            (if focus-window-p
                                (count-lines (lem::window-view-point window)
                                             (lem::window-point window))
                                0))
      (setf (screen-old-left-width screen)
            (screen-left-width screen))
      (when (lem::window-use-modeline-p window)
        (screen-redraw-separator window)
        (screen-redraw-modeline window))
      (charms/ll:wnoutrefresh (screen-%scrwin screen))
      (setf (screen-modified-p screen) nil))))

(define-implementation update-display ()
  (charms/ll:doupdate))

(define-implementation input-loop (editor-thread)
  (loop
    (unless (bt:thread-alive-p editor-thread) (return))
    (let ((code (charms/ll:getch)))
      (cond ((= code -1))
            ((= code 410)
             (loop :while (< 0 (lem::event-queue-length)) :do
               (sleep 0.01))
             (lem::change-display-size-hook t))
            ((= code #.(char-code lem::C-\]))
             (send-abort-event editor-thread))
            (t
             (send-event
              (let ((nbytes (utf8-bytes code)))
                (if (= nbytes 1)
                    (code-char code)
                    (let ((vec (make-array nbytes :element-type '(unsigned-byte 8))))
                      (setf (aref vec 0) code)
                      (loop :for i :from 1 :below nbytes
                            :do (setf (aref vec i) (charms/ll:getch)))
                      (schar (babel:octets-to-string vec) 0))))))))))

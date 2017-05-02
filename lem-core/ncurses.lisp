(uiop:define-package :lem-ncurses
  (:use :cl :lem :lem.term :lem-interface)
  (:reexport :lem-interface))
(in-package :lem-ncurses)

(defvar *echo-area-scrwin*)

(defvar *old-display-width*)
(defvar *old-display-height*)

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
  (term-init)
  (setq *old-display-width* charms/ll:*cols*)
  (setq *old-display-height* charms/ll:*lines*)
  (setf *echo-area-scrwin*
        (newwin (minibuffer-window-height)
                (display-width)
                (- (display-height) (minibuffer-window-height))
                0)))

(defun display-finalize ()
  (term-finalize))

(define-implementation display-width () charms/ll:*cols*)
(define-implementation display-height () charms/ll:*lines*)

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
  lines
  old-lines
  wrap-lines
  width
  modified-p)

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
                   (loop :with size := (+ *print-start-x* (* (tab-size) (floor (+ (tab-size) x) (tab-size))))
                         :while (< x size)
                         :do
                         (charms/ll:mvwaddch scrwin y x #.(char-code #\space))
                         (incf x)))
                  (t
                   (charms/ll:mvwaddstr scrwin y x (string char))
                   (setf x (char-width char x)))))
  (charms/ll:wattroff scrwin attr))

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
            :do (setf end (min (length str) end))
            :do (progn
                  (screen-print-string screen x y (subseq str prev-end start) nil)
                  (incf x (string-width str prev-end start)))
            :do (progn
                  (screen-print-string screen x y (subseq str start end) attr)
                  (incf x (string-width str start end)))
            :do (setf prev-end end))
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
  (when (eq window (current-window))
    (let ((buffer (window-buffer window)))
      (when (buffer-mark-p buffer)
        (let ((start (buffer-point buffer))
              (end (buffer-mark buffer)))
          (when (point< end start)
            (rotatef start end))
          (make-overlay start end 'region))))))

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
    (let ((mark-overlay (maybe-make-mark-overlay window)))
      (disp-set-overlays screen
                         (lem::overlays buffer)
                         view-point)
      (when mark-overlay
        (delete-overlay mark-overlay)))))

(defvar *truncate-str/attributes* (cons " " (list '(0 1 lem::truncate-attribute))))

(defun screen-display-line-wrapping (screen screen-width start-x view-charpos
                                            visual-cursor-x visual-cursor-y
                                            point-x point-y str/attributes)
  (when (and (< 0 view-charpos) (= point-y 0))
    (setf str/attributes
          (cons (subseq (car str/attributes) view-charpos)
                (lem-base::subseq-elements (cdr str/attributes)
                                           view-charpos
                                           (length (car str/attributes))))))
  (let ((cursor-line-p nil))
    (when (= point-y visual-cursor-y)
      (setq cursor-line-p t)
      (setf visual-cursor-x (string-width (car str/attributes) 0 point-x)))
    (let ((start 0))
      (loop :for i := (wide-index (car str/attributes)
                                  (1- screen-width)
                                  :start start)
            :while (< point-y (screen-height screen))
            :do (cond ((null i)
                       (disp-print-line screen point-y str/attributes t :string-start start :start-x start-x)
                       (return))
                      (t
                       (cond ((< point-y visual-cursor-y)
                              (incf visual-cursor-y))
                             ((= point-y visual-cursor-y)
                              (let ((len (string-width (car str/attributes) start i)))
                                (when (<= len visual-cursor-x)
                                  (decf visual-cursor-x len)
                                  (incf visual-cursor-y)))))
                       (disp-print-line screen point-y str/attributes t
                                        :string-start start :string-end i
                                        :start-x start-x)
                       (disp-print-line screen point-y
                                        *truncate-str/attributes*
                                        t
                                        :start-x (+ start-x (1- screen-width)))
                       (incf point-y)
                       (setf start i))))
      (values (if cursor-line-p
                  (+ visual-cursor-x start-x)
                  visual-cursor-x)
              visual-cursor-y
              point-y))))

(defun screen-display-line (screen screen-width start-x view-charpos
                                   visual-cursor-x visual-cursor-y
                                   point-x point-y str/attributes)
  (declare (ignore view-charpos))
  (when (= visual-cursor-y point-y)
    (setf visual-cursor-x (string-width (car str/attributes) 0 point-x)))
  (let ((cols screen-width))
    (cond
      ((< (string-width (car str/attributes))
          screen-width)
       (disp-print-line screen point-y str/attributes t :start-x start-x))
      ((or (/= visual-cursor-y point-y)
           (< visual-cursor-x (1- cols)))
       (let ((i (wide-index (car str/attributes) (1- cols))))
         (cond ((<= cols (string-width (car str/attributes) 0 i))
                (disp-print-line screen point-y str/attributes nil :string-end (1- i)
                                 :start-x start-x)
                (disp-print-line screen point-y (cons " $" nil) nil
                                 :start-x (+ start-x (1- i))))
               (t
                (disp-print-line screen point-y str/attributes nil
                                 :string-end i
                                 :start-x start-x)
                (disp-print-line screen point-y (cons "$" nil) nil
                                 :start-x (+ start-x i))))))
      ((< point-x (length (car str/attributes)))
       (let ((start (wide-index (car str/attributes) (- visual-cursor-x cols -3)))
             (end point-x))
         (setf visual-cursor-x (- cols 2))
         (cond ((wide-char-p (char (car str/attributes) end))
                (disp-print-line screen point-y (cons "$" nil) nil :start-x start-x)
                (disp-print-line screen point-y str/attributes nil
                                 :start-x (+ start-x 1) :string-start start :string-end (1- end))
                (disp-print-line screen point-y (cons " $" nil) nil :start-x (+ start-x (1- end)))
                (decf visual-cursor-x))
               (t
                (disp-print-line screen point-y (cons "$" nil) nil :start-x start-x)
                (disp-print-line screen point-y str/attributes nil
                                 :start-x (+ start-x 1)
                                 :string-start start
                                 :string-end end)
                (disp-print-line screen point-y (cons "$" nil) nil
                                 :start-x (+ start-x (1+ end)))))))
      (t
       (let ((start (- visual-cursor-x cols -2)))
         (disp-print-line screen point-y (cons "$" nil) nil :start-x start-x)
         (disp-print-line screen point-y str/attributes t
                          :start-x (+ start-x 1)
                          :string-start (wide-index (car str/attributes) start)))
       (setq visual-cursor-x (- cols 1))))
    (values (if (= visual-cursor-y point-y)
                (+ start-x visual-cursor-x)
                visual-cursor-x)
            visual-cursor-y
            point-y)))

(defun screen-display-lines (screen redraw-flag buffer view-point cursor-point focus-window-p)
  (let* ((truncate-lines (variable-value 'truncate-lines :default buffer))
         (disp-line-function
          (if truncate-lines
              #'screen-display-line-wrapping
              #'screen-display-line))
         (wrap-lines (screen-wrap-lines screen))
         (left-width (screen-left-width screen)))
    (setf (screen-wrap-lines screen) nil)
    (let* ((visual-cursor-x 0)
           (visual-cursor-y (if focus-window-p
                                (count-lines view-point cursor-point)
                                0))
           (cursor-y visual-cursor-y)
           (view-charpos (if focus-window-p (point-charpos view-point) 0))
           (point-x (if focus-window-p
                        (point-charpos cursor-point)
                        0)))
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
                     (when (and (< 0 n) (<= y visual-cursor-y))
                       (incf visual-cursor-y n))
                     (incf y n)
                     (dotimes (_ n)
                       (push i (screen-wrap-lines screen)))))
                  (str/attributes
                   (setf (aref (screen-old-lines screen) i) str/attributes)
                   (when (zerop (length (car str/attributes)))
                     (charms/ll:wmove (screen-%scrwin screen) y 0)
                     (charms/ll:wclrtoeol (screen-%scrwin screen)))
                   (let ((screen-width (- (screen-width screen) left-width))
                         (start-x left-width)
                         y2)
                     (when left-str/attr
                       (screen-print-string screen
                                            0
                                            y
                                            (car left-str/attr)
                                            (cdr left-str/attr)))
                     (let ((*print-start-x* start-x))
                       (multiple-value-setq
                        (visual-cursor-x visual-cursor-y y2)
                        (funcall disp-line-function
                                 screen
                                 screen-width
                                 start-x
                                 view-charpos
                                 visual-cursor-x
                                 visual-cursor-y
                                 point-x
                                 y
                                 str/attributes)))
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
                   (return))))
      (screen-move-cursor screen visual-cursor-x visual-cursor-y))))

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
  (let ((focus-window-p (eq window (current-window))))
    (when focus-window-p (window-see window))
    (lem::window-prompt-display window)
    (progn
      #+(or)without-interrupts
      (disp-reset-lines window)
      (screen-display-lines (lem::window-screen window)
                            (or force
                                (screen-modified-p (lem::window-screen window)))
                            (window-buffer window)
                            (lem::window-view-point window)
                            (lem::window-point window)
                            focus-window-p)
      (when (lem::window-use-modeline-p window)
        (screen-redraw-separator window)
        (screen-redraw-modeline window))
      (charms/ll:wnoutrefresh (screen-%scrwin (lem::window-screen window)))
      (setf (screen-modified-p (lem::window-screen window)) nil))))

(define-implementation update-display ()
  (charms/ll:doupdate))

(define-implementation update-display-size (display-width display-height)
  (let ((delete-windows))
    (dolist (window (window-list))
      (when (<= display-height
                (+ (window-y window) 2))
        (push window delete-windows))
      (when (<= display-width
                (+ (window-x window) 1))
        (push window delete-windows)))
    (mapc #'delete-window delete-windows))
  (let ((window-list (window-list)))
    (dolist (window (lem::collect-right-windows window-list))
      (lem::window-resize window
                          (- display-width
                             *old-display-width*)
                          0))
    (dolist (window (lem::collect-bottom-windows window-list))
      (lem::window-resize window
                          0
                          (- display-height
                             *old-display-height*)))
    (setq *old-display-width* display-width)
    (setq *old-display-height* display-height)
    (charms/ll:mvwin *echo-area-scrwin*
                     (- display-height
                        (minibuffer-window-height))
                     0)
    (charms/ll:wresize *echo-area-scrwin*
                       (minibuffer-window-height)
                       display-width)
    (lem::minibuf-update-size)
    (print-echoarea nil nil)
    (redraw-display)))

(define-implementation print-echoarea (string doupdate-p)
  (charms/ll:werase *echo-area-scrwin*)
  (unless (null string)
    (charms/ll:mvwaddstr *echo-area-scrwin* 0 0 string))
  (if doupdate-p
      (charms/ll:wrefresh *echo-area-scrwin*)
      (charms/ll:wnoutrefresh *echo-area-scrwin*)))

(define-implementation input-loop (editor-thread)
  (loop
    (unless (bt:thread-alive-p editor-thread) (return))
    (let ((code (charms/ll:getch)))
      (cond ((= code -1))
            ((= code 410)
             (loop :while (< 0 (lem::event-queue-length)) :do
               (sleep 0.01))
             (update-display-size (display-width)
                                  (display-height)))
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

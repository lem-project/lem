(in-package :lem-interface)

(defvar *echo-area-scrwin*)

(defvar *old-display-width*)
(defvar *old-display-height*)

(defun %attribute-to-bits (attribute)
  (or (attribute-%internal-value attribute)
      (let ((bits (logior (get-color-pair (attribute-fg-color attribute)
                                          (attribute-bg-color attribute))
                          (if (attribute-reverse-p attribute)
                              charms/ll:a_reverse
                              0)
                          (if (attribute-bold-p attribute)
                              charms/ll:a_bold
                              0)
                          (if (attribute-underline-p attribute)
                              charms/ll:a_underline
                              0))))
        (setf (attribute-%internal-value attribute) bits)
        bits)))

(defun display-init ()
  (term-init)
  (setq *old-display-width* charms/ll:*cols*)
  (setq *old-display-height* charms/ll:*lines*)
  (setf *echo-area-scrwin* (charms/ll:newwin 1 (display-width) (1- (display-height)) 0)))

(defun display-finalize ()
  (term-finalize))

(defun display-width () charms/ll:*cols*)
(defun display-height () charms/ll:*lines*)

(defstruct (screen (:constructor %make-screen))
  %scrwin
  %modeline-scrwin
  x
  y
  lines
  old-lines
  wrap-lines
  width
  modified-p)

(defun make-screen (x y width height use-modeline-p)
  (when use-modeline-p
    (decf height))
  (%make-screen :%scrwin (charms/ll:newwin height width y x)
                :%modeline-scrwin (when use-modeline-p (charms/ll:newwin 1 width (+ y height) x))
                :x x
                :y y
                :width width
                :lines (make-array (max 0 height) :initial-element nil)
                :old-lines (make-array (max 0 height) :initial-element nil)))

(defun screen-delete (screen)
  (charms/ll:delwin (screen-%scrwin screen))
  (charms/ll:delwin (screen-%modeline-scrwin screen)))

(defun screen-clear (screen)
  (screen-modify screen)
  (charms/ll:clearok (screen-%scrwin screen) 1)
  (charms/ll:clearok (screen-%modeline-scrwin screen) 1))

(defun screen-erase (screen)
  (charms/ll:werase (screen-%scrwin screen)))

(defun screen-height (screen)
  (length (screen-lines screen)))

(defun screen-modify (screen)
  (setf (screen-modified-p screen) t))

(defun screen-set-size (screen width height)
  (screen-modify screen)
  (when (screen-%modeline-scrwin screen)
    (decf height))
  (charms/ll:wresize (screen-%scrwin screen)
                     height
                     width)
  (charms/ll:mvwin (screen-%modeline-scrwin screen)
                   (+ (screen-y screen) height)
                   (screen-x screen))
  (charms/ll:wresize (screen-%modeline-scrwin screen)
                     1
                     width)
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
  (charms/ll:mvwin (screen-%scrwin screen) y x)
  (charms/ll:mvwin (screen-%modeline-scrwin screen)
                   (+ y (screen-height screen))
                   x))

(defun scrwin-print-string (scrwin x y string attr)
  (cond ((null attr)
         (setf attr 0))
        ((attribute-p attr)
         (setf attr (%attribute-to-bits attr))))
  (charms/ll:wattron scrwin attr)
  (charms/ll:mvwaddstr scrwin y x string)
  (charms/ll:wattroff scrwin attr))

(defun screen-print-string (screen x y string)
  (scrwin-print-string (screen-%scrwin screen) x y string nil))

(defun screen-print-string-attr (screen x y string attr)
  (scrwin-print-string (screen-%scrwin screen) x y string attr))

(defun screen-move-cursor (screen x y)
  (charms/ll:wmove (screen-%scrwin screen) y x))

(defun set-attr-display-line (screen
                              attr
                              start-linum
                              linum
                              start-charpos
                              end-charpos)
  (let ((i (- linum start-linum)))
    (when (<= 0 i (1- (screen-height screen)))
      (unless end-charpos
        (setq end-charpos (fat-length (aref (screen-lines screen) i))))
      (when (aref (screen-lines screen) i)
        (setf (aref (screen-lines screen) i)
              (copy-fatstring (aref (screen-lines screen) i))))
      (let ((fatstr (aref (screen-lines screen) i)))
        (change-font fatstr
                     (%attribute-to-bits attr)
                     :to
                     start-charpos
                     (min end-charpos (fat-length fatstr)))))))

(defun set-attr-display-lines (screen
                               attr
                               top-linum
                               start-linum
                               start-charpos
                               end-linum
                               end-charpos)
  (set-attr-display-line screen
                         attr
                         top-linum
                         start-linum
                         start-charpos
                         nil)
  (loop :for linum :from (1+ start-linum) :below end-linum :do
    (set-attr-display-line screen
                           attr
                           top-linum
                           linum
                           0
                           nil))
  (set-attr-display-line screen
                         attr
                         top-linum
                         end-linum
                         0
                         end-charpos))

(defun disp-set-overlays (screen overlays start-linum end-linum)
  (loop
    :for overlay :in overlays
    :for start := (overlay-start overlay)
    :for end := (overlay-end overlay)
    :do (cond ((and (= (point-linum start) (point-linum end))
                    (<= start-linum (point-linum start) (1- end-linum)))
               (set-attr-display-line screen
                                      (overlay-attribute overlay)
                                      start-linum
                                      (point-linum start)
                                      (point-charpos start)
                                      (point-charpos end)))
              ((and (<= start-linum (point-linum start))
                    (< (point-linum end) end-linum))
               (set-attr-display-lines screen
                                       (overlay-attribute overlay)
                                       start-linum
                                       (point-linum start)
                                       (point-charpos start)
                                       (point-linum end)
                                       (point-charpos end)))
              ((<= (point-linum start)
                   start-linum
                   (point-linum end)
                   end-linum)
               (set-attr-display-lines screen
                                       (overlay-attribute overlay)
                                       start-linum
                                       start-linum
                                       0
                                       (point-linum end)
                                       (point-charpos end)))
              ((<= start-linum
                   (point-linum start))
               (set-attr-display-lines screen
                                       (overlay-attribute overlay)
                                       start-linum
                                       (point-linum start)
                                       (point-charpos start)
                                       end-linum
                                       nil)))))

(defun disp-line-fatstring (buffer linum)
  (multiple-value-bind (string attributes)
      (buffer-line-string-with-attributes buffer linum)
    (let ((fatstr (make-fatstring string 0)))
      (loop :for (start end value) :in attributes
            :do (when value
                  (change-font fatstr
                               (%attribute-to-bits value)
                               :to
                               start
                               (min (fat-length fatstr) end))))
      fatstr)))

(defun disp-reset-lines (screen buffer start-linum)
  (lem::buffer-update-mark-overlay buffer)
  (let ((end-linum (+ start-linum (screen-height screen)))
        (disp-index 0))
    (loop
      :for linum :from start-linum :to (buffer-nlines buffer)
      :while (< disp-index (screen-height screen)) :do
        (setf (aref (screen-lines screen) disp-index)
              (disp-line-fatstring buffer linum))
        (incf disp-index))
    (loop
      :for i :from disp-index :below (screen-height screen)
      :do (setf (aref (screen-lines screen) i) nil))
    (disp-set-overlays screen
                       (buffer-overlays buffer)
                       start-linum
                       end-linum)))


(defun disp-print-line (screen y str &key (start-x 0) (string-start 0) string-end)
  (let ((x start-x))
    (loop :for i :from string-start :below (or string-end (fat-length str)) :do
      (multiple-value-bind (char attr)
          (fat-char str i)
        (screen-print-string-attr screen x y (string char)
                                  (if (and (lem::ctrl-p char) (char/= char #\tab))
                                      *control-char-attribute*
                                      attr))
        (setq x (char-width char x))))
    (charms/ll:wclrtoeol (screen-%scrwin screen))))

(defun disp-line-wrapping (screen start-charpos curx cury pos-x y str)
  (when (and (< 0 start-charpos) (= y 0))
    (setq str (fat-substring str start-charpos)))
  (when (= y cury)
    (setq curx (string-width (fat-string str) 0 pos-x)))
  (loop :with start := 0 :and width := (screen-width screen)
        :for i := (wide-index (fat-string str) (1- width) :start start)
        :while (< y (screen-height screen))
        :do (cond ((null i)
                   (disp-print-line screen y str :string-start start)
                   (return))
                  (t
                   (cond ((< y cury)
                          (incf cury))
                         ((= y cury)
                          (let ((len (string-width (fat-string str) start i)))
                            (when (<= len curx)
                              (decf curx len)
                              (incf cury)))))
                   (disp-print-line screen y str :string-start start :string-end i)
                   (disp-print-line screen y (load-time-value (make-fatstring "!" 0)) :start-x (1- width))
                   (incf y)
                   (setq start i))))
  (values curx cury y))

(defun disp-line (screen start-charpos curx cury pos-x y str)
  (declare (ignore start-charpos))
  (check-type str fatstring)
  (when (= cury y)
    (setq curx (string-width (fat-string str) 0 pos-x)))
  (let ((width (string-width (fat-string str)))
        (cols (screen-width screen)))
    (cond
      ((< width (screen-width screen))
       nil)
      ((or (/= cury y)
           (< curx (1- cols)))
       (let ((i (wide-index (fat-string str) (1- cols))))
         (setq str
               (if (<= cols (string-width (fat-string str) 0 i))
                   (fat-concat (fat-substring str 0 (1- i)) " $")
                   (fat-concat (fat-substring str 0 i) "$")))))
      ((< pos-x (fat-length str))
       (let* ((start (wide-index (fat-string str) (- curx cols -3)))
              (end pos-x)
              (substr (fat-substring str start end)))
         (setq curx (- cols 2))
         (if (wide-char-p (fat-char substr (- (fat-length substr) 1)))
             (progn
               (setq str
                     (fat-concat "$"
                                 (fat-substring
                                  substr
                                  0 (1- (fat-length substr)))
                                 " $"))
               (decf curx))
             (setq str (fat-concat "$" substr "$")))))
      (t
       (let ((start (- curx cols -2)))
         (setq str
               (fat-concat "$"
                           (fat-substring str
                                          (wide-index (fat-string str) start)))))
       (setq curx (- cols 1))))
    (disp-print-line screen y str))
  (values curx cury y))

(defun screen-display-lines (screen redraw-flag buffer start-charpos start-linum pos-x pos-y)
  (when redraw-flag
    (charms/ll:werase (screen-%scrwin screen)))
  (disp-reset-lines screen buffer start-linum)
  (let ((curx 0)
        (cury (- pos-y start-linum))
        (disp-line-fun
         (if (buffer-truncate-lines buffer)
             #'disp-line-wrapping
             #'disp-line)))
    (let ((wrap-lines (screen-wrap-lines screen))
          (flag redraw-flag))
      (setf (screen-wrap-lines screen) nil)
      (loop
        :with y := 0
        :for i :from 0
        :for str :across (screen-lines screen)
        :while (< y (screen-height screen))
        :do
        (cond ((and (not flag)
                    (aref (screen-old-lines screen) i)
                    str
                    (fat-equalp str (aref (screen-old-lines screen) i))
                    (/= (- pos-y start-linum) i))
               (setf (aref (screen-old-lines screen) i) str)
               (let ((n (count i wrap-lines)))
                 (when (and (< 0 n) (<= y cury))
                   (incf cury n))
                 (incf y (1+ n))
                 (dotimes (_ n)
                   (push i (screen-wrap-lines screen)))))
              (str
               (when (zerop (fat-length str))
                 (charms/ll:wmove (screen-%scrwin screen) y 0)
                 (charms/ll:wclrtoeol (screen-%scrwin screen)))
               (setf (aref (screen-old-lines screen) i) str)
               (let (y2)
                 (multiple-value-setq (curx cury y2)
                                      (funcall disp-line-fun
                                               screen start-charpos curx cury pos-x y str))
                 (let ((offset (- y2 y)))
                   (cond ((< 0 offset)
                          (setq flag t)
                          (dotimes (_ offset)
                            (push i (screen-wrap-lines screen))))
                         ((and (= offset 0) (find i wrap-lines))
                          (setq flag t))))
                 (setf y y2)
                 (incf y)))
              (t
               (fill (screen-old-lines screen) nil :start i)
               (charms/ll:wmove (screen-%scrwin screen) y 0)
               (charms/ll:wclrtobot (screen-%scrwin screen))
               (return)))))
    (screen-move-cursor screen curx cury)))

(defun screen-redraw-separator (window)
  (charms/ll:attron charms/ll:a_reverse)
  (when (< 0 (window-x window))
    (charms/ll:move (window-y window) (1- (window-x window)))
    (charms/ll:vline (char-code #\|) (window-height window)))
  (charms/ll:attroff charms/ll:a_reverse)
  (charms/ll:wnoutrefresh charms/ll:*stdscr*))

(defun screen-redraw-modeline (window)
  (scrwin-print-string (screen-%modeline-scrwin (window-screen window))
                       0
                       0
                       (modeline-string window)
                       (if (eq window (current-window))
                           *modeline-attribute*
                           *modeline-inactive-attribute*))
  (charms/ll:wnoutrefresh (screen-%modeline-scrwin (window-screen window))))

(defun redraw-display-window (window doupdate-p)
  (cond ((minibuffer-window-p window)
         (lem::minibuf-window-update))
        (t
         (window-see window)
         (screen-display-lines (window-screen window)
                               (screen-modified-p (window-screen window))
                               (window-buffer window)
                               (lem::window-view-charpos window)
                               (lem::window-view-linum window)
                               (window-current-charpos window)
                               (window-current-linum window))
         (screen-redraw-separator window)
         (screen-redraw-modeline window)))
  (charms/ll:wnoutrefresh (screen-%scrwin (window-screen window)))
  (setf (screen-modified-p (window-screen window)) nil)
  (when doupdate-p
    (charms/ll:doupdate)))

(defun redraw-display ()
  (dolist (window (window-list))
    (unless (eq window (current-window))
      (redraw-display-window window nil)))
  (redraw-display-window (current-window) nil)
  (charms/ll:doupdate))

(defun update-display-size ()
  (let ((delete-windows))
    (dolist (window (window-list))
      (when (<= (display-height)
                (+ (window-y window) 2))
        (push window delete-windows))
      (when (<= (display-width)
                (+ (window-x window) 1))
        (push window delete-windows)))
    (mapc #'delete-window delete-windows))
  (let ((window-list (window-list)))
    (dolist (window (lem::collect-right-windows window-list))
      (lem::window-resize window
                          (- (display-width)
                             *old-display-width*)
                          0))
    (dolist (window (lem::collect-bottom-windows window-list))
      (lem::window-resize window
                          0
                          (- (display-height)
                             *old-display-height*)))
    (setq *old-display-width* (display-width))
    (setq *old-display-height* (display-height))
    (charms/ll:mvwin *echo-area-scrwin* (1- (display-height)) 0)
    (charms/ll:wresize *echo-area-scrwin* 1 (display-width))
    (lem::minibuf-update-size)
    (redraw-display)))

(defun print-echoarea (string doupdate-p)
  (charms/ll:werase *echo-area-scrwin*)
  (unless (null string)
    (charms/ll:mvwaddstr *echo-area-scrwin* 0 0 string))
  (if doupdate-p
      (charms/ll:wrefresh *echo-area-scrwin*)
      (charms/ll:wnoutrefresh *echo-area-scrwin*)))

(defun get-char-1 ()
  (loop :for code := (charms/ll:getch) :do
    (cond ((= code 410)
           (update-display-size))
          ((= code -1)
           (return nil))
          (t
           (return
             (let ((nbytes (utf8-bytes code)))
               (if (= nbytes 1)
                   (code-char code)
                   (aref (babel:octets-to-string
                          (coerce (cons code
                                        (loop :repeat (1- nbytes)
                                          :collect (charms/ll:getch)))
                                  '(vector (unsigned-byte 8))))
                         0))))))))

(defun get-char (timeout)
  (etypecase timeout
    (integer
     (let ((num 100000))
       (cond ((< num timeout)
              (multiple-value-bind (div mod)
                  (floor timeout num)
                (loop :repeat div :do
                  (multiple-value-bind (char timeout-p)
                      (get-char num)
                    (unless timeout-p
                      (return char))))
                (if (zerop mod)
                    (values #\nul t)
                    (get-char mod))))
             (t
              (charms/ll:timeout timeout)
              (let ((char (get-char-1)))
                (charms/ll:timeout -1)
                (if (null char)
                    (values #\nul t)
                    (values char nil)))))))
    (null
     (loop :for char := (get-char-1) :do
       (unless (null char)
         (return char))))))

(defun call-with-allow-interrupt (flag fn)
  (with-raw (not flag)
    (funcall fn)))

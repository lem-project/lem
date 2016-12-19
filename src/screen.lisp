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
  (setf *echo-area-scrwin*
        (charms/ll:newwin (minibuffer-window-height)
                          (display-width)
                          (- (display-height) (minibuffer-window-height))
                          0)))

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
                :%modeline-scrwin (when use-modeline-p
                                    (charms/ll:newwin 1 width (+ y height) x))
                :x x
                :y y
                :width width
                :lines (make-array (max 0 height) :initial-element nil)
                :old-lines (make-array (max 0 height) :initial-element nil)))

(defun screen-delete (screen)
  (charms/ll:delwin (screen-%scrwin screen))
  (when (screen-%modeline-scrwin screen)
    (charms/ll:delwin (screen-%modeline-scrwin screen))))

(defun screen-clear (screen)
  (screen-modify screen)
  (charms/ll:clearok (screen-%scrwin screen) 1)
  (when (screen-%modeline-scrwin screen)
    (charms/ll:clearok (screen-%modeline-scrwin screen) 1)))

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
  (when (screen-%modeline-scrwin screen)
    (charms/ll:mvwin (screen-%modeline-scrwin screen)
                     (+ (screen-y screen) height)
                     (screen-x screen))
    (charms/ll:wresize (screen-%modeline-scrwin screen)
                       (minibuffer-window-height)
                       width))
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
  (when (screen-%modeline-scrwin screen)
    (charms/ll:mvwin (screen-%modeline-scrwin screen)
                     (+ y (screen-height screen))
                     x)))

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

(defun aref-screen-line-string (screen i)
  (car (aref (screen-lines screen) i)))

(defun aref-screen-line-attributes (screen i)
  (cdr (aref (screen-lines screen) i)))

(defun set-attr-display-line (screen
                              attr
                              start-linum
                              linum
                              start-charpos
                              end-charpos)
  (let ((i (- linum start-linum)))
    (when (<= 0 i (1- (screen-height screen)))
      (unless end-charpos
        (setq end-charpos (length (aref-screen-line-string screen i))))
      (when (aref (screen-lines screen) i)
        (destructuring-bind (string . attributes) (aref (screen-lines screen) i)
          (let ((start start-charpos)
                (end (min end-charpos (length string))))
            (when (< start end)
              (setf (cdr (aref (screen-lines screen) i))
                    (lem::put-elements attributes
                                       start-charpos
                                       (min end-charpos (length string))
                                       attr)))))))))

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
    :for start := (marker-point (overlay-start overlay))
    :for end := (marker-point (overlay-end overlay))
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

(defun disp-reset-lines (screen buffer start-linum)
  (when (eq buffer (current-buffer))
    (lem::buffer-update-mark-overlay buffer))
  (let ((end-linum (+ start-linum (screen-height screen)))
        (disp-index 0))
    (loop
      :for linum :from start-linum :to (buffer-nlines buffer)
      :while (< disp-index (screen-height screen)) :do
      (setf (aref (screen-lines screen) disp-index)
            (multiple-value-bind (string attributes)
                (buffer-line-string-with-attributes buffer linum)
              (cons string attributes)))
      (incf disp-index))
    (loop
      :for i :from disp-index :below (screen-height screen)
      :do (setf (aref (screen-lines screen) i) nil))
    (disp-set-overlays screen
                       (buffer-overlays buffer)
                       start-linum
                       end-linum)))


(defun disp-print-line (screen y str/attributes do-clrtoeol
                               &key (start-x 0) (string-start 0) string-end)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
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
      (setf attributes (lem::subseq-elements attributes string-start string-end)))
    (let ((prev-end 0)
          (x start-x))
      (loop :for (start end attr) :in attributes
            :do (setf end (min (length str) end))
            :do (progn
                  (screen-print-string-attr screen x y (subseq str prev-end start) nil)
                  (incf x (string-width str prev-end start)))
            :do (progn
                  (screen-print-string-attr screen x y (subseq str start end) attr)
                  (incf x (string-width str start end)))
            :do (setf prev-end end))
      (screen-print-string-attr screen x y
                                (if (= prev-end 0)
                                    str
                                    (subseq str prev-end))
                                nil))
    (when do-clrtoeol
      (charms/ll:wclrtoeol (screen-%scrwin screen)))))

(defun disp-line-wrapping (screen start-charpos curx cury pos-x y str/attributes)
  (let ((start (if (and (< 0 start-charpos) (= y 0))
                   start-charpos
                   0)))
    (when (= y cury)
      (setf curx (string-width (car str/attributes) start pos-x)))
    (loop :for i := (wide-index (car str/attributes)
                                (1- (screen-width screen))
                                :start start)
          :while (< y (screen-height screen))
          :do (cond
                ((null i)
                 (disp-print-line screen y str/attributes t :string-start start)
                 (return))
                (t
                 (cond ((< y cury)
                        (incf cury))
                       ((= y cury)
                        (let ((len (string-width (car str/attributes) start i)))
                          (when (<= len curx)
                            (decf curx len)
                            (incf cury)))))
                 (disp-print-line screen y str/attributes t :string-start start :string-end i)
                 (disp-print-line screen y (cons "!" nil) t :start-x (1- (screen-width screen)))
                 (incf y)
                 (setf start i))))
    (values curx cury y)))

(defun disp-line (screen start-charpos curx cury pos-x y str/attributes)
  (declare (ignore start-charpos))
  (when (= cury y)
    (setq curx (string-width (car str/attributes) 0 pos-x)))
  (let ((cols (screen-width screen)))
    (cond
      ((< (string-width (car str/attributes))
          (screen-width screen))
       (disp-print-line screen y str/attributes t))
      ((or (/= cury y)
           (< curx (1- cols)))
       (let ((i (wide-index (car str/attributes) (1- cols))))
         (cond ((<= cols (string-width (car str/attributes) 0 i))
                (disp-print-line screen y str/attributes nil :string-end (1- i))
                (disp-print-line screen y (cons " $" nil) nil :start-x (1- i)))
               (t
                (disp-print-line screen y str/attributes nil :string-end i)
                (disp-print-line screen y (cons "$" nil) nil :start-x i)))))
      ((< pos-x (length (car str/attributes)))
       (let ((start (wide-index (car str/attributes) (- curx cols -3)))
             (end pos-x))
         (setf curx (- cols 2))
         (cond ((wide-char-p (char (car str/attributes) end))
                (disp-print-line screen y (cons "$" nil) nil)
                (disp-print-line screen y str/attributes nil :start-x 1 :string-start start :string-end (1- end))
                (disp-print-line screen y (cons " $" nil) nil :start-x (1- end))
                (decf curx))
               (t
                (disp-print-line screen y (cons "$" nil) nil)
                (disp-print-line screen y str/attributes nil :start-x 1 :string-start start :string-end end)
                (disp-print-line screen y (cons "$" nil) nil :start-x (1+ end))))))
      (t
       (let ((start (- curx cols -2)))
         (disp-print-line screen y (cons "$" nil) nil)
         (disp-print-line screen y str/attributes t :start-x 1 :string-start (wide-index (car str/attributes) start)))
       (setq curx (- cols 1))))
    (values curx cury y)))

(defun screen-display-lines (screen redraw-flag buffer start-charpos start-linum pos-x pos-y)
  ;; (when redraw-flag
  ;;   (charms/ll:werase (screen-%scrwin screen)))
  (disp-reset-lines screen buffer start-linum)
  (let ((curx 0)
        (cury (- pos-y start-linum))
        (disp-line-fun
         (if (buffer-truncate-lines buffer)
             #'disp-line-wrapping
             #'disp-line)))
    (let ((wrap-lines (screen-wrap-lines screen))
          ;; wrap-linesという変数は物理行の単位でどの行が折り返されたかを覚えておくためのもの
          ;; 以前折り返した位置はwrap-linesから探して、今から折り返す位置はscreen-wrap-linesに記録していく
          )
      (setf (screen-wrap-lines screen) nil)
      (loop
        ;; 物理行の単位でループする
        :for y :from 0 ; 論理行
        :for i :from 0 ; 物理行
        :for str/attributes :across (screen-lines screen)
        :while (< y (screen-height screen))
        :do
        (cond ((and ;; 表示回数を減らすための節
                    (buffer-truncate-lines buffer)
                    (not redraw-flag)                     ; 再描画フラグが偽で
                    (not (null str/attributes))           ; その行に表示する行文字列があり
                    #1=(aref (screen-old-lines screen) i) ; 以前にその行に文字列を表示しており
                    (equal str/attributes #1#)            ; 表示しようとしている文字列が以前に表示する行と内容が同じで
                    (/= (- pos-y start-linum) i)          ; その行がカーソルの位置ではないなら真
                    )
               (when (buffer-truncate-lines buffer)
                 ;; 折り返した回数分、論理行の位置を下にずらす
                 (let ((n (count i wrap-lines)))
                   (when (and (< 0 n) (<= y cury))
                     (incf cury n))
                   (incf y n)
                   (dotimes (_ n)
                     (push i (screen-wrap-lines screen))))))
              (str/attributes
               (setf (aref (screen-old-lines screen) i) str/attributes)
               (when (zerop (length (car str/attributes)))
                 ;; 表示する文字列が無い場合は行を表示する関数まで辿りつかないのでここでしておく
                 (charms/ll:wmove (screen-%scrwin screen) y 0)
                 (charms/ll:wclrtoeol (screen-%scrwin screen)))
               (let (y2)
                 (multiple-value-setq (curx cury y2)
                                      (funcall disp-line-fun
                                               screen start-charpos curx cury pos-x y str/attributes))
                 (when (buffer-truncate-lines buffer)
                   (let ((offset (- y2 y))) ; offsetはその行の折り返し回数を表す
                     ;; 折り返しがあったらそれより下は表示をやりなおす必要があるのでredraw-flagをtにする
                     (cond ((< 0 offset)
                            (setq redraw-flag t)
                            (dotimes (_ offset)
                              (push i (screen-wrap-lines screen))))
                           ((and (= offset 0) (find i wrap-lines))
                            (setq redraw-flag t))))
                   (setf y y2))))
              (t
               ;; バッファの末尾まできたときの処理
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
  (window-see window)
  (lem::window-prompt-display window)
  (screen-display-lines (window-screen window)
                        (screen-modified-p (window-screen window))
                        (window-buffer window)
                        (lem::window-view-charpos window)
                        (lem::window-view-linum window)
                        (window-current-charpos window)
                        (window-current-linum window))
  (when (window-use-modeline-p window)
    (screen-redraw-separator window)
    (screen-redraw-modeline window))
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
    (charms/ll:mvwin *echo-area-scrwin*
                     (- (display-height)
                        (minibuffer-window-height))
                     0)
    (charms/ll:wresize *echo-area-scrwin*
                       (minibuffer-window-height)
                       (display-width))
    (lem::minibuf-update-size)
    (print-echoarea nil nil)
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

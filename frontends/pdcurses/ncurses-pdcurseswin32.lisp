(in-package :lem-ncurses)

;; debug log
(defun dbg-log-format (fmt &rest args)
  (with-open-file (out "lemlog_ncurses0001.txt"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :append)
    (fresh-line out)
    (apply #'format out fmt args)
    (terpri out)))

;; for resizing display
(defkeycode "[resize]" #x222)
(defvar *resizing* nil)

;; for mouse
(defkeycode "[mouse]" #x21b)
(defvar *dragging-window* '())

;; use only stdscr
(defmethod lem-if:make-view
    ((implementation ncurses) window x y width height use-modeline)
  (make-ncurses-view
   :scrwin charms/ll:*stdscr*
   :modeline-scrwin (if use-modeline charms/ll:*stdscr* nil)
   :x x
   :y y
   :width width
   :height height))

;; for mouse
(defun mouse-get-window-rect (window)
  (values (lem:window-x      window)
          (lem:window-y      window)
          (lem:window-width  window)
          (lem:window-height window)))
;; for ConEmu
;; get mouse pos-x for adjusting wide characters
(defun get-mouse-pos-x (view x y)
  (unless (= lem.term:*windows-term-type* 2)
    (return-from get-mouse-pos-x x))
  (let ((disp-x 0)
        (pos-x  0)
        (pos-y  (get-pos-y view x y)))
    (loop :while (< pos-x x)
       :for c-code := (get-charcode-from-scrwin view pos-x pos-y)
       :for c := (code-char c-code)
       :do (setf disp-x (lem-base:char-width c disp-x))
         (setf pos-x (+ pos-x (if (< c-code #x10000) 1 2))))
    disp-x))
(defun mouse-move-to-cursor (window x y)
  (lem:move-point (lem:current-point) (lem::window-view-point window))
  (lem:move-to-next-virtual-line (lem:current-point) y)
  (lem:move-to-virtual-line-column (lem:current-point)
                                   (get-mouse-pos-x (lem:window-view window) x y)))
(defun mouse-event-proc (bstate x1 y1)
  (lambda ()
    (cond
      ;; button1 down
      ((logtest bstate (logior charms/ll:BUTTON1_PRESSED
                               charms/ll:BUTTON1_CLICKED
                               charms/ll:BUTTON1_DOUBLE_CLICKED
                               charms/ll:BUTTON1_TRIPLE_CLICKED))
       (let ((press (logtest bstate charms/ll:BUTTON1_PRESSED)))
         (find-if
          (lambda(o)
            (multiple-value-bind (x y w h) (mouse-get-window-rect o)
              (cond
                ;; vertical dragging window
                ((and press (= y1 (- y 1)) (<= x x1 (+ x w -1)))
                 (setf *dragging-window* (list o 'y))
                 t)
                ;; horizontal dragging window
                ((and press (= x1 (- x 1)) (<= y y1 (+ y h -2)))
                 (setf *dragging-window* (list o 'x))
                 t)
                ;; move cursor
                ((and (<= x x1 (+ x w -1)) (<= y y1 (+ y h -2)))
                 (setf (lem:current-window) o)
                 (mouse-move-to-cursor o (- x1 x) (- y1 y))
                 (lem:redraw-display)
                 t)
                (t nil))))
          (lem:window-list))))
      ;; button1 up
      ((logtest bstate charms/ll:BUTTON1_RELEASED)
       (let ((o (first *dragging-window*)))
         (when (windowp o)
           (multiple-value-bind (x y w h) (mouse-get-window-rect o)
             (setf (lem:current-window) o)
             (cond
               ;; vertical dragging window
               ((eq (second *dragging-window*) 'y)
                (let ((vy (- (- (lem:window-y o) 1) y1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (>= y1       3)
                             (>= (+ h vy) 3))
                    (lem:grow-window vy)
                    (lem:redraw-display))))
               ;; horizontal dragging window
               (t
                (let ((vx (- (- (lem:window-x o) 1) x1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (>= x1       5)
                             (>= (+ w vx) 5))
                    (lem:grow-window-horizontally vx)
                    ;; workaround for display update problem (incomplete)
                    (force-refresh-display charms/ll:*cols* (- charms/ll:*lines* 1))
                    (lem:redraw-display))))
               )))
         (when o
           (setf *dragging-window*
                 (list nil (list x1 y1) *dragging-window*)))))
      ;; wheel up
      ((logtest bstate charms/ll:BUTTON4_PRESSED)
       (lem:scroll-up 3)
       (lem:redraw-display))
      ;; wheel down
      ((logtest bstate charms/ll:BUTTON5_PRESSED)
       (lem:scroll-down 3)
       (lem:redraw-display))
      )))

;; deal with utf-16 surrogate pair characters
(defun get-key (code)
  (when (<= #xd800 code #xdbff)
    (charms/ll:timeout 100)
    (let ((c-lead  code)
          (c-trail (charms/ll:getch)))
      (when (<= #xdc00 c-trail #xdfff)
        (setf code (+ #x10000 (* (- c-lead #xd800) #x0400) (- c-trail #xdc00))))
      (charms/ll:timeout -1)))
  (char-to-key (code-char code)))

;; enable modifier keys
(let ((resize-code (get-code "[resize]"))
      (mouse-code  (get-code "[mouse]"))
      (abort-code  (get-code "C-]"))
      (escape-code (get-code "escape"))
      (ctrl-key nil)
      (alt-key  nil)
      (esc-key  nil))
  (defun get-ch ()
    (charms/ll:PDC-save-key-modifiers 1)
    (let ((code          (charms/ll:getch))
          (modifier-keys (charms/ll:PDC-get-key-modifiers)))
      (setf ctrl-key (logtest modifier-keys charms/ll:PDC_KEY_MODIFIER_CONTROL))
      (setf alt-key  (logtest modifier-keys charms/ll:PDC_KEY_MODIFIER_ALT))
      (cond
        ;; ctrl key workaround
        (ctrl-key
         (cond
           ;; C-space / C-@
           ((= code #x040) (setf code 0)) ; for mintty
           ((= code #x020) (setf code 0)) ; for ConEmu
           ;; C-down / C-up / C-left / C-right
           ((= code #x1e1) (setf code 525))
           ((= code #x1e0) (setf code 566))
           ((= code #x1bb) (setf code 545))
           ((= code #x1bc) (setf code 560))
           ;; C-Home / C-End / C-PageUp / C-PageDown
           ((= code #x1bf) (setf alt-key t) (setf code #x03c)) ; M-<
           ((= code #x1c0) (setf alt-key t) (setf code #x03e)) ; M->
           ((= code #x1bd) (setf code #o523)) ; PageUp
           ((= code #x1be) (setf code #o522)) ; PageDown
           ;; C-|
           ((= code #x07c) (setf code 31)) ; C-_ (Redo) for ConEmu
           ))
        ;; alt key workaround
        (alt-key
         (cond
           ;; M-kanji
           ((= code #x000) (setf code -1)) ; for cmd.exe
           ;; M-0 - M-9
           ((<= #x197 code #x1a0) (setf code (- code #x167)))
           ;; M-A - M-Z
           ((<= #x1a1 code #x1ba) (setf code (- code #x140)))
           ;; M-down / M-up / M-left / M-right
           ((= code #x1eb) (setf code #o402))
           ((= code #x1ea) (setf code #o403))
           ((= code #x1ed) (setf code #o404))
           ((= code #x1ec) (setf code #o405))
           ;; M-[
           ((= code #x1f1)
            (charms/ll:timeout 100)
            (let ((code1 (charms/ll:getch)))
              (cond
		;; drop mouse escape sequence
		((= code1 #x03c)	; <
                 (loop :for code2 := (charms/ll:getch)
                    :until (or (= code2 -1)
                               (= code2 #x04d)	   ; M
                               (= code2 #x06d)))   ; m
                 (setf code -1)))
              (charms/ll:timeout -1)))
           ))
        ;; normal key workaround
        (t
         (cond
           ;; Home / End / PageUp / PageDown
           ;;((= code #x106) (setf code #o406)) ; same code
           ((= code #x166) (setf code #o550))
           ;;((= code #x153) (setf code #o523)) ; same code
           ;;((= code #x152) (setf code #o522)) ; same code
           ;; C-|
           ((= code #x09c) (setf code 31)) ; C-_ (Redo) for mintty
           )))
      code))
  (defun get-event ()
    (tagbody :start
       (return-from get-event
         (let ((code (get-ch)))
           (cond ((= code -1) (go :start))
                 ((= code resize-code)
                  ;;(setf esc-key nil)
                  ;; for resizing display
                  (setf *resizing* t)
                  :resize)
                 ((= code mouse-code)
                  ;;(setf esc-key nil)
                  ;; for mouse
                  (multiple-value-bind (bstate x y z id)
                      (charms/ll:getmouse)
                    (mouse-event-proc bstate x y)))
                 ((= code abort-code)
                  (setf esc-key nil)
                  :abort)
                 ((= code escape-code)
                  (setf esc-key t)
                  (get-key-from-name "escape"))
                 ((or alt-key esc-key)
                  (setf esc-key nil)
                  (let ((key (get-key code)))
                    (make-key :meta t
                              :sym (key-sym key)
                              :ctrl (key-ctrl key))))
                 (t
                  (setf esc-key nil)
                  (get-key code))))))))

;; workaround for exit problem
(defun input-loop (editor-thread)
  (handler-case
      (loop
         (handler-case
             (progn
               (unless (bt:thread-alive-p editor-thread) (return))
               (let ((event (get-event)))
                 (if (eq event :abort)
                     (send-abort-event editor-thread nil)
                     (send-event event)))
               ;; workaround for exit problem
               ;; workaround for display update problem (incomplete)
               (sleep 0.0001))
           #+sbcl
           (sb-sys:interactive-interrupt (c)
             (declare (ignore c))
             (send-abort-event editor-thread t))))
    (exit-editor (c) (return-from input-loop c))))

;; workaround for exit problem
(defmethod lem-if:invoke ((implementation ncurses) function)
  (let ((result nil)
        (input-thread (bt:current-thread)))
    (unwind-protect
         (progn
           (when (lem.term:term-init)
             (let ((editor-thread
                    (funcall function
                             nil
                             (lambda (report)
                               (bt:interrupt-thread
                                input-thread
                                (lambda () (error 'exit-editor :value report)))))))
               (setf result (input-loop editor-thread))
               ;; workaround for exit problem
               ;; (to avoid 'compilation unit aborted caught 1 fatal ERROR condition')
               (bt:join-thread editor-thread)
               )))
      (lem.term:term-finalize))
    (when (and (typep result 'exit-editor)
               (exit-editor-value result))
      (format t "~&~A~%" (exit-editor-value result)))))

;; workaround for display update problem (incomplete)
(defun force-refresh-display (width height)
  (loop :for y1 :from 0 :below height
     ;; '#\.' is necessary ('#\space' doesn't work)
     :with str := (make-string width :initial-element #\.)
     :do (charms/ll:mvwaddstr charms/ll:*stdscr* y1 0 str))
  (charms/ll:refresh)
  (sleep 0.1))

;; for resizing display
(defun resize-display ()
  (when *resizing*
    (setf *resizing* nil)
    (charms/ll:resizeterm 0 0)
    (charms/ll:erase)
    ;; workaround for display update problem (incomplete)
    (force-refresh-display charms/ll:*cols* charms/ll:*lines*)))

;; for resizing display
(defmethod lem-if:display-width ((implementation ncurses))
  (resize-display)
  (max 5 charms/ll:*cols*))

;; for resizing display
(defmethod lem-if:display-height ((implementation ncurses))
  (resize-display)
  (max 3 (- charms/ll:*lines*
            ;; for cmd.exe (windows ime uses the last line)
            (if (= lem.term:*windows-term-type* 3) 1 0))))

;; use only stdscr
(defmethod lem-if:delete-view ((implementation ncurses) view)
  ;; nop
  )

;; use only stdscr
(defmethod lem-if:clear ((implementation ncurses) view)
  (loop :for y1 :from 0 :below (+ (ncurses-view-height view)
                                  (if (ncurses-view-modeline-scrwin view) 1 0))
     :with str := (make-string (ncurses-view-width view) :initial-element #\space)
     :do (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                              (get-pos-y view 0 y1)
                              (get-pos-x view 0 y1)
                              str)))

;; use only stdscr
(defmethod lem-if:set-view-size ((implementation ncurses) view width height)
  (setf (ncurses-view-width view) width)
  (setf (ncurses-view-height view) height))

;; use only stdscr
(defmethod lem-if:set-view-pos ((implementation ncurses) view x y)
  (setf (ncurses-view-x view) x)
  (setf (ncurses-view-y view) y))

;; deal with utf-16 surrogate pair characters
(defun get-charcode-from-scrwin (view x y)
  (let ((code (logand (charms/ll:mvwinch (ncurses-view-scrwin view) y x)
                      charms/ll:A_CHARTEXT)))
    (when (<= #xd800 code #xdbff)
      (let ((c-lead  code)
            (c-trail (logand (charms/ll:mvwinch (ncurses-view-scrwin view) y (+ x 1))
                             charms/ll:A_CHARTEXT)))
        (when (<= #xdc00 c-trail #xdfff)
          (setf code (+ #x10000 (* (- c-lead #xd800) #x0400) (- c-trail #xdc00))))))
    code))

;; for mintty and ConEmu
;; get pos-x/y for printing wide characters
(defun get-pos-x (view x y &key (modeline nil) (cursor nil))
  (unless (or (and (= lem.term:*windows-term-type* 1) (not cursor))
              (= lem.term:*windows-term-type* 2))
    (return-from get-pos-x (+ x (ncurses-view-x view))))
  (let* ((floating (not (ncurses-view-modeline-scrwin view)))
         (start-x  (ncurses-view-x view))
         (disp-x0  (+ x start-x))
         (disp-x   (if floating 0 start-x))
         (pos-x    (if floating 0 start-x))
         (pos-y    (get-pos-y view x y :modeline modeline)))
    (loop :while (< disp-x disp-x0)
       :for c-code := (get-charcode-from-scrwin view pos-x pos-y)
       :for c := (code-char c-code)
       :do (setf disp-x (lem-base:char-width c disp-x))
         ;; pos-x is incremented only 1 at wide characters
         ;; (except for utf-16 surrogate pair characters)
         (setf pos-x (+ pos-x (if (< c-code #x10000) 1 2))))
    pos-x))
(defun get-pos-y (view x y &key (modeline nil))
  (+ y (ncurses-view-y view) (if modeline (ncurses-view-height view) 0)))

;; for mintty and ConEmu
;; adjust line width by using zero-width-space characters (#\u200b)
(defun adjust-line (view x y &key (modeline nil))
  (unless (or (= lem.term:*windows-term-type* 1)
              (= lem.term:*windows-term-type* 2))
    (return-from adjust-line))
  (let* ((start-x    (ncurses-view-x view))
         (disp-width (ncurses-view-width view))
         (disp-x0    (+ disp-width start-x))
         (pos-x      (get-pos-x view disp-width y :modeline modeline))
         (pos-y      (get-pos-y view disp-width y :modeline modeline)))
    ;; write zero-width-space characters (#\u200b)
    ;; only when horizontal splitted window
    ;; (to reduce the possibility of copying zero-width-space characters to clipboard)
    (when (and (> disp-x0 pos-x)
               (< (+ start-x disp-width) charms/ll:*cols*))
      (charms/ll:mvwaddstr (ncurses-view-scrwin view) pos-y pos-x
                           (make-string (- disp-x0 pos-x)
                                        :initial-element #\u200b)))))

;; workaround for display problem of utf-16 surrogate pair characters (incomplete)
(defun remake-string (string)
  (let ((clist    '())
        (splitted nil))
    (loop :for c :across string
       :for c-code := (char-code c)
       :do (if (< c-code #x10000)
               (push c clist)
               ;; split to 2 characters
               (multiple-value-bind (q r) (floor (- c-code #x10000) #x0400)
                 (push (code-char (+ q #xd800)) clist) ; leading surrogate
                 (push (code-char (+ r #xdc00)) clist) ; trailing surrogate
                 (setf splitted t))))
    (if splitted
        (concatenate 'string (reverse clist))
        string)))

;; clip string to fit inside of view
(defun clip-string (view x y string)
  (let ((disp-width (ncurses-view-width view)))
    (cond
      ((>= x disp-width)
       "")
      ((> (+ x (length string)) disp-width)
       (subseq string 0 (- disp-width x)))
      (t
       string))))

;; for cmd.exe (using cjk code page)
;; workaround for printing problem of wide characters (incomplete)
(defun print-sub (scrwin x y string)
  (unless (and (= lem.term:*windows-term-type* 3)
               (member lem.term:*windows-code-page* '(932 936 949 950)))
    (charms/ll:mvwaddstr scrwin y x string)
    (return-from print-sub))
  ;; clear display area
  (let ((disp-width 0))
    (loop :for c :across string
       :for c-code := (char-code c)
       :do (incf disp-width)
         ;; check wide characters (except for cp932 halfwidth katakana)
         (when (and (> c-code #x7f)
                    (not (and (= lem.term:*windows-code-page* 932)
                              (<= #xff61 c-code #xff9f))))
           (incf disp-width)))
    (charms/ll:mvwaddstr scrwin y x
                         (make-string disp-width :initial-element #\.))
    (charms/ll:refresh))
  ;; display wide characters
  (loop :for c :across string
     :for c-code := (char-code c)
     :with pos-x := x
     :do (charms/ll:mvwaddch scrwin y pos-x c-code)
       (incf pos-x)
       ;; check wide characters (except for cp932 halfwidth katakana)
       (when (and (> c-code #x7f)
                  (not (and (= lem.term:*windows-code-page* 932)
                            (<= #xff61 c-code #xff9f))))
         (charms/ll:mvwaddch scrwin y pos-x c-code)
         (incf pos-x)
         (charms/ll:refresh))))

;; use get-pos-x/y and adjust-line
(defmethod lem-if:print ((implementation ncurses) view x y string attribute)
  (let ((attr (attribute-to-bits attribute)))
    (charms/ll:wattron (ncurses-view-scrwin view) attr)
    ;;(charms/ll:scrollok (ncurses-view-scrwin view) 0)
    (print-sub (ncurses-view-scrwin view)
               (get-pos-x view x y)
               (get-pos-y view x y)
               (clip-string view x y (remake-string string)))
    ;;(charms/ll:scrollok (ncurses-view-scrwin view) 1)
    (charms/ll:wattroff (ncurses-view-scrwin view) attr)
    (adjust-line view x y)))

;; use get-pos-x/y and adjust-line
(defmethod lem-if:print-modeline ((implementation ncurses) view x y string attribute)
  (let ((attr (attribute-to-bits attribute)))
    (charms/ll:wattron (ncurses-view-modeline-scrwin view) attr)
    (print-sub (ncurses-view-modeline-scrwin view)
               (get-pos-x view x y :modeline t)
               (get-pos-y view x y :modeline t)
               (clip-string view x y (remake-string string)))
    (charms/ll:wattroff (ncurses-view-modeline-scrwin view) attr)
    (adjust-line view x y :modeline t)))

;; use get-pos-x/y and adjust-line
(defmethod lem-if:clear-eol ((implementation ncurses) view x y)
  (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                       (get-pos-y view x y)
                       (get-pos-x view x y)
                       (make-string (max (- (ncurses-view-width view)
                                            (- (get-pos-x view x y)
                                               (ncurses-view-x view)))
                                         0)
                                    :initial-element #\space))
  (adjust-line view x y))

;; use get-pos-x/y and adjust-line
(defmethod lem-if:clear-eob ((implementation ncurses) view x y)
  (lem-if:clear-eol implementation view x y)
  (loop :for y1 :from (+ y 1) :below (ncurses-view-height view)
     :with str := (make-string (ncurses-view-width view) :initial-element #\space)
     :do (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                              (get-pos-y view 0 y1)
                              (get-pos-x view 0 y1)
                              str)))

;; use only stdscr
(defmethod lem-if:redraw-view-after ((implementation ncurses) view focus-window-p)
  (let ((attr (attribute-to-bits 'modeline)))
    (charms/ll:attron attr)
    (when (and (ncurses-view-modeline-scrwin view)
               (< 0 (ncurses-view-x view)))
      ;; vertical line
      (loop :for y1 :from 0 :below (+ (ncurses-view-height view) 1)
         :do (charms/ll:mvwaddch (ncurses-view-scrwin view)
                                 (+ (ncurses-view-y view) y1)
                                 (- (ncurses-view-x view) 1)
                                 (char-code #\space))))
    (charms/ll:attroff attr))
  (charms/ll:wnoutrefresh (ncurses-view-scrwin view)))

;; adjust cursor position
(defmethod lem-if:update-display ((implementation ncurses))
  (let* ((view   (window-view (current-window)))
         (scrwin (ncurses-view-scrwin view)))
    (if (lem::covered-with-floating-window-p (current-window) lem::*cursor-x* lem::*cursor-y*)
        (charms/ll:curs-set 0)
        (progn
          (charms/ll:curs-set 1)
          (charms/ll:wmove scrwin
                           (get-pos-y view lem::*cursor-x* lem::*cursor-y*)
                           (get-pos-x view lem::*cursor-x* lem::*cursor-y* :cursor t))))
    (charms/ll:wnoutrefresh scrwin)
    (charms/ll:doupdate)))

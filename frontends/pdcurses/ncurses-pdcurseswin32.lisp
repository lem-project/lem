(in-package :lem-ncurses)

;; windows terminal type
;;   :mintty  : mintty  (winpty is needed)
;;   :conemu  : ConEmu  (experimental)
;;               ('chcp 65001' must be done before run)
;;   :cmd.exe : cmd.exe (experimental)
(defvar *windows-term-type*
  (cond
    ((string= (uiop:getenv "MSYSCON") "mintty.exe") :mintty)
    ((uiop:getenv "ConEmuBuild") :conemu)
    (t :cmd.exe)))
(defun windows-term-type ()
  *windows-term-type*)
(defun (setf windows-term-type) (v)
  (setf *windows-term-type* v)
  (setf *windows-term-setting*
        (case v
          (:mintty
           (make-windows-term-setting
            :disp-char-width t
            :pos-char-width  t
            :cur-char-width  t
            :cur-mov-by-pos  nil
            :reserved-last-lines 0))
          (:conemu
           (make-windows-term-setting
            :disp-char-width t
            :pos-char-width  t
            :cur-char-width  nil
            :cur-mov-by-pos  t
            :reserved-last-lines 0))
          (t
           (make-windows-term-setting
            :disp-char-width nil
            :pos-char-width  nil
            :cur-char-width  nil
            :cur-mov-by-pos  nil
            ;; reserve last line for windows ime
            :reserved-last-lines 1))))
  v)

;; windows terminal setting
(defvar *windows-term-setting*)
(defstruct windows-term-setting
  disp-char-width ;; character width setting for display
                  ;;   t        : use default function
                  ;;   nil      : no conversion
                  ;;   function : use custom function
                  ;;                (lambda (code) width)
  pos-char-width  ;; character width setting for position
                  ;;   t        : use default function
                  ;;   nil      : no conversion
                  ;;   function : use custom function
                  ;;                (lambda (code) width)
  cur-char-width  ;; character width setting for cursor movement
                  ;;   t        : use default function
                  ;;   nil      : no conversion
                  ;;   function : use custom function
                  ;;                (lambda (code) width)
  cur-mov-by-pos  ;; cursor movement setting
                  ;;   t   : cursor movement is specified by pos-char-width
                  ;;   nil : cursor movement is specified by cur-char-width
  (reserved-last-lines 0 :type fixnum)
                  ;; reserve last lines for windows ime and so on
                  ;;   fixnum : number of reserved lines
  )
(defmethod calc-disp-char-width ((wt windows-term-setting) code)
  (let ((fn (windows-term-setting-disp-char-width wt)))
    (if (functionp fn)
        (funcall fn code)
        ;; check zero-width-space character (#\u200b)
        (if (= code #x200b)
            0
            (if (lem-base:wide-char-p (code-char code)) 2 1)))))
(defmethod calc-pos-char-width ((wt windows-term-setting) code)
  (let ((fn (windows-term-setting-pos-char-width wt)))
    (if (functionp fn)
        (funcall fn code)
        ;; check utf-16 surrogate pair characters
        (if (< code #x10000) 1 2))))
(defmethod calc-cur-char-width ((wt windows-term-setting) code)
  (let ((fn (windows-term-setting-cur-char-width wt)))
    (if (functionp fn)
        (funcall fn code)
        ;; check zero-width-space character (#\u200b)
        (if (= code #x200b)
            0
            (if (lem-base:wide-char-p (code-char code)) 2 1)))))
(defmethod (setf reserved-last-lines) (v (wt windows-term-setting))
  (setf (windows-term-setting-reserved-last-lines wt) v)
  (setf *resizing* t)
  (lem::change-display-size-hook)
  v)
(setf (windows-term-type) *windows-term-type*)

;; load windows dll
;;  (we load winmm.dll with a full path because it isn't listed in
;;   windows knowndlls)
(cffi:load-foreign-library '(:default "kernel32"))
(let* ((csize  1024)
       (cbuf   (cffi:foreign-alloc :char :count csize :initial-element 0))
       (sysdir ""))
  (if (zerop (cffi:foreign-funcall "GetSystemDirectoryA"
                                   :pointer cbuf :int csize :int))
    (error "winmm.dll load error (GetSystemDirectoryA failed)")
    (setf sysdir (concatenate 'string (cffi:foreign-string-to-lisp cbuf) "\\")))
  (cffi:load-foreign-library (concatenate 'string sysdir "winmm.dll"))
  (cffi:foreign-free cbuf))

;; windows console code page
(defvar *windows-code-page*
  (cffi:foreign-funcall "GetConsoleOutputCP" :int))

;; input polling interval (sec)
;;  (we don't use PDCurses's internal polling timer (0.05 sec interval))
;;  (we use high precision timer when this interval is less than 0.01)
(defvar *input-polling-interval* 0.001)
(defun input-polling-interval ()
  *input-polling-interval*)
(let ((high-precision nil)
      (exit-hook      nil))
  (defun (setf input-polling-interval) (v)
    (setf *input-polling-interval* v)
    (cond
      ((< v 0.01)
       (unless high-precision
         (setf high-precision t)
         (cffi:foreign-funcall "timeBeginPeriod" :int 1 :int))
       (unless exit-hook
         (setf exit-hook t)
         (add-hook *exit-editor-hook*
                   (lambda ()
                     (when high-precision
                       (cffi:foreign-funcall "timeEndPeriod" :int 1 :int))))))
      (t
       (when high-precision
         (setf high-precision nil)
         (cffi:foreign-funcall "timeEndPeriod" :int 1 :int))))
    v)
  (setf (input-polling-interval) *input-polling-interval*))

;; for input
;;  (we don't use stdscr for input because it calls wrefresh implicitly
;;   and causes the display confliction by two threads)
(defvar *padwin* nil)
(defun getch-pad ()
  (unless *padwin*
    (setf *padwin* (charms/ll:newpad 1 1))
    (charms/ll:keypad *padwin* 1)
    (charms/ll:PDC-save-key-modifiers 1)
    ;; timeout setting is necessary to exit lem normally
    (charms/ll:wtimeout *padwin* 0))
  (charms/ll:wgetch *padwin*))

;; for resizing display
(defkeycode "[resize]" #x222)
(defvar *resizing* nil)
(defvar *min-cols*  5)
(defvar *min-lines* 3)

;; for mouse
(defkeycode "[mouse]" #x21b)
(defvar *dragging-window* '())
(defvar *wheel-scroll-size* 3)

;; debug log
(defun dbg-log-format (fmt &rest args)
  (with-open-file (out "lemlog_pdcurses0001.txt"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :append)
    (fresh-line out)
    (apply #'format out fmt args)
    (terpri out)))

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

;; mouse function
(defun mouse-get-window-rect (window)
  (values (lem:window-x      window)
          (lem:window-y      window)
          (lem:window-width  window)
          (lem:window-height window)))
;; for mintty and ConEmu
;; get mouse disp-x for pointing wide characters properly
(defun mouse-get-disp-x (view x y)
  (cond
    ((and (not (windows-term-setting-cur-mov-by-pos *windows-term-setting*))
          (windows-term-setting-disp-char-width *windows-term-setting*)
          (windows-term-setting-pos-char-width  *windows-term-setting*)
          (windows-term-setting-cur-char-width  *windows-term-setting*))
     ;; for mintty
     (let ((disp-x 0)
           (pos-x  0)
           (pos-y  y)
           (cur-x  0))
       (loop :while (< cur-x x)
          :for code := (get-charcode-from-scrwin view pos-x pos-y)
          :until (= code charms/ll:ERR)
          :do (incf disp-x (calc-disp-char-width *windows-term-setting* code))
              (incf pos-x  (calc-pos-char-width  *windows-term-setting* code))
              (incf cur-x  (calc-cur-char-width  *windows-term-setting* code)))
       disp-x))
    ((and (windows-term-setting-cur-mov-by-pos  *windows-term-setting*)
          (windows-term-setting-disp-char-width *windows-term-setting*)
          (windows-term-setting-pos-char-width  *windows-term-setting*))
     ;; for ConEmu
     (let ((disp-x 0)
           (pos-x  0)
           (pos-y  y))
       (loop :while (< pos-x x)
          :for code := (get-charcode-from-scrwin view pos-x pos-y)
          :until (= code charms/ll:ERR)
          :do (incf disp-x (calc-disp-char-width *windows-term-setting* code))
              (incf pos-x  (calc-pos-char-width  *windows-term-setting* code)))
       disp-x))
    (t x)))
;; for ConEmu
;; get mouse cur-x for horizontal dragging window
(defun mouse-get-cur-x (view x y)
  (cond
    ((and (windows-term-setting-cur-mov-by-pos *windows-term-setting*)
          (windows-term-setting-pos-char-width *windows-term-setting*)
          (windows-term-setting-cur-char-width *windows-term-setting*))
     (let ((pos-x 0)
           (pos-y y)
           (cur-x 0))
       (loop :while (< pos-x x)
          :for code := (get-charcode-from-scrwin view pos-x pos-y)
          :until (= code charms/ll:ERR)
          :do (incf pos-x (calc-pos-char-width *windows-term-setting* code))
              (incf cur-x (calc-cur-char-width *windows-term-setting* code)))
       cur-x))
    ((and (windows-term-setting-cur-mov-by-pos  *windows-term-setting*)
          (windows-term-setting-disp-char-width *windows-term-setting*)
          (windows-term-setting-pos-char-width  *windows-term-setting*))
     (mouse-get-disp-x view x y))
    (t x)))
(defun mouse-move-to-cursor (window x y)
  (lem:move-point (lem:current-point) (lem::window-view-point window))
  (lem:move-to-next-virtual-line (lem:current-point) y)
  (lem:move-to-virtual-line-column (lem:current-point) x))
(defun mouse-event-proc (bstate x1 y1)
  (lambda ()
    ;; workaround for cursor position problem
    (let ((disp-x (mouse-get-disp-x (lem:window-view (lem:current-window)) x1 y1))
          (cur-x  (mouse-get-cur-x  (lem:window-view (lem:current-window)) x1 y1))
          (no-floating-window (if lem::*floating-windows* nil t)))
      ;; process mouse event
      (cond
        ;; button1 down
        ((and no-floating-window
              (logtest bstate (logior charms/ll:BUTTON1_PRESSED
                                      charms/ll:BUTTON1_CLICKED
                                      charms/ll:BUTTON1_DOUBLE_CLICKED
                                      charms/ll:BUTTON1_TRIPLE_CLICKED)))
         (let ((press (logtest bstate charms/ll:BUTTON1_PRESSED)))
           (find-if
            (lambda(o)
              (multiple-value-bind (x y w h) (mouse-get-window-rect o)
                (cond
                  ;; vertical dragging window
                  ((and press (= y1 (- y 1)) (<= x disp-x (+ x w -1)))
                   (setf *dragging-window* (list o 'y))
                   t)
                  ;; horizontal dragging window
                  ((and press (= disp-x (- x 1)) (<= y y1 (+ y h -2)))
                   (setf *dragging-window* (list o 'x))
                   t)
                  ;; move cursor
                  ((and (<= x disp-x (+ x w -1)) (<= y y1 (+ y h -2)))
                   (setf (lem:current-window) o)
                   (mouse-move-to-cursor o (- disp-x x) (- y1 y))
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
                    (when (and no-floating-window
                               (>= y1       *min-lines*)
                               (>= (+ h vy) *min-lines*))
                      (lem:grow-window vy)
                      (lem:redraw-display))))
                 ;; horizontal dragging window
                 (t
                  (let ((vx (- (- (lem:window-x o) 1) cur-x)))
                    ;; this check is incomplete if 3 or more divisions exist
                    (when (and no-floating-window
                               (>= cur-x    *min-cols*)
                               (>= (+ w vx) *min-cols*))
                      (lem:grow-window-horizontally vx)
                      (lem:redraw-display))))
                 )))
           (when o
             (setf *dragging-window*
                   (list nil (list cur-x y1) *dragging-window*)))))
        ;; wheel up
        ((logtest bstate charms/ll:BUTTON4_PRESSED)
         (lem:scroll-up *wheel-scroll-size*)
         (lem:redraw-display))
        ;; wheel down
        ((logtest bstate charms/ll:BUTTON5_PRESSED)
         (lem:scroll-down *wheel-scroll-size*)
         (lem:redraw-display))
        ))))

;; deal with utf-16 surrogate pair characters (input)
(defun get-key (code)
  (when (<= #xd800 code #xdbff)
    (charms/ll:wtimeout *padwin* 100)
    (let ((c-lead  code)
          (c-trail (getch-pad)))
      (when (<= #xdc00 c-trail #xdfff)
        (setf code (+ #x10000 (* (- c-lead #xd800) #x0400) (- c-trail #xdc00))))
      (charms/ll:wtimeout *padwin* 0)))
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
    (let ((code          (getch-pad))
          (modifier-keys (charms/ll:PDC-get-key-modifiers)))
      (setf ctrl-key (logtest modifier-keys charms/ll:PDC_KEY_MODIFIER_CONTROL))
      (setf alt-key  (logtest modifier-keys charms/ll:PDC_KEY_MODIFIER_ALT))
      ;;(unless (= code -1)
      ;;  (dbg-log-format "code=~X ctrl=~S alt=~S" code ctrl-key alt-key))
      (cond
        ;; ctrl key workaround
        (ctrl-key
         (cond
           ;; C-space / C-@
           ((= code #x040) (setf code 0)) ; for mintty
           ((= code #x020) (setf code 0)) ; for ConEmu
           ;; C-j
           ((= code #x211) (setf code 10))
           ;; C-Delete
           ((= code #x20f) (setf code 519))
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
           ;; M-Delete / M-Backspace
           ((= code #x1de) (setf code #o512))
           ((= code #x1f8) (setf code #x07f))
           ;; M-down / M-up / M-left / M-right
           ((= code #x1eb) (setf code #o402))
           ((= code #x1ea) (setf code #o403))
           ((= code #x1ed) (setf code #o404))
           ((= code #x1ec) (setf code #o405))
           ;; M-Home / M-End / M-PageUp / M-PageDown
           ((= code #x1e6) (setf code #x03c)) ; M-<
           ((= code #x1e9) (setf code #x03e)) ; M->
           ((= code #x1e7) (setf code #o523))
           ((= code #x1e8) (setf code #o522))
           ;; M-( / M-) / M-^ / M-\
           ((= code #x02A) (setf code #x028))
           ((= code #x028) (setf code #x029))
           ((= code #x1f4) (setf code #x05e))
           ((= code #x210) (setf code #x05c))
           ;; M-[
           ((= code #x1f1)
            (charms/ll:wtimeout *padwin* 100)
            (let ((code1 (getch-pad)))
              (cond
		;; drop mouse escape sequence
		((= code1 #x03c)	; <
                 (loop :for code2 := (getch-pad)
                    :until (or (= code2 -1)
                               (= code2 #x04d)	   ; M
                               (= code2 #x06d)))   ; m
                 (setf code -1)))
              (charms/ll:wtimeout *padwin* 0)))
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
    (let ((code (get-ch)))
      (cond ((= code -1)
             ;; retry is necessary to exit lem normally
             :retry)
            ((= code resize-code)
             ;; for resizing display
             (setf *resizing* t)
             :resize)
            ((= code mouse-code)
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
             (get-key code))))))

;; workaround for exit problem
(defun input-loop (editor-thread)
  (handler-case
      (loop
         (handler-case
             (progn
               (unless (bt:thread-alive-p editor-thread) (return))
               (let ((event (get-event)))
                 (case event
                   ;; retry is necessary to exit lem normally
                   (:retry
                    (sleep *input-polling-interval*))
                   (:abort
                    (send-abort-event editor-thread nil))
                   (t
                    (send-event event)))))
           #+sbcl
           (sb-sys:interactive-interrupt (c)
             (declare (ignore c))
             (send-abort-event editor-thread t))))
    (exit-editor (c) (return-from input-loop c))))

;; workaround for display update problem (incomplete)
(let ((temp-view   nil)
      (toggle-flag t))
  (defun write-something-to-last-line ()
    (when (eq *windows-term-type* :cmd.exe)
      (return-from write-something-to-last-line))
    (unless temp-view
      (setf temp-view (lem-if:make-view *implementation* nil 0 0 0 0 nil)))
    ;; this recovers winpty's screen corruption
    (let ((x (- charms/ll:*cols*  2))
          (y (- charms/ll:*lines* 1)))
      (print-sub (ncurses-view-scrwin temp-view)
                 (get-pos-x temp-view x y)
                 (get-pos-y temp-view x y)
                 (if toggle-flag "+" "-"))
      (setf toggle-flag (if toggle-flag nil t)))))

;; workaround for display update problem (incomplete)
(defun force-refresh-display (width height)
  (loop :for y1 :from 0 :below height
     ;; clear display area to reset PDCurses's internal cache memory
     ;; ('#\.' is necessary ('#\space' doesn't work))
     :with str := (make-string width :initial-element #\.)
     :do (charms/ll:mvwaddstr charms/ll:*stdscr* y1 0 str))
  (charms/ll:refresh)
  (sleep 0.1)
  ;; clear reserved last lines
  (let ((last-lines (windows-term-setting-reserved-last-lines
                     *windows-term-setting*)))
    (when (and (> last-lines 0) (> height *min-lines*))
      (loop :for y1 :from (max (- height last-lines) *min-lines*) :below height
         :with str := (make-string width :initial-element #\space)
         :do (charms/ll:mvwaddstr charms/ll:*stdscr* y1 0 str)))))

;; for resizing display
(defun resize-display ()
  (when *resizing*
    (setf *resizing* nil)
    ;; wait to get window size certainly
    (sleep 0.1)
    ;; check resize error
    (when (= (charms/ll:resizeterm 0 0) charms/ll:ERR)
      ;; this is needed to clear PDCurses's inner event flag
      (charms/ll:resizeterm (max *min-lines* charms/ll:*lines*)
                            (max *min-cols*  charms/ll:*cols*)))
    (charms/ll:erase)
    ;; workaround for display update problem (incomplete)
    (force-refresh-display charms/ll:*cols* charms/ll:*lines*)))

;; for resizing display
(defmethod lem-if:display-width ((implementation ncurses))
  (resize-display)
  (max *min-cols* charms/ll:*cols*))

;; for resizing display
(defmethod lem-if:display-height ((implementation ncurses))
  (resize-display)
  ;; reserve last lines for windows ime and so on
  (max *min-lines*
       (- charms/ll:*lines*
          (windows-term-setting-reserved-last-lines
           *windows-term-setting*))))

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

;; deal with utf-16 surrogate pair characters (screen memory)
(defun get-charcode-from-scrwin (view x y)
  (let ((code (charms/ll:mvwinch (ncurses-view-scrwin view) y x)))
    (unless (= code charms/ll:ERR)
      (setf code (logand code charms/ll:A_CHARTEXT))
      (when (<= #xd800 code #xdbff)
        (let ((c-lead  code)
              (c-trail (logand (charms/ll:mvwinch (ncurses-view-scrwin view) y (+ x 1))
                               charms/ll:A_CHARTEXT)))
          (when (<= #xdc00 c-trail #xdfff)
            (setf code (+ #x10000 (* (- c-lead #xd800) #x0400) (- c-trail #xdc00)))))))
    code))

;; for mintty and ConEmu
;; get pos-x/y for printing wide characters
(defun get-pos-x (view x y &key (modeline nil))
  (unless (and (windows-term-setting-disp-char-width *windows-term-setting*)
               (windows-term-setting-pos-char-width  *windows-term-setting*))
    (return-from get-pos-x (+ x (ncurses-view-x view))))
  (let* ((floating (not (ncurses-view-modeline-scrwin view)))
         (start-x  (ncurses-view-x view))
         (disp-x0  (+ x start-x))
         (disp-x   (if floating 0 start-x))
         (pos-x    (if floating 0 start-x))
         (pos-y    (get-pos-y view x y :modeline modeline)))
    (loop :while (< disp-x disp-x0)
       :for code := (get-charcode-from-scrwin view pos-x pos-y)
       :until (= code charms/ll:ERR)
       :do (incf disp-x (calc-disp-char-width *windows-term-setting* code))
           (incf pos-x  (calc-pos-char-width  *windows-term-setting* code)))
    pos-x))
(defun get-pos-y (view x y &key (modeline nil))
  (+ y (ncurses-view-y view) (if modeline (ncurses-view-height view) 0)))

;; for mintty
;; get cur-x for moving cursor position properly
(defun get-cur-x (view x y &key (modeline nil))
  (unless (and (not (windows-term-setting-cur-mov-by-pos *windows-term-setting*))
               (windows-term-setting-disp-char-width *windows-term-setting*)
               (windows-term-setting-disp-char-width *windows-term-setting*)
               (windows-term-setting-cur-char-width  *windows-term-setting*))
    (return-from get-cur-x (get-pos-x view x y :modeline modeline)))
  (let* ((start-x (ncurses-view-x view))
         (disp-x0 (+ x start-x))
         (disp-x  0)
         (pos-x   0)
         (pos-y   (get-pos-y view x y :modeline modeline))
         (cur-x   0))
    (loop :while (< disp-x disp-x0)
       :for code := (get-charcode-from-scrwin view pos-x pos-y)
       :until (= code charms/ll:ERR)
       :do (incf disp-x (calc-disp-char-width *windows-term-setting* code))
           (incf pos-x  (calc-pos-char-width  *windows-term-setting* code))
           (incf cur-x  (calc-cur-char-width  *windows-term-setting* code)))
    cur-x))

;; for mintty and ConEmu
;; adjust line width by using zero-width-space character (#\u200b)
(defun adjust-line (view x y &key (modeline nil))
  (unless (and (windows-term-setting-disp-char-width *windows-term-setting*)
               (windows-term-setting-pos-char-width  *windows-term-setting*))
    (return-from adjust-line))
  (let* ((start-x    (ncurses-view-x view))
         (disp-width (ncurses-view-width view))
         (disp-x0    (+ disp-width start-x))
         (pos-x      (get-pos-x view disp-width y :modeline modeline))
         (pos-y      (get-pos-y view disp-width y :modeline modeline)))
    ;; write string of zero-width-space character (#\u200b)
    ;; only when horizontal splitted window
    ;; (to reduce the possibility of copying zero-width-space characters to clipboard)
    (when (and (> disp-x0 pos-x)
               (< (+ start-x disp-width) charms/ll:*cols*))
      (charms/ll:mvwaddstr (ncurses-view-scrwin view) pos-y pos-x
                           (make-string (- disp-x0 pos-x)
                                        :initial-element #\u200b)))))

;; deal with utf-16 surrogate pair characters (output)
(defun remake-string (string)
  (let ((clist    '())
        (splitted nil))
    (loop :for c :across string
       :for code := (char-code c)
       :do (if (< code #x10000)
               (push c clist)
               ;; split to 2 characters
               (multiple-value-bind (q r) (floor (- code #x10000) #x0400)
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
  (unless (and (eq *windows-term-type* :cmd.exe)
               (member *windows-code-page* '(932 936 949 950)))
    (charms/ll:mvwaddstr scrwin y x string)
    (return-from print-sub))
  ;; clear display area to reset PDCurses's internal cache memory
  ;; ('#\.' is necessary ('#\space' doesn't work))
  (let ((disp-width 0))
    (loop :for c :across string
       :for code := (char-code c)
       :do (incf disp-width)
         ;; check wide characters (except for cp932 halfwidth katakana)
         (when (and (> code #x7f)
                    (not (and (= *windows-code-page* 932)
                              (<= #xff61 code #xff9f))))
           (incf disp-width)))
    (charms/ll:mvwaddstr scrwin y x
                         (make-string disp-width :initial-element #\.))
    (charms/ll:refresh))
  ;; display wide characters
  (loop :for c :across string
     :for code := (char-code c)
     :with pos-x := x
     :do (charms/ll:mvwaddch scrwin y pos-x code)
       (incf pos-x)
       ;; check wide characters (except for cp932 halfwidth katakana)
       (when (and (> code #x7f)
                  (not (and (= *windows-code-page* 932)
                            (<= #xff61 code #xff9f))))
         (charms/ll:mvwaddch scrwin y pos-x code)
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
  (when (and (ncurses-view-modeline-scrwin view)
             (< 0 (ncurses-view-x view)))
    (let ((attr (attribute-to-bits 'modeline)))
      (charms/ll:attron attr)
      ;; vertical line for horizontal splitted window
      (loop :for y1 :from 0 :below (+ (ncurses-view-height view) 1)
         :do (charms/ll:mvaddch (+ (ncurses-view-y view) y1)
                                (- (ncurses-view-x view) 1)
                                (char-code #\space)))
      (charms/ll:attroff attr))))

;; use get-pos-x/y
(defmethod lem-if:update-display ((implementation ncurses))
  (let* ((view   (window-view (current-window)))
         (scrwin (ncurses-view-scrwin view)))
    ;; workaround for display update problem (incomplete)
    (write-something-to-last-line)
    ;; set cursor position
    (if (lem::covered-with-floating-window-p (current-window) lem::*cursor-x* lem::*cursor-y*)
        (charms/ll:curs-set 0)
        (progn
          (charms/ll:curs-set 1)
          (charms/ll:wmove scrwin
                           (get-pos-y view lem::*cursor-x* lem::*cursor-y*)
                           ;; workaround for cursor position problem
                           ;;(get-pos-x view lem::*cursor-x* lem::*cursor-y*)
                           (get-cur-x view lem::*cursor-x* lem::*cursor-y*))))
    (charms/ll:wnoutrefresh scrwin)
    (charms/ll:doupdate)))

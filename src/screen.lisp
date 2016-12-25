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

(defun disp-set-line (screen attribute screen-row start-charpos end-charpos)
  (when (and (<= 0 screen-row)
             (< screen-row (screen-height screen))
             (not (null (aref (screen-lines screen) screen-row)))
             (or (null end-charpos)
                 (< start-charpos end-charpos)))
    (destructuring-bind (string . attributes)
        (aref (screen-lines screen) screen-row)
      (setf (cdr (aref (screen-lines screen) screen-row))
            (lem::put-elements attributes
                               start-charpos
                               (or end-charpos
                                   (length string))
                               attribute)))))

(defun disp-set-overlay (screen attribute view-point start end)
  (let ((screen-row (1- (lem::count-lines view-point start))))
    (disp-set-line screen attribute screen-row (point-charpos start) nil)
    (lem::with-point ((point start))
      (lem::line-offset point 1)
      (loop :for i :from (1+ screen-row)
	 :do
	 (when (lem::same-line-p point end)
	   (disp-set-line screen attribute i 0 (point-charpos end))
	   (return))
	 (disp-set-line screen attribute i 0 nil)
	 (unless (lem::line-offset point 1)
	   (return-from disp-set-overlay))))))

(defun disp-set-overlays (screen overlays view-point)
  (let ((view-end-point
         (lem::with-point ((view-point view-point))
           (or (lem::line-offset view-point (screen-height screen))
               (lem::buffer-end view-point)))))
    (loop :for overlay :in overlays
       :for start := (overlay-start overlay)
       :for end := (overlay-end overlay)
       :do (cond
	     ((and (lem::same-line-p start end)
		   (point<= view-point start)
		   (point< start view-end-point))
	      (disp-set-line screen
			     (overlay-attribute overlay)
			     (1- (lem::count-lines view-point start))
			     (point-charpos start)
			     (point-charpos end)))
	     ((and (point<= view-point start)
		   (point< end view-end-point))
	      (disp-set-overlay screen
				(overlay-attribute overlay)
				view-point
				start
				end))
	     ((and (point<= start view-point)
		   (point<= view-point end)
		   (point<= end view-end-point))
	      (disp-set-overlay screen
				(overlay-attribute overlay)
				view-point
				view-point
				end))
	     ((point<= view-point start)
	      (disp-set-overlay screen
				(overlay-attribute overlay)
				view-point
				start
				view-end-point))))))

(defun disp-reset-lines (screen buffer view-point)
  (when (eq buffer (current-buffer))
    (lem::buffer-update-mark-overlay buffer))
  (lem::with-point ((point view-point))
    (loop :for i :from 0 :below (screen-height screen)
       :do
       (let ((line (lem::get-line/point point)))
	 (setf (aref (screen-lines screen) i)
	       (lem::line-string/attributes line)))
       (unless (lem::line-offset point 1)
	 (fill (screen-lines screen) nil :start (1+ i))
	 (return))))
  (disp-set-overlays screen
                     (buffer-overlays buffer)
                     view-point))

(defun screen-display-line-wrapping (screen view-charpos
				     visual-cursor-x visual-cursor-y
				     point-x point-y str/attributes)
  (when (and (< 0 view-charpos) (= point-y 0))
    (setf str/attributes
          (cons (subseq (car str/attributes) view-charpos)
                (lem::subseq-elements (cdr str/attributes)
                                      view-charpos
                                      (length (car str/attributes))))))
  (when (= point-y visual-cursor-y)
    (setf visual-cursor-x (string-width (car str/attributes) 0 point-x)))
  (let ((start 0))
    (loop :for i := (wide-index (car str/attributes)
                                (1- (screen-width screen))
                                :start start)
       :while (< point-y (screen-height screen))
       :do (cond ((null i)
		  (disp-print-line screen point-y str/attributes t :string-start start)
		  (return))
		 (t
		  (cond ((< point-y visual-cursor-y)
			 (incf visual-cursor-y))
			((= point-y visual-cursor-y)
			 (let ((len (string-width (car str/attributes) start i)))
			   (when (<= len visual-cursor-x)
			     (decf visual-cursor-x len)
			     (incf visual-cursor-y)))))
		  (disp-print-line screen point-y str/attributes t :string-start start :string-end i)
		  (disp-print-line screen point-y (cons "!" nil) t :start-x (1- (screen-width screen)))
		  (incf point-y)
		  (setf start i))))
    (values visual-cursor-x visual-cursor-y point-y)))

(defun screen-display-line (screen view-charpos
			    visual-cursor-x visual-cursor-y
			    point-x point-y str/attributes)
  (declare (ignore view-charpos))
  (when (= visual-cursor-y point-y)
    (setf visual-cursor-x (string-width (car str/attributes) 0 point-x)))
  (let ((cols (screen-width screen)))
    (cond
      ((< (string-width (car str/attributes))
          (screen-width screen))
       (disp-print-line screen point-y str/attributes t))
      ((or (/= visual-cursor-y point-y)
           (< visual-cursor-x (1- cols)))
       (let ((i (wide-index (car str/attributes) (1- cols))))
         (cond ((<= cols (string-width (car str/attributes) 0 i))
                (disp-print-line screen point-y str/attributes nil :string-end (1- i))
                (disp-print-line screen point-y (cons " $" nil) nil :start-x (1- i)))
               (t
                (disp-print-line screen point-y str/attributes nil :string-end i)
                (disp-print-line screen point-y (cons "$" nil) nil :start-x i)))))
      ((< point-x (length (car str/attributes)))
       (let ((start (wide-index (car str/attributes) (- visual-cursor-x cols -3)))
             (end point-x))
         (setf visual-cursor-x (- cols 2))
         (cond ((wide-char-p (char (car str/attributes) end))
                (disp-print-line screen point-y (cons "$" nil) nil)
                (disp-print-line screen point-y str/attributes nil
                                 :start-x 1 :string-start start :string-end (1- end))
                (disp-print-line screen point-y (cons " $" nil) nil :start-x (1- end))
                (decf visual-cursor-x))
               (t
                (disp-print-line screen point-y (cons "$" nil) nil)
                (disp-print-line screen point-y str/attributes nil :start-x 1 :string-start start :string-end end)
                (disp-print-line screen point-y (cons "$" nil) nil :start-x (1+ end))))))
      (t
       (let ((start (- visual-cursor-x cols -2)))
         (disp-print-line screen point-y (cons "$" nil) nil)
         (disp-print-line screen point-y str/attributes t
                          :start-x 1 :string-start (wide-index (car str/attributes) start)))
       (setq visual-cursor-x (- cols 1))))
    (values visual-cursor-x visual-cursor-y point-y)))

(defun screen-display-lines (screen redraw-flag buffer view-point cursor-point)
  (let ((disp-line-function
         (if (buffer-truncate-lines buffer)
             #'screen-display-line-wrapping
             #'screen-display-line))
        (wrap-lines (screen-wrap-lines screen)))
    (setf (screen-wrap-lines screen) nil)
    (let* ((visual-cursor-x 0)
           (visual-cursor-y (1- (lem::count-lines view-point cursor-point)))
           (cursor-y visual-cursor-y)
           (view-charpos (point-charpos view-point))
           (point-x (point-charpos cursor-point)))
      (loop :for y :from 0
	 :for i :from 0
	 :for str/attributes :across (screen-lines screen)
	 :while (< y (screen-height screen))
	 :do (cond
	       ((and (buffer-truncate-lines buffer)
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
		(let (y2)
		  (multiple-value-setq (visual-cursor-x visual-cursor-y y2)
		    (funcall disp-line-function
			     screen
			     view-charpos
			     visual-cursor-x
			     visual-cursor-y
			     point-x
			     y
			     str/attributes))
		  (when (buffer-truncate-lines buffer)
		    (let ((offset (- y2 y)))
		      (cond ((< 0 offset)
			     (setf redraw-flag t)
			     (dotimes (_ offset)
			       (push i (screen-wrap-lines screen))))
			    ((and (= offset 0) (find i wrap-lines))
			     (setf redraw-flag t))))
		    (setf y y2))))
	       (t
		(fill (screen-old-lines screen) nil :start i)
		(charms/ll:wmove (screen-%scrwin screen) y 0)
		(charms/ll:wclrtobot (screen-%scrwin screen))
		(return))))
      (screen-move-cursor screen visual-cursor-x visual-cursor-y))))

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
  (disp-reset-lines (window-screen window)
                    (window-buffer window)
                    (lem::window-view-point window))
  (screen-display-lines (window-screen window)
                        (screen-modified-p (window-screen window))
                        (window-buffer window)
                        (lem::window-view-point window)
                        (lem::window-point window))
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

(in-package :ncurses-clone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter A_BOLD
  #b100000000 ;;8 bits for char, could be 7?
  ;;#x00200000
  )
(defparameter A_UNDERLINE
  #b1000000000
  ;;#x00020000
  )
(defparameter A_REVERSE
  #b10000000000
  )

;;https://invisible-island.net/ncurses/ncurses-intro.html#stdscr
#+nil "The other is to set the current-highlight value. This is logical-or'ed with any highlight you specify the first way. You do this with the functions attron(), attroff(), and attrset(); see the manual pages for details. Color is a special kind of highlight. The package actually thinks in terms of color pairs, combinations of foreground and background colors. The sample code above sets up eight color pairs, all of the guaranteed-available colors on black. Note that each color pair is, in effect, given the name of its foreground color. Any other range of eight non-conflicting values could have been used as the first arguments of the init_pair() values."
(defparameter *current-attributes* 0)
(defun ncurses-attron (n)
  (setf *current-attributes*
	(logior *current-attributes* n)))
(defun ncurses-attroff (n)
  (setf *current-attributes*
	(logand *current-attributes* (lognot n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Glyphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;glyphs stored as either a struct, a class, or an integer

#+nil
(struct-to-clos:struct->class
 (defstruct glyph
   value
   attributes))

(deftype glyph () `(unsigned-byte ,(+ 8 8 3)))
(deftype glyph-attributes () `(unsigned-byte ,(+ 8 3)))
(defun glyph-value (glyph)
  (declare (type glyph glyph))
  (declare (optimize (speed 3)
		     (safety 0)))
  (code-char (logand glyph (utility::etouq (1- (ash 1 8))))))
(defun glyph-attributes (glyph)
  (declare (type glyph glyph))
  (declare (optimize (speed 3)
		     (safety 0)))
  (ash glyph -8))

(defun prepare-attributes-for-glyph (attributes)
  (declare (type glyph-attributes attributes))
  (declare (optimize (speed 3)
		     (safety 0)))
  (ash attributes 8))

(defun gen-glyph (value attributes)
  (declare (optimize (speed 3)
		     (safety 0)))
  (declare (type glyph-attributes attributes))
  #+nil
  (make-glyph :value value
	      :attributes attributes)
  (logior (char-code value)
	  (ash attributes 8)))
#+nil
(progn
  (defun print-glyph (stream glyph)
    (write-char (glyph-value glyph) stream))
  (set-pprint-dispatch 'glyph 'print-glyph))

(defparameter *clear-glyph* (gen-glyph #\Space 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;The virtual window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *columns* 80)
(defparameter *lines* 25)

(defun make-virtual-window ()
  (let ((array (make-array *lines*)))
    (dotimes (i (length array))
      (setf (aref array i)
	    (make-array *columns*
			:initial-element *clear-glyph*)))
    array))
(defparameter *virtual-window* (make-virtual-window))
(defparameter *virtual-window-lock* (bt:make-recursive-lock))
(defun set-virtual-window (x y value)
  (setf (aref (aref *virtual-window* y) x)
	value))

(defmacro with-virtual-window-lock (&body body)
  `(bt:with-recursive-lock-held (*virtual-window-lock*)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Color Pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fg-default-really* 15)
(defparameter *bg-default-really* 0)

(defparameter *fg-default* *fg-default-really*)
(defparameter *bg-default* *bg-default-really*)

(defparameter *pairs* (let ((pairs (make-hash-table)))
			(setf (gethash 0 pairs)
			      (cons *fg-default-really*
				    *bg-default-really*)
 ;;;;FIXME whats white and black for default? short?
			      )
			pairs))

(defun ncurses-init-pair (pair-counter fg bg)
  (setf (gethash pair-counter *pairs*)
	(cons fg bg)))
(defun ncurses-color-pair (pair-counter)
  (gethash pair-counter *pairs*)) ;;fixme -> this is not how ncurses works.

(defun ncurses-pair-content (pair-counter)
  (let ((pair (ncurses-color-pair pair-counter)))
    (values (car pair)
	    (cdr pair))))

(defun ncurses-assume-default-color (fg bg)
  ;;;;how ncurses works. see https://users-cs.au.dk/sortie/sortix/release/nightly/man/man3/assume_default_colors.3.html
  (setf *fg-default* (if (= fg -1)
			 *fg-default*
			 fg)
	*bg-default* (if (= bg -1)
			 *bg-default*
			 bg))
  (ncurses-init-pair 0 *fg-default* *bg-default*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Window and window operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct-to-clos:struct->class
 (defstruct win
   lines
   COLS
   y
   x
   keypad-p ;;see https://linux.die.net/man/3/keypad
   clearok
   scrollok
   attr-bits
   cursor-y
   cursor-x
   data))

(set-pprint-dispatch 'win 'print-win)
(defun print-win (stream win)
  (format stream "lines: ~a cols: ~a" (win-lines win) (win-cols win))
  (print-grid (win-data win) stream (win-cursor-x win) (win-cursor-y win)))

;;window is an array of lines, for easy swapping and scrolling of lines. optimizations later
(defun make-row (width)
  (make-array width :initial-element *clear-glyph*))
(defun make-grid (rows columns)
  (let ((rows-array (make-array rows)))
    (dotimes (i rows)
      (setf (aref rows-array i)
	    (make-row columns)))
    rows-array))

(defun grid-rows (grid)
  (length grid))
(defun grid-columns (grid)
  (length (aref grid 0)))
(utility::etouq
  (let ((place '(aref (aref grid y) x))
	(args '(x y grid)))
    `(progn
       (defun ref-grid (,@args)
	 ,place)
       (defun (setf ref-grid) (new ,@args)
	 (setf ,place new)
	 new))))

(defun print-grid (grid &optional (stream *standard-output*) (cursor-x 0) (cursor-y 0))
  (dotimes (grid-row (grid-rows grid))
    (terpri stream)
    (write-char #\| stream)
    (let ((row-data (aref grid grid-row)))	
      (dotimes (grid-column (grid-columns grid)) ;;FIXME dereferencing redundancy
	(let ((cursor-here-p (and (= grid-column cursor-x)
				  (= grid-row cursor-y)))
	      (x (aref row-data grid-column)))
	  (when cursor-here-p (write-char #\[ stream))
	  (write-char 
	   (typecase x
	     (glyph (glyph-value x))
	     (t #\space))
	   stream)
	  (when cursor-here-p (write-char #\] stream)))))
    (write-char #\| stream))
  (terpri stream)
  grid)

(defun move-row (old-n new-n grid)
  "move row old-n to new-n"
  (cond ((> (grid-rows grid) new-n -1)
	 (setf (aref grid new-n)
	       (aref grid old-n))
	 (setf (aref grid old-n) nil))
	(t (error "moving to a row that does not exist")))
  grid)

(defun transfer-data (grid-src grid-dest)
  (let ((shared-rows
	 (min (grid-rows grid-src)
	      (grid-rows grid-dest)))
	(shared-columns
	 (min (grid-columns grid-src)
	      (grid-columns grid-dest))))
    (dotimes (row-index shared-rows)
      ;;FIXME optimization? can cache the row. but its a fragile optimization
      (dotimes (column-index shared-columns)
	(setf (ref-grid column-index row-index grid-dest)
	      (ref-grid column-index row-index grid-src)))))
  grid-dest)

(defparameter *win* nil)

(defun ncurses-newwin (nlines ncols begin-y begin-x)
  (let ((win (make-win :lines nlines
		       :cols ncols
		       :y begin-y
		       :x begin-x
		       :cursor-x 0
		       :cursor-y 0
		       :data (make-grid nlines ncols)
		       :attr-bits 0)))
  ;;  (add-win win)
    (setf *win* win)
    win))
(defun ncurses-delwin (win)
  (declare (ignorable win))
  ;;(remove-win win)
  )

(defparameter *std-scr* (ncurses-newwin 25 80 0 0))

;;(defun ncurses-wscrl (win n))
;;https://linux.die.net/man/3/scrollok
(defun ncurses-wmove (win y x)
  (setf (win-cursor-x win) x
	(win-cursor-y win) y))
(defun ncurses-move (y x)
  (ncurses-wmove *std-scr* y x))

;;https://www.mkssoftware.com/docs/man3/curs_border.3.asp
(defun ncurses-wvline (win char n)
  (let ((y (win-cursor-y win))
	(x (win-cursor-x win)))
    (loop :for i :from y :below (min (+ y n)
				     (win-lines win))
       :do (add-char x i char))))
(defun ncurses-vline (char n)
  (ncurses-wvline *std-scr* char n))

(defun ncurses-keypad (win value)
  (setf (win-keypad-p win) value))
(defun c-true (value)
  (not (zerop value)))

(defun ncurses-clearok (win value)
  "If clearok is called with TRUE as argument, the next call to wrefresh with this window will clear the screen completely and redraw the entire screen from scratch. This is useful when the contents of the screen are uncertain, or in some cases for a more pleasing visual effect. If the win argument to clearok is the global variable curscr, the next call to wrefresh with any window causes the screen to be cleared and repainted from scratch. "
  (setf (win-clearok win)
	(c-true value)))

;;;FIXME add default window for ncurses like stdscr

(defun ncurses-mvwin (win y x)
  "Calling mvwin moves the window so that the upper left-hand corner is at position (x, y). If the move would cause the window to be off the screen, it is an error and the window is not moved. Moving subwindows is allowed, but should be avoided."
  ;;;FIXME: detect off screen 
  (setf (win-x win) x
	(win-y win) y))

(defun ncurses-wresize (win height width)
  (setf (win-lines win) height
	(win-cols win) width)
  (let ((old-data (win-data win))
	(new-grid (make-grid height width)))
    (transfer-data old-data new-grid)
    (setf (win-data win)
	  new-grid)))

(defparameter *mouse-enabled-p* nil)

(defun ncurses-wattron (win attr)
  (let ((old (win-attr-bits win)))
    (setf (win-attr-bits win)
	  (logior attr old))))

(defun ncurses-wattroff (win attr)
  (let ((old (win-attr-bits win)))
    (setf (win-attr-bits win)
	  (logand (lognot attr) old))))

(defun %ncurses-wscrl (grid n)
  (let ((width (grid-columns grid)))
    (cond ((plusp n)
	   ;;scrolling up means lines get moved up,
	   ;;which means start at top of screen to move, which is smallest.
	   (loop :for i :from n :below (grid-rows grid)
	      :do
	      (move-row i
			(- i n)
			grid)))
	  ((minusp n)
	   ;;scrolling down means lines get moved down,
	   ;;which means start at bottom of screen to move, which is largest.
	   (let ((move-distance (- n)))
	     (loop :for i :from (- (grid-rows grid) 1 move-distance) :downto 0
		:do
		(move-row i
			  (+ i move-distance)
			  grid))))
	  ((zerop n) t))
    ;;;;fill in those nil's. OR FIXME
    (map-into grid (lambda (x) (or x (make-row width))) grid))
  grid)
(defun ncurses-wscrl (win n)
  (%ncurses-wscrl (win-data win) n))

(defun ncurses-mvwaddstr (win y x string)
  (setf (win-cursor-x win) x
	(win-cursor-y win) y)
  (ncurses-waddstr win string))
(defun ncurses-waddstr (win string)
  (dotimes (index (length string))
    (ncurses-waddch win (aref string index))))
(defun ncurses-wclrtoeol (&optional (win *win*))
  "The clrtoeol() and wclrtoeol() routines erase the current line to the right of the cursor, inclusive, to the end of the current line. https://www.mkssoftware.com/docs/man3/curs_clear.3.asp"
  (let ((x (win-cursor-x win))
	(y (win-cursor-y win)))
    (loop :for i :from x :below (win-cols win)
       :do (add-char i y #\Space win)))
  win)
(defun ncurses-clrtoeol ()
  (let ((win *win*))
    (ncurses-wclrtoeol win)
    win))
(defun ncurses-wclrtobot (&optional (win *win*))
  "The clrtobot() and wclrtobot() routines erase from the cursor to the end of screen. That is, they erase all lines below the cursor in the window. Also, the current line to the right of the cursor, inclusive, is erased. https://www.mkssoftware.com/docs/man3/curs_clear.3.asp"
  (ncurses-wclrtoeol win)
  (let ((y (win-cursor-y win)))
    (loop :for i :from (+ y 1) :below (win-lines win)
       :do
       (loop :for z :from 0 :below (win-cols win)
	  :do (add-char z i #\Space win))))
  win)

(defun max-cursor-y (&optional (win *win*))
  "the greatest value a cursor's y pos can be"
  (1- (win-lines win)))
(defun max-cursor-x (&optional (win *win*))
  "the greatest value a cursor's x pos can be"
  (1- (win-cols win)))

(defun ncurses-waddch (win char)
  " The addch(), waddch(), mvaddch() and mvwaddch() routines put the character ch into the given window at its current window position, which is then advanced. They are analogous to putchar() in stdio(). If the advance is at the right margin, the cursor automatically wraps to the beginning of the next line. At the bottom of the current scrolling region, if scrollok() is enabled, the scrolling region is scrolled up one line.

If ch is a tab, newline, or backspace, the cursor is moved appropriately within the window. Backspace moves the cursor one character left; at the left edge of a window it does nothing. Newline does a clrtoeol(), then moves the cursor to the window left margin on the next line, scrolling the window if on the last line). Tabs are considered to be at every eighth column. https://www.mkssoftware.com/docs/man3/curs_addch.3.asp If ch is any control character other than tab, newline, or backspace, it is drawn in ^X notation. Calling winch() after adding a control character does not return the character itself, but instead returns the ^-representation of the control character. (To emit control characters literally, use echochar().) "
  (let ((x (win-cursor-x win))
	(y (win-cursor-y win)))
    (flet ((advance ()	     
	     (if (= (max-cursor-x win) x)
		 (if (= (max-cursor-y win) y)
		     (cond ((win-scrollok win) ;;scroll the window and reset to x pos
			    (ncurses-wscrl win 1)
			    (setf (win-cursor-x win) 0))
			   (t (progn ;;do nothing
				)))
		     ;;reset x and go to next line, theres space
		     (setf (win-cursor-x win) 0
			   (win-cursor-y win) (+ 1 y)))
		 ;;its not at the end of line, no one cares
		 (setf (win-cursor-x win) (+ 1 x)))))
      (cond 
	((char= char #\tab)
	 (setf (win-cursor-x win)
	       (next-8 x)))
	((char= char #\newline)
	 (ncurses-clrtoeol)
	 (let ((max-cursor-y (max-cursor-y win)))
	   (if (= max-cursor-y y)
	       (ncurses-wscrl win 1)
	       (setf (win-cursor-y win)
		     (min (+ 1 y)
			  max-cursor-y))))
	 (setf (win-cursor-x win) 0))
	((char= char #\backspace)
	 (setf (win-cursor-x win)
	       (max 0 (- x 1))))
	((char-control char)
	 (add-char x y #\^ win)
	 (advance)
	 (ncurses-waddch win (char-control-printable char)))
	((standard-char-p char)
	 (add-char x y char win)
	 (advance))
	(t (error "what char? ~s" (char-code char)))))))
(defun next-8 (n)
  "this is for tabbing, see waddch. its every 8th column"
  (* 8 (+ 1 (floor n 8))))
(defun add-char (x y value &optional (win *win*))
  (when (and (> (win-lines win) y -1)
	     (> (win-cols win) x -1))
    (setf (ref-grid x y (win-data win))
	  (gen-glyph value
		     (logior (win-attr-bits win)
			     *current-attributes*))))
  win)

(defun char-control (char)
  ;;FIXME: not portable common lisp, requires ASCII
  (let ((value (char-code char)))
	(if (> 32 value)
	    t
	    nil)))

(defun char-control-printable (char)
  ;;FIXME: not portable common lisp, requires ASCII
  (code-char (logior 64 (char-code char))))

(defun fuzz (&optional (win *win*))
  (dotimes (x 100)
    (add-char (random (win-cols win))
	      (random (win-lines win))
	      #\a
	      win))
  win)

(defun ncurses-wnoutrefresh (&optional (win *win*))
  ;;;FIXME:: follow https://linux.die.net/man/3/wnoutrefresh with "touching"
  ;;;different lines
  (when (win-clearok win)
    ;;FIXME -> clearok? what to do? check this: https://linux.die.net/man/3/clearok
    (setf (win-clearok win) nil))
  (with-virtual-window-lock
    (let ((grid (win-data win))
	  (xwin (win-x win))
	  (ywin (win-y win))
	  ;;(cursor-x (win-cursor-x win))
	  ;;(cursor-y (win-cursor-y win))
	  (columns (length (aref *virtual-window* 0)))
	  (lines (length *virtual-window*)))
      (dotimes (y (win-lines win))
	(dotimes (x (win-cols win))
	  (let ((glyph (ref-grid x y grid)))
	    (let ((xdest (+ xwin x))
		  (ydest (+ ywin y)))
	      (when (and (> columns xdest -1)
			 (> lines ydest -1))

		#+nil
		(when (and (= cursor-x xdest)
			   (= cursor-y ydest))
		  (setf glyph
			(logior glyph
				(prepare-attributes-for-glyph a_reverse))))
		(set-virtual-window xdest
				    ydest
				    glyph
				    )))))))))

(defparameter *update-p* nil)
(defun ncurses-doupdate ()
  (setf *update-p* t)) ;;;when copied to opengl buffer, set again to nil

(defparameter *cursor-state* :normal)
(defun ncurses-curs-set (value)
  "The curs_set routine sets the cursor state is set to invisible, normal, or very visible for visibility equal to 0, 1, or 2 respectively. If the terminal supports the visibility requested, the previous cursor state is returned; otherwise, ERR is returned."
  (setf *cursor-state*
	(case value
	  (0 :invisible)
	  (1 :normal)
	  (2 :very-visible))))

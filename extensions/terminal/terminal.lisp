(uiop:define-package :lem-terminal/terminal
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi)
                    (:queue :lem/common/queue)
                    (:line :lem/buffer/line))
  (:export :find-terminal-buffer
           :create
           :destroy
           :copy-mode-on
           :copy-mode-off
           :clear
           :render
           :update
           :input-character
           :input-key
           :resize
           :adjust-point
           :scrollback-count
           :activate-scrollback
           :deactivate-scrollback
           :snap-to-bottom))
(in-package :lem-terminal/terminal)

;;; --- module-level state and tunables ---------------------------------
;;; Kept at the top of the file per the contract's file_structure_rule.

(defvar *terminal-id-counter* 0
  "Monotonic counter for `generate-terminal-id'.")

(defvar *terminals* '()
  "All live terminals, used by callbacks to locate their Lisp object by id.")

(defparameter *scrollback-limit* 500
  "Maximum number of scrollback lines stored in the ring buffer.")

(defparameter *write-timeout-ms* 12
  "Max milliseconds to spend parsing PTY data per update tick (xterm.js pattern).
Prevents UI freeze by yielding to the renderer after this budget.")

(defparameter *attribute-cache-limit* 4096
  "Maximum entries in a terminal's attribute cache.  When exceeded the cache
is cleared wholesale — cheaper than maintaining LRU bookkeeping on the
render hot path, and the cache refills quickly for the active palette.")

(defparameter *fixed-blue* (parse-color "#3465A4")
  "Replacement colour used by `fix-blue-color' for vterm's unreadable default blue.")

(defparameter *blue-to-fix* (make-color 0 0 #xe0)
  "vterm's default \"blue\" — too dark on most terminals; replaced with `*fixed-blue*'.")

;; Byte offsets into the packed cell-data struct (14 bytes per cell):
;; uint32 ch (0), uint8 fg_r(4), fg_g(5), fg_b(6), bg_r(7), bg_g(8), bg_b(9),
;; bold(10), underline(11), reverse(12), width(13)
(defconstant +cell-off-ch+ 0)
(defconstant +cell-off-fg-r+ 4)
(defconstant +cell-off-fg-g+ 5)
(defconstant +cell-off-fg-b+ 6)
(defconstant +cell-off-bg-r+ 7)
(defconstant +cell-off-bg-g+ 8)
(defconstant +cell-off-bg-b+ 9)
(defconstant +cell-off-bold+ 10)
(defconstant +cell-off-underline+ 11)
(defconstant +cell-off-reverse+ 12)

;;; --- id generator ----------------------------------------------------

(defun generate-terminal-id ()
  (incf *terminal-id-counter*))

;;; --- terminals registry ----------------------------------------------

(defun add-terminal (terminal)
  (push terminal *terminals*))

(defun find-terminal-by-id (id)
  (find id *terminals* :key #'terminal-id))

(defun remove-terminal (terminal)
  (alexandria:deletef *terminals* terminal))

(defun find-terminal-buffer ()
  (alexandria:when-let (terminal (first *terminals*))
    (terminal-buffer terminal)))

;;; terminal
(defclass terminal ()
  ((id :initarg :id
       :reader terminal-id)
   (viscus :initarg :viscus
           :reader terminal-viscus)
   (thread :initarg :thread
           :accessor terminal-thread)
   (buffer :initarg :buffer
           :reader terminal-buffer)
   (rows :initarg :rows
         :accessor terminal-rows)
   (cols :initarg :cols
         :accessor terminal-cols)
   (copy-mode :initform nil
              :accessor terminal-copy-mode)
   (dirty-rows :initform nil
               :accessor terminal-dirty-rows
               :documentation "Bit vector tracking which rows need re-rendering.")
   (dirty-any-p :initform nil
                :accessor terminal-dirty-any-p
                :documentation "Fast-path flag: T if any bit in dirty-rows is set.
Avoids an O(rows) scan in the hot update loop.")
   (scrollback-ring :initform nil
                    :accessor terminal-scrollback-ring
                    :documentation "Simple-vector ring buffer of scrollback line strings.")
   (scrollback-head :initform 0
                    :accessor terminal-scrollback-head
                    :type fixnum
                    :documentation "Write position in the scrollback ring (next slot to fill).")
   (scrollback-count :initform 0
                     :accessor scrollback-count
                     :type fixnum
                     :documentation "Number of valid entries in the scrollback ring.")
   (scrollback-active :initform nil
                      :accessor terminal-scrollback-active
                      :documentation "T when scrollback lines are displayed in the buffer.")
   (last-render-time :initform 0
                     :accessor terminal-last-render-time
                     :type fixnum
                     :documentation "Internal-real-time of last render, for throttling.")
   (render-skips :initform 0
                 :accessor terminal-render-skips
                 :type fixnum
                 :documentation "Number of consecutive frames skipped since last render.")
   (attribute-cache :initform (make-hash-table :test #'eql)
                    :accessor terminal-attribute-cache
                    :documentation "Cache of packed-fixnum-key -> attribute.")
   (render-deferred :initform nil
                    :accessor terminal-render-deferred
                    :type boolean
                    :documentation "T when a deferred render event is already in the queue.")))

(defun mark-all-rows-dirty (terminal)
  "Mark every row as dirty so the next render redraws the full screen."
  (let ((dirty (terminal-dirty-rows terminal))
        (rows (terminal-rows terminal)))
    (if (and dirty (= (length dirty) rows))
        (fill dirty 1)
        (setf (terminal-dirty-rows terminal)
              (make-array rows :element-type 'bit :initial-element 1))))
  (setf (terminal-dirty-any-p terminal) t))

(defun mark-rows-dirty (terminal start-row end-row)
  "Mark rows from START-ROW (inclusive) to END-ROW (exclusive) as dirty."
  (let ((dirty (terminal-dirty-rows terminal)))
    (when dirty
      (loop :for r :from (max 0 start-row) :below (min end-row (length dirty))
            :do (setf (aref dirty r) 1))
      (setf (terminal-dirty-any-p terminal) t))))

(defun any-rows-dirty-p (terminal)
  "Return T if any row is marked dirty.  O(1) — reads the cached flag."
  (terminal-dirty-any-p terminal))

(defun update (terminal)
  (process-input terminal)
  (when (and (not (terminal-copy-mode terminal))
             (any-rows-dirty-p terminal))
    (cond
      ;; Other events pending — defer render to avoid starving key handling.
      ;; Schedule a retry event so dirty rows are rendered even if no more
      ;; PTY data arrives (fixes missing prompt after command finishes).
      ((> (event-queue-length) 0)
       (unless (terminal-render-deferred terminal)
         (setf (terminal-render-deferred terminal) t)
         (send-event (lambda ()
                       (setf (terminal-render-deferred terminal) nil)
                       (ignore-errors (update terminal))))))
      (t
       ;; Render immediately.  Previous versions skipped frames when the
       ;; throttle interval hadn't elapsed, but that caused the final frame
       ;; (e.g. shell prompt) to be lost when no more PTY data followed.
       (setf (terminal-last-render-time terminal) (get-internal-real-time)
             (terminal-render-skips terminal) 0)
       (render terminal)
       (redraw-display)))))

(defun create (&key (rows (alexandria:required-argument :rows))
                    (cols (alexandria:required-argument :cols))
                    (buffer (alexandria:required-argument :buffer))
                    (directory (alexandria:required-argument :directory)))
  (declare (type (string) directory)
           (type (integer) rows)
           (type (integer) cols))
  (let* ((id (generate-terminal-id))
         (terminal
           (make-instance 'terminal
                          :id id
                          :viscus (ffi::terminal-new directory id rows cols)
                          :buffer buffer
                          :rows rows
                          :cols cols)))
    (setf (terminal-scrollback-ring terminal)
          (make-array *scrollback-limit* :initial-element nil))
    (mark-all-rows-dirty terminal)
    ;; Alacritty-style WakeupGate: the I/O thread only sends a new event
    ;; if one isn't already pending.  When pending, it sleeps briefly to
    ;; avoid hot-spinning on select() (PTY data isn't consumed until the
    ;; editor thread runs process-input).
    (let ((event-pending nil)
          (lock (bt2:make-lock :name "terminal-wakeup")))
      (setf (terminal-thread terminal)
            (bt2:make-thread
             (lambda ()
               (loop
                 (ffi::terminal-process-input-wait (terminal-viscus terminal))
                 (bt2:with-lock-held (lock)
                   (cond
                     (event-pending
                      ;; Previous event not yet consumed — sleep to avoid
                      ;; hot-spinning (select returns immediately since
                      ;; data is still in the PTY buffer).
                      (sleep 0.001))
                     (t
                      (setf event-pending t)
                      (send-event
                       (lambda ()
                         (ignore-errors (update terminal))
                         (bt2:with-lock-held (lock)
                           (setf event-pending nil)))))))))
             :name (format nil "Terminal ~D" id))))
    (add-terminal terminal)
    terminal))

(defmethod destroy ((terminal terminal))
  (bt2:destroy-thread (terminal-thread terminal))
  (remove-terminal terminal)
  (ffi::terminal-delete (terminal-viscus terminal)))

(defmethod copy-mode-on ((terminal terminal))
  (setf (terminal-copy-mode terminal) t))

(defmethod copy-mode-off ((terminal terminal))
  (setf (terminal-copy-mode terminal) nil))

(defun fix-blue-color (color)
  (if (lem/common/color:color-equal color *blue-to-fix*)
      *fixed-blue*
      color))

(declaim (inline pack-attribute-key))
(defun pack-attribute-key (fg-r fg-g fg-b bg-r bg-g bg-b bold underline reverse-attr)
  "Pack cell style into a single fixnum key (51 bits — fits in a 64-bit fixnum).
Attr flags are masked to one bit each: some vterm builds expose underline as
a multi-bit style enum, and an unmasked value would collide with the bg-b
field (bits 3..10) or with neighbouring flags."
  (logior (ash fg-r 43) (ash fg-g 35) (ash fg-b 27)
          (ash bg-r 19) (ash bg-g 11) (ash bg-b 3)
          (ash (logand bold 1) 2)
          (ash (logand underline 1) 1)
          (logand reverse-attr 1)))

(defun default-bg-p (r g b)
  "Return T if the RGB values represent the terminal default background (black)."
  (and (zerop r) (zerop g) (zerop b)))

(defun get-attribute-for-cell (terminal fg-r fg-g fg-b bg-r bg-g bg-b bold underline reverse-attr)
  "Return a cached attribute for the given cell style components."
  (let* ((key (pack-attribute-key fg-r fg-g fg-b bg-r bg-g bg-b bold underline reverse-attr))
         (cache (terminal-attribute-cache terminal)))
    (or (gethash key cache)
        (progn
          (when (>= (hash-table-count cache) *attribute-cache-limit*)
            (clrhash cache))
          (setf (gethash key cache)
                (make-attribute :foreground (fix-blue-color (make-color fg-r fg-g fg-b))
                                :background (if (default-bg-p bg-r bg-g bg-b)
                                                nil
                                                (fix-blue-color (make-color bg-r bg-g bg-b)))
                                :reverse (= 1 (logand reverse-attr 1))
                                :bold (= 1 (logand bold 1))
                                :underline (= 1 (logand underline 1))))))))

;; Cached struct size — resolved once at load time so we never call
;; cffi:foreign-type-size on the render hot path and don't carry a
;; mutable lazy-init global.
(declaim (inline cell-data-size))
(defun cell-data-size ()
  (load-time-value (cffi:foreign-type-size '(:struct ffi::cell-data)) t))

(defun render-row-direct (terminal line-obj cols row buf cell-size)
  "Render a row directly into LINE-OBJ, bypassing the buffer edit API.

CONTRACT: this deliberately skips before/after-change hooks, undo
recording, and marker shifting.  It is safe ONLY for the terminal
buffer, whose content is owned exclusively by the vterm renderer:
nothing else writes to it, nothing observes its change hooks (the
terminal mode bypasses standard editing commands in EXECUTE), and
undo is irrelevant for a live shell.  Do not use this on any buffer
that may be edited by the user or watched by other features."
  (ffi::terminal-get-row (terminal-viscus terminal) row buf cols)
  (let ((chars (make-string cols))
        (attrs nil)
        (run-start 0)
        (previous-attribute nil))
    (loop :for col :from 0 :below cols
          :for ptr := (cffi:inc-pointer buf (* col cell-size))
          :do (let* ((ch (cffi:mem-ref ptr :uint32 +cell-off-ch+))
                     (fg-r (cffi:mem-ref ptr :uint8 +cell-off-fg-r+))
                     (fg-g (cffi:mem-ref ptr :uint8 +cell-off-fg-g+))
                     (fg-b (cffi:mem-ref ptr :uint8 +cell-off-fg-b+))
                     (bg-r (cffi:mem-ref ptr :uint8 +cell-off-bg-r+))
                     (bg-g (cffi:mem-ref ptr :uint8 +cell-off-bg-g+))
                     (bg-b (cffi:mem-ref ptr :uint8 +cell-off-bg-b+))
                     (bold (cffi:mem-ref ptr :uint8 +cell-off-bold+))
                     (underline (cffi:mem-ref ptr :uint8 +cell-off-underline+))
                     (reverse-attr (cffi:mem-ref ptr :uint8 +cell-off-reverse+))
                     (attribute (get-attribute-for-cell terminal
                                                       fg-r fg-g fg-b
                                                       bg-r bg-g bg-b
                                                       bold underline reverse-attr))
                     ;; Fast validity check instead of ignore-errors + code-char
                     (char (if (and (plusp ch) (<= ch #x10FFFF))
                               (code-char ch)
                               #\Space)))
                (unless (eq attribute previous-attribute)
                  (when previous-attribute
                    (push (list run-start col previous-attribute) attrs))
                  (setf run-start col)
                  (setf previous-attribute attribute))
                (setf (schar chars col) char))
          :finally (when previous-attribute
                     (push (list run-start cols previous-attribute) attrs)))
    ;; Directly set line content — bypass all buffer edit machinery.
    ;; See CONTRACT above; this is permitted because the terminal buffer
    ;; is rendered-into, not edited.
    (line:set-line-string chars line-obj)
    (setf (getf (line:line-plist line-obj) :attribute)
          (nreverse attrs))))

(defun deactivate-scrollback (terminal)
  "Remove scrollback lines from the buffer, keeping only terminal rows.
Called when transitioning from scrollback-active to normal view."
  (when (terminal-scrollback-active terminal)
    (setf (terminal-scrollback-active terminal) nil)
    (let* ((buffer (terminal-buffer terminal))
           (point (buffer-point buffer))
           (rows (terminal-rows terminal))
           (nlines (buffer-nlines buffer)))
      (when (> nlines rows)
        (with-buffer-read-only buffer nil
          ;; Delete all lines before the terminal area
          (buffer-start point)
          (with-point ((end point))
            (move-to-line end (1+ (- nlines rows)))
            (line-start end)
            (delete-between-points point end)))))))

(defun activate-scrollback (terminal)
  "Insert scrollback lines from the ring buffer into the buffer above the terminal rows."
  (unless (terminal-scrollback-active terminal)
    (let ((count (scrollback-count terminal)))
      (when (plusp count)
        (setf (terminal-scrollback-active terminal) t)
        (let* ((buffer (terminal-buffer terminal))
               (point (buffer-point buffer))
               (ring (terminal-scrollback-ring terminal))
               (head (terminal-scrollback-head terminal))
               (limit (length ring)))
          (with-buffer-read-only buffer nil
            ;; Build scrollback text and insert at top of buffer
            (let ((combined
                    (with-output-to-string (s)
                      (loop :for i :from 0 :below count
                            :for idx := (mod (- head count (- i)) limit)
                            :for line := (aref ring idx)
                            :do (when line (write-string line s))
                                (write-char #\newline s)))))
              (buffer-start point)
              (insert-string point combined))))))))

(defun snap-to-bottom (terminal)
  "Reset view of every window showing the terminal buffer to the terminal area."
  (let ((buffer (terminal-buffer terminal)))
    (dolist (window (get-buffer-windows buffer))
      (buffer-start (window-view-point window)))))

(defmethod render ((terminal terminal))
  (let* ((viscus (terminal-viscus terminal))
         (rows (terminal-rows terminal))
         (cols (terminal-cols terminal))
         (buffer (terminal-buffer terminal))
         (point (buffer-point buffer))
         (dirty (terminal-dirty-rows terminal))
         (cell-size (cell-data-size)))
    ;; If scrollback is displayed, remove it before rendering terminal rows
    (deactivate-scrollback terminal)
    (with-buffer-read-only buffer nil
      ;; Ensure the buffer has exactly ROWS lines
      (let ((nlines (buffer-nlines buffer)))
        (cond ((< nlines rows)
               (buffer-end point)
               (dotimes (i (- rows nlines))
                 (insert-character point #\newline)))
              ((> nlines rows)
               (move-to-line point (1+ rows))
               (line-start point)
               (with-point ((start point))
                 (character-offset start -1)
                 (buffer-end point)
                 (delete-between-points start point)))))
      ;; Get the first line object and walk the linked list directly.
      ;; This avoids all point manipulation during row rendering.
      (move-to-line point 1)
      (let ((first-line (point-line point)))
        (cffi:with-foreign-object (buf '(:struct ffi::cell-data) cols)
          (cond
            ;; Full redraw: render every row directly
            ((or (null dirty)
                 (/= (length dirty) rows))
             (loop :for row :from 0 :below rows
                   :for cur-line := first-line :then (line:line-next cur-line)
                   :while cur-line
                   :do (render-row-direct terminal cur-line cols row buf cell-size))
             (setf (terminal-dirty-rows terminal)
                   (make-array rows :element-type 'bit :initial-element 0))
             (setf (terminal-dirty-any-p terminal) nil))
            ;; Partial update: only dirty rows
            (t
             (loop :for row :from 0 :below rows
                   :for cur-line := first-line :then (line:line-next cur-line)
                   :while cur-line
                   :do (when (= 1 (aref dirty row))
                         (render-row-direct terminal cur-line cols row buf cell-size)))
             (fill dirty 0)
             (setf (terminal-dirty-any-p terminal) nil)))))
      ;; Position cursor
      (move-to-line point (1+ (ffi::terminal-cursor-row viscus)))
      (move-to-column point (ffi::terminal-cursor-col viscus)))))

(defmethod adjust-point ((terminal terminal))
  (let* ((buffer (terminal-buffer terminal))
         (point (buffer-point buffer))
         (viscus (terminal-viscus terminal)))
    (move-to-line point (1+ (ffi::terminal-cursor-row viscus)))
    (move-to-column point (ffi::terminal-cursor-col viscus))))

(defmethod process-input ((terminal terminal))
  "Drain PTY data with a time budget to avoid blocking the UI thread."
  (ffi::terminal-process-input-timed (terminal-viscus terminal)
                                     *write-timeout-ms*))

(defmethod input-character ((terminal terminal) character &key (mod 0))
  (ffi::terminal-input-char (terminal-viscus terminal)
                            (char-code character)
                            mod))

(defmethod input-key ((terminal terminal) key &key (mod 0))
  (ffi::terminal-input-key (terminal-viscus terminal)
                           key
                           mod))

(defun same-size-p (terminal rows cols)
  (and (= (terminal-rows terminal) rows)
       (= (terminal-cols terminal) cols)))

(defmethod resize ((terminal terminal)
                   &key (rows (alexandria:required-argument :rows))
                        (cols (alexandria:required-argument :cols)))
  (unless (same-size-p terminal rows cols)
    (setf (terminal-rows terminal) rows
          (terminal-cols terminal) cols)
    (ffi::terminal-resize (terminal-viscus terminal) rows cols)
    (mark-all-rows-dirty terminal)
    (clrhash (terminal-attribute-cache terminal))))

;;; callbacks
(defun cb-damage (rect id)
  "Mark damaged rows as dirty using the VTermRect region."
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (cffi:with-foreign-slots ((ffi::start-row ffi::end-row) rect (:struct ffi::vterm-rect))
        (mark-rows-dirty terminal ffi::start-row ffi::end-row)))))

(defun cb-moverect (dest src id)
  "Mark both source and destination regions as dirty."
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (cffi:with-foreign-slots ((ffi::start-row ffi::end-row) dest (:struct ffi::vterm-rect))
        (mark-rows-dirty terminal ffi::start-row ffi::end-row))
      (cffi:with-foreign-slots ((ffi::start-row ffi::end-row) src (:struct ffi::vterm-rect))
        (mark-rows-dirty terminal ffi::start-row ffi::end-row)))))

(defun cb-movecursor (pos oldpos visible id)
  "Mark both old and new cursor rows as dirty."
  (declare (ignore visible))
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (cffi:with-foreign-slots ((ffi::row) pos (:struct ffi::vterm-pos))
        (mark-rows-dirty terminal ffi::row (1+ ffi::row)))
      (cffi:with-foreign-slots ((ffi::row) oldpos (:struct ffi::vterm-pos))
        (mark-rows-dirty terminal ffi::row (1+ ffi::row))))))

(defun cb-settermprop (prop val id)
  (declare (ignore prop val id)))

(defun cb-bell (id)
  (declare (ignore id)))

(defun cb-resize (rows cols id)
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (setf (terminal-rows terminal) rows
            (terminal-cols terminal) cols)
      (mark-all-rows-dirty terminal))))

(defun cb-sb-pushline (cols cells id)
  "Store scrollback line in the ring buffer.  Uses zero-allocation C extraction
into the terminal's per-terminal buffer; trailing spaces are stripped C-side
so only the final Lisp string is allocated here."
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (let* ((ring (terminal-scrollback-ring terminal))
             (limit (length ring))
             (head (terminal-scrollback-head terminal)))
        ;; Extract text via the C extension's per-terminal buffer.
        (cffi:with-foreign-object (len-ptr :int)
          (let* ((c-str (ffi::terminal-sb-line-extract
                         (terminal-viscus terminal) cols cells len-ptr))
                 (len (cffi:mem-ref len-ptr :int))
                 (text (cffi:foreign-string-to-lisp c-str :count len :encoding :utf-8)))
            (setf (aref ring head) text)))
        (setf (terminal-scrollback-head terminal) (mod (1+ head) limit))
        (when (< (scrollback-count terminal) limit)
          (incf (scrollback-count terminal))))
      ;; Must mark all rows dirty because vterm scrolls content up internally
      ;; and cb-damage may only report the newly-written bottom row.
      (mark-all-rows-dirty terminal))))

(defun cb-sb-popline (cols cells id)
  "Remove the most recent scrollback line from the ring."
  (declare (ignore cols cells))
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (when (plusp (scrollback-count terminal))
        (let* ((ring (terminal-scrollback-ring terminal))
               (limit (length ring))
               (head (terminal-scrollback-head terminal))
               (new-head (mod (1- head) limit)))
          (setf (aref ring new-head) nil)
          (setf (terminal-scrollback-head terminal) new-head)
          (decf (scrollback-count terminal))))
      (mark-all-rows-dirty terminal))))

(ffi::set-callbacks :damage 'cb-damage
                    :moverect 'cb-moverect
                    :movecursor 'cb-movecursor
                    :settermprop 'cb-settermprop
                    :bell 'cb-bell
                    :resize 'cb-resize
                    :sb-pushline 'cb-sb-pushline
                    :sb-popline 'cb-sb-popline)

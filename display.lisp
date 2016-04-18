;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(defstruct (display (:constructor %make-display))
  screen
  lines
  width)

(defun make-display (screen width height)
  (%make-display :screen screen
                 :width width
                 :lines (make-array height :initial-element nil)))

(defun display-height (display)
  (length (display-lines display)))

(defun disp-set-size (display width height)
  (setf (display-lines display)
        (make-array height :initial-element nil))
  (setf (display-width display)
        width))

(defun set-attr-display-line (disp-lines
                              attr
                              start-linum
                              linum
                              start-charpos
                              end-charpos)
  (let ((i (- linum start-linum)))
    (when (<= 0 i (1- (length disp-lines)))
      (unless end-charpos
        (setq end-charpos (fat-length (aref disp-lines i))))
      (let ((fatstr (aref disp-lines i)))
        (change-font fatstr
                     attr
                     :to
                     start-charpos
                     (min end-charpos (fat-length fatstr)))))))

(defun set-attr-display-lines (disp-lines
                               attr
                               top-linum
                               start-linum
                               start-charpos
                               end-linum
                               end-charpos)
  (set-attr-display-line disp-lines
                         attr
                         top-linum
                         start-linum
                         start-charpos
                         nil)
  (loop :for linum :from (1+ start-linum) :below end-linum :do
    (set-attr-display-line disp-lines
                           attr
                           top-linum
                           linum
                           0
                           nil))
  (set-attr-display-line disp-lines
                         attr
                         top-linum
                         end-linum
                         0
                         end-charpos))

(defun disp-set-overlays (disp-lines overlays start-linum end-linum)
  (loop
    :for overlay :in overlays
    :for start := (overlay-start overlay)
    :for end := (overlay-end overlay)
    :do (cond ((and (= (point-linum start) (point-linum end))
                    (<= start-linum (point-linum start) (1- end-linum)))
               (set-attr-display-line disp-lines
                                      (overlay-attr overlay)
                                      start-linum
                                      (point-linum start)
                                      (point-charpos start)
                                      (point-charpos end)))
              ((and (<= start-linum (point-linum start))
                    (< (point-linum end) end-linum))
               (set-attr-display-lines disp-lines
                                       (overlay-attr overlay)
                                       start-linum
                                       (point-linum start)
                                       (point-charpos start)
                                       (point-linum end)
                                       (point-charpos end)))
              ((<= (point-linum start)
                   start-linum
                   (point-linum end)
                   end-linum)
               (set-attr-display-lines disp-lines
                                       (overlay-attr overlay)
                                       start-linum
                                       start-linum
                                       0
                                       (point-linum end)
                                       (point-charpos end)))
              ((<= start-linum
                   (point-linum start))
               (set-attr-display-lines disp-lines
                                       (overlay-attr overlay)
                                       start-linum
                                       (point-linum start)
                                       (point-charpos start)
                                       end-linum
                                       nil)))))

(defun disp-reset-lines (disp-lines buffer start-linum)
  (buffer-update-mark-overlay buffer)
  (let ((end-linum (+ start-linum (length disp-lines)))
        (disp-index 0))
    (loop
      :for linum :from start-linum :to (buffer-nlines buffer)
      :while (< disp-index (length disp-lines)) :do
      (setf (aref disp-lines disp-index)
            (copy-fatstring (buffer-line-fatstring buffer linum)))
      (incf disp-index))
    (loop
      :for i :from disp-index :below (length disp-lines)
      :do (setf (aref disp-lines i) nil))
    (disp-set-overlays disp-lines
                       (buffer-overlays buffer)
                       start-linum
                       end-linum)))

(defun disp-print-line (display y str &key (start-x 0) (string-start 0) string-end)
  (let ((x start-x)
        (win (display-screen display)))
    (loop :for i :from string-start :below (or string-end (fat-length str)) :do
      (multiple-value-bind (char attr)
          (fat-char str i)
        (when (ctrl-p char)
          (setq attr (make-attr :color "red")))
        (charms/ll:wattron win attr)
        (charms/ll:mvwaddstr win y x (string char))
        (charms/ll:wattroff win attr)
        (setq x (char-width char x))))))

(defun disp-line-wrapping (display start-charpos curx cury pos-x y str)
  (when (and (< 0 start-charpos) (= y 0))
    (setq str (fat-substring str start-charpos)))
  (when (= y cury)
    (setq curx (str-width (fat-string str) 0 pos-x)))
  (loop :with start := 0 :and width := (display-width display)
        :for i := (wide-index (fat-string str) (1- width) :start start)
        :while (< y (display-height display))
        :do (cond ((null i)
                   (disp-print-line display y str :string-start start)
                   (return))
                  (t
                   (cond ((< y cury)
                          (incf cury))
                         ((= y cury)
                          (let ((len (str-width (fat-string str) start i)))
                            (when (<= len curx)
                              (decf curx len)
                              (incf cury)))))
                   (disp-print-line display y str :string-start start :string-end i)
                   (disp-print-line display y (load-time-value (make-fatstring "!" 0)) :start-x (1- width))
                   (incf y)
                   (setq start i))))
  (values curx cury y))

(defun disp-line (display start-charpos curx cury pos-x y str)
  (declare (ignore start-charpos))
  (check-type str fatstring)
  (when (= cury y)
    (setq curx (str-width (fat-string str) 0 pos-x)))
  (let ((width (str-width (fat-string str)))
        (cols (display-width display)))
    (cond
      ((< width (display-width display))
       nil)
      ((or (/= cury y)
           (< curx (1- cols)))
       (let ((i (wide-index (fat-string str) (1- cols))))
         (setq str
               (if (<= cols (str-width (fat-string str) 0 i))
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
    (disp-print-line display y str))
  (values curx cury y))

(defun disp-lines (display buffer start-charpos start-linum pos-x pos-y)
  (disp-reset-lines (display-lines display) buffer start-linum)
  (let ((curx 0)
        (cury (- pos-y start-linum))
        (disp-line-fun
          (if (buffer-truncate-lines buffer)
              #'disp-line-wrapping
              #'disp-line)))
    (loop
      :with y := 0
      :for str :across (display-lines display)
      :while (< y (display-height display))
      :do (cond (str
                 (multiple-value-setq (curx cury y)
                   (funcall disp-line-fun
                            display start-charpos curx cury pos-x y str))
                 (incf y))
                (t
                 (return))))
    (charms/ll:wmove (display-screen display) cury curx)))

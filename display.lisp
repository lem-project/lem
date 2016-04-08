;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(defstruct (display (:constructor %make-display))
  screen
  lines
  )

(defun make-display (height)
  (%make-display :lines (make-array height :initial-element nil)))

(defun disp-set-height (display height)
  (setf (display-lines display)
        (make-array height :initial-element nil)))

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
                       end-linum)
    disp-lines))

(defun disp-lines (display buffer start-linum)
  (disp-reset-lines (display-lines display) buffer start-linum)
  (display-lines display))

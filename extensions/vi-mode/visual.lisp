(defpackage :lem-vi-mode/visual
  (:use :cl
        :lem
        :lem-vi-mode/core)
  (:import-from :lem-vi-mode/core
                :ensure-state)
  (:import-from :lem-vi-mode/states
                :*command-keymap*
                :normal)
  (:import-from :lem-vi-mode/modeline
                :state-modeline-orange)
  (:import-from :lem-base
                :alive-point-p)
  (:export :*visual-keymap*
           :vi-visual-end
           :vi-visual-char
           :vi-visual-line
           :vi-visual-block
           :visual-p
           :visual-char-p
           :visual-line-p
           :visual-block-p
           :visual-range
           :apply-visual-range
           :visual-yank
           :visual-kill
           :vi-visual-insert
           :vi-visual-append))
(in-package :lem-vi-mode/visual)

(defvar *start-point* nil)
(defvar *visual-overlays* '())

(defvar *visual-keymap* (make-keymap :name '*visual-keymap* :parent *command-keymap*))

(define-vi-state visual (vi-state) ()
  (:default-initargs
   :message "-- VISUAL --"
   :modeline-color 'state-modeline-orange
   :keymap *visual-keymap*))

(define-vi-state visual-char (visual)
  ()
  (:default-initargs :name "VISUAL"))

(define-vi-state visual-line (visual) ()
  (:default-initargs
   :name "V-LINE"
   :message "-- VISUAL LINE --"))

(define-vi-state visual-block (visual) ()
  (:default-initargs
   :name "V-BLOCK"
   :message "-- VISUAL BLOCK --"))

(defmethod state-enabled-hook :after ((state visual))
  (setf *start-point* (copy-point (current-point))))

(defmethod state-disabled-hook ((state visual))
  (delete-point *start-point*)
  (setf *start-point* nil)
  (clear-visual-overlays))

(defun disable ()
  (clear-visual-overlays))

(defun clear-visual-overlays ()
  (mapc 'delete-overlay *visual-overlays*)
  (setf *visual-overlays* '()))

(defmethod post-command-hook ((state visual))
  (clear-visual-overlays)
  (if (not (eq (current-buffer) (point-buffer *start-point*)))
      (vi-visual-end)
      (state-setup state)))

(defgeneric state-setup (visual-state))

(defmethod state-setup ((state visual-char))
  (with-point ((start *start-point*)
               (end (current-point)))
    (when (point< end start)
      (rotatef start end))
    (character-offset end 1)
    (push (make-overlay start end 'region)
          *visual-overlays*)))

(defmethod state-setup ((state visual-line))
  (with-point ((start *start-point*)
               (end (current-point)))
    (when (point< end start) (rotatef start end))
    (line-start start)
    (line-end end)
    (push (make-overlay start end 'region)
          *visual-overlays*)))

(defmethod state-setup ((state visual-block))
  (with-point ((start *start-point*)
               (end (current-point)))
    (when (point< end start)
      (rotatef start end))
    (character-offset end 1)
    (let ((start-column (point-column start))
          (end-column (point-column end)))
      (unless (< start-column end-column)
        (rotatef start-column end-column))
      (apply-region-lines start end
                          (lambda (p)
                            (with-point ((s p) (e p))
                              (move-to-column s start-column)
                              (move-to-column e end-column)
                              (push (make-overlay s e 'region) *visual-overlays*)))))))

(define-command vi-visual-end () ()
  (clear-visual-overlays)
  (change-state 'normal))

(defun enable-visual (new-state)
  (let ((new-state (ensure-state new-state))
        (current-state (current-state)))
    (cond
      ((typep current-state (class-name (class-of new-state)))
       (vi-visual-end))
      ((typep current-state 'visual)
       (check-type *start-point* point)
       (assert (alive-point-p *start-point*))
       (let ((start (copy-point *start-point*)))
         (prog1 (change-state new-state)
           (setf *start-point* start))))
      (t
       (change-state new-state)))))

(define-command vi-visual-char () ()
  (enable-visual 'visual-char))

(define-command vi-visual-line () ()
  (enable-visual 'visual-line))

(define-command vi-visual-block () ()
  (enable-visual 'visual-block))

(defun visual-p ()
  (typep (current-state) 'visual))

(defun visual-char-p ()
  (typep (current-state) 'visual-char))

(defun visual-line-p ()
  (typep (current-state) 'visual-line))

(defun visual-block-p ()
  (typep (current-state) 'visual-block))

(defun visual-range ()
  (let ((ov (sort (copy-list *visual-overlays*) #'point< :key #'overlay-start)))
    (if (visual-block-p)
        (list *start-point* (copy-point (current-point)))
        (progn
          (assert (null (rest ov)))
          (list
           (overlay-start (first ov))
           (overlay-end (first ov)))))))

(defun apply-visual-range (function)
  (dolist (ov (sort (copy-list *visual-overlays*) #'point< :key #'overlay-start))
    (funcall function
             (overlay-start ov)
             (overlay-end ov))))

(defun visual-yank ()
  (with-killring-context (:options (when (visual-line-p) :vi-line))
    (apply-visual-range
     (lambda (start end)
       (copy-region start end)))))

(defun visual-kill ()
  (with-killring-context (:options (when (visual-line-p) :vi-line))
    (apply-visual-range
     (lambda (start end)
       (kill-region start end)))))

(defun string-without-escape ()
  (concatenate 'string
               (loop for key-char = (key-to-char (read-key))
                     while (char/= #\Escape key-char)
                     collect key-char)))

(define-command vi-visual-append () ()
  (when (visual-block-p)
    (let ((str (string-without-escape))
          (max-end (apply #'max (mapcar (lambda (ov)
                                          (point-charpos (overlay-end ov)))
                                        *visual-overlays*))))
      (apply-visual-range (lambda (start end)
                            (unless (point< start end)
                              (rotatef start end))
                            (let* ((space-len (- max-end (point-charpos end)))
                                   (spaces (make-string space-len
                                                        :initial-element #\Space)))
                              (insert-string end (concatenate 'string
                                                              spaces
                                                              str))))))
    (vi-visual-end)))

(define-command vi-visual-insert () ()
  (when (visual-block-p)
    (let ((str (string-without-escape)))
      (apply-visual-range (lambda (start end)
                            (unless (point< start end)
                              (rotatef start end))
                            (insert-string start str))))
    (vi-visual-end)))

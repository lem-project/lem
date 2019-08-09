(defpackage :lem-vi-mode.visual
  (:use :cl
        :lem
        :lem-vi-mode.core)
  (:export :vi-visual-end
           :vi-visual-char
           :vi-visual-line
           :vi-visual-block
           :visual-p
           :visual-char-p
           :visual-line-p
           :visual-block-p
           :apply-visual-range
           :vi-visual-insert
           :vi-visual-append
           :vi-visual-upcase
           :vi-visual-downcase))
(in-package :lem-vi-mode.visual)

(defvar *set-visual-function* nil)
(defvar *start-point* nil)
(defvar *visual-overlays* '())

(defvar *visual-keymap* (make-keymap :name '*visual-keymap* :parent *command-keymap*))

(define-key *visual-keymap* "Escape" 'vi-visual-end)
(define-key *visual-keymap* "A" 'vi-visual-append)
(define-key *visual-keymap* "I" 'vi-visual-insert)
(define-key *visual-keymap* "U" 'vi-visual-upcase)
(define-key *visual-keymap* "u" 'vi-visual-downcase)

(define-vi-state visual (:keymap *visual-keymap*
                         :post-command-hook 'post-command-hook)
  (:disable ()
   (delete-point *start-point*)
   (clear-visual-overlays))
  (:enable (function)
   (setf *set-visual-function* function)
   (setf *start-point* (copy-point (current-point)))))

(defun disable ()
  (clear-visual-overlays))

(defun clear-visual-overlays ()
  (mapc 'delete-overlay *visual-overlays*)
  (setf *visual-overlays* '()))

(defun post-command-hook ()
  (clear-visual-overlays)
  (if (not (eq (current-buffer) (point-buffer *start-point*)))
      (vi-visual-end)
      (funcall *set-visual-function*)))

(defun visual-char ()
  (with-point ((start *start-point*)
               (end (current-point)))
    (when (point< end start)
      (rotatef start end))
    (character-offset end 1)
    (push (make-overlay start end 'region)
          *visual-overlays*)))

(defun visual-line ()
  (with-point ((start *start-point*)
               (end (current-point)))
    (when (point< end start) (rotatef start end))
    (line-start start)
    (line-end end)
    (push (make-overlay start end 'region)
          *visual-overlays*)))

(defun visual-block ()
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
  (change-state 'command))

(define-command vi-visual-char () ()
  (if (visual-char-p)
      (vi-visual-end)
      (progn
        (change-state 'visual 'visual-char)
        (message "-- VISUAL --"))))

(define-command vi-visual-line () ()
  (if (visual-line-p)
      (vi-visual-end)
      (progn
        (change-state 'visual 'visual-line)
        (message "-- VISUAL LINE --"))))

(define-command vi-visual-block () ()
  (if (visual-block-p)
      (vi-visual-end)
      (progn
        (change-state 'visual 'visual-block)
        (message "-- VISUAL BLOCK --"))))

(defun visual-p ()
  (eq 'visual (current-state)))

(defun visual-char-p ()
  (and (visual-p)
       (eq *set-visual-function* 'visual-char)))

(defun visual-line-p ()
  (and (visual-p)
       (eq *set-visual-function* 'visual-line)))

(defun visual-block-p ()
  (and (visual-p)
       (eq *set-visual-function* 'visual-block)))

(defun apply-visual-range (function)
  (dolist (ov (sort (copy-list *visual-overlays*) #'point< :key #'overlay-start))
    (funcall function
             (overlay-start ov)
             (overlay-end ov))))

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

(defun %visual-case (f)
  (with-point ((start *start-point*)
               (end (current-point)))
    (apply-visual-range f)
    (vi-visual-end)
    (move-point (current-point) (if (point< start end)
                                    start
                                    end))))

(define-command vi-visual-upcase () ()
  (%visual-case #'uppercase-region))

(define-command vi-visual-downcase () ()
  (%visual-case #'downcase-region))

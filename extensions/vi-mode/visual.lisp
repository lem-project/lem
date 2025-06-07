(defpackage :lem-vi-mode/visual
  (:use :cl
        :lem
        :lem-vi-mode/core)
  (:import-from :lem-vi-mode/core
                :buffer-state
                :ensure-state)
  (:import-from :lem-vi-mode/states
                :*motion-keymap*
                :*normal-keymap*
                :normal)
  (:import-from :lem-vi-mode/modeline
                :state-modeline-orange)
  (:import-from :lem
                :alive-point-p)
  (:import-from :alexandria
                :when-let
                :last-elt)
  (:export :*visual-keymap*
           :vi-visual-end
           :vi-visual-char
           :vi-visual-line
           :vi-visual-block
           :visual
           :visual-p
           :visual-char-p
           :visual-line-p
           :visual-block-p
           :visual-range
           :apply-visual-range
           :vi-visual-insert
           :vi-visual-append
           :vi-visual-swap-points
           :vi-visual-opposite-side))
(in-package :lem-vi-mode/visual)

(defvar *visual-keymap* (make-keymap :name '*visual-keymap*))

(defmethod make-region-overlays-using-global-mode ((global-mode vi-mode) cursor)
  (let ((buffer (point-buffer cursor)))
    (visual-overlays buffer)))

(defun visual-overlays (buffer)
  (cond
    ;; Char mode
    ((visual-char-p buffer)
     (with-point ((start (buffer-mark buffer))
                  (end (buffer-point buffer)))
       (when (point< end start)
         (rotatef start end))
       (character-offset end 1)
       (list (make-overlay start end 'region :temporary t))))
    ;; Line mode
    ((visual-line-p buffer)
     (let ((overlays '()))
       (apply-region-lines (buffer-mark buffer) (buffer-point buffer)
                           (lambda (p)
                             (push (make-line-overlay p 'region :temporary t)
                                   overlays)))
       overlays))
    ;; Block mode
    ((visual-block-p buffer)
     (let ((overlays '()))
       (with-point ((start (buffer-mark buffer))
                    (end (buffer-point buffer)))
         (let ((start-column (point-column start))
               (end-column (point-column end)))
           (cond
             ;; left-top or left-bottom
             ((< end-column start-column)
              (character-offset start 1)
              (setf start-column (point-column start)))
             ;; right-top or right-bottom
             (t
              (unless (= end-column (length (line-string end)))
                (character-offset end 1))
              (setf end-column (point-column end))))
           (apply-region-lines start end
                               (lambda (p)
                                 (with-point ((s p) (e p))
                                   (move-to-column s start-column)
                                   (move-to-column e end-column)
                                   (push (make-overlay s e 'region :temporary t) overlays))))))
       overlays))
    (t '())))

(define-state visual (vi-state) ()
  (:default-initargs
   :modeline-color 'state-modeline-orange
   :keymaps (list *visual-keymap* *motion-keymap* *normal-keymap*)))

(define-state visual-char (visual) ()
  (:default-initargs
   :name "VISUAL"))

(define-state visual-line (visual) ()
  (:default-initargs
   :name "V-LINE"))

(define-state visual-block (visual) ()
  (:default-initargs
   :name "V-BLOCK"))

(defmethod buffer-state-enabled-hook :after ((state visual) buffer)
  (unless (buffer-mark-p buffer)
    (setf (buffer-mark buffer) (buffer-point buffer))))

(defmethod buffer-state-disabled-hook ((state visual) buffer))

(define-command vi-visual-end (&optional (buffer (current-buffer))) ()
  (buffer-mark-cancel buffer)
  (setf (buffer-state buffer) 'normal))

(defun enable-visual (new-state buffer)
  (let ((new-state (ensure-state new-state))
        (current-state (buffer-state buffer)))
    (cond
      ((typep current-state (class-name (class-of new-state)))
       (vi-visual-end buffer))
      ((typep current-state 'visual)
       (with-point ((mark (buffer-mark buffer)))
         (prog1 (setf (buffer-state buffer) new-state)
           (setf (buffer-mark buffer) mark))))
      (t
       (setf (buffer-state buffer) new-state)
       (unless (buffer-mark-p buffer)
         (setf (buffer-mark buffer) (current-point)))))))

(define-command vi-visual-char (&optional (buffer (current-buffer))) ()
  (enable-visual 'visual-char buffer))

(define-command vi-visual-line (&optional (buffer (current-buffer))) ()
  (enable-visual 'visual-line buffer))

(define-command vi-visual-block (&optional (buffer (current-buffer))) ()
  (enable-visual 'visual-block buffer))

(defun visual-p (&optional (buffer (current-buffer)))
  (typep (buffer-state buffer) 'visual))

(defun visual-char-p (&optional (buffer (current-buffer)))
  (typep (buffer-state buffer) 'visual-char))

(defun visual-line-p (&optional (buffer (current-buffer)))
  (typep (buffer-state buffer) 'visual-line))

(defun visual-block-p (&optional (buffer (current-buffer)))
  (typep (buffer-state buffer) 'visual-block))

(defun visual-range (&optional (buffer (current-buffer)))
  (with-point ((start (buffer-mark buffer))
               (end (buffer-point buffer)))
    (cond
      ((visual-char-p buffer)
       (cond ((point<= start end)
              (character-offset end 1))
             ((point< end start)
              (character-offset start 1)))
       (list start end))
      ((visual-block-p buffer)
       (list start end))
      (t
       (when (point< end start)
         (rotatef start end))
       (line-start start)
       (or (line-offset end 1 0)
           (line-end end))
       (list start end)))))

(defun (setf visual-range) (new-range &optional (buffer (current-buffer)))
  (check-type new-range list)
  (destructuring-bind (start end) new-range
    (cond
      ((point< start end)
       (character-offset end -1))
      ((point< end start)
       (character-offset start -1)))
    (cond
      ((or (visual-char-p buffer)
           (visual-block-p buffer))
       (setf (buffer-mark buffer) start)
       (move-point (buffer-point buffer) end))
      ((visual-line-p buffer)
       (unless (same-line-p (buffer-mark buffer) start)
         (setf (buffer-mark buffer) start))
       (unless (same-line-p end (buffer-point buffer))
         (move-point (buffer-point buffer) end))))))

(defun apply-visual-range (function &optional (buffer (current-buffer)))
  (if (visual-line-p buffer)
      (apply function (visual-range buffer))
      (progn
        (dolist (ov (sort (visual-overlays buffer) #'point< :key #'overlay-start))
          (funcall function
                   (overlay-start ov)
                   (overlay-end ov))))))

(defun string-without-escape ()
  (concatenate 'string
               (loop for key-char = (key-to-char (read-key))
                     while (char/= #\Escape key-char)
                     collect key-char)))

(define-command vi-visual-append (&optional (buffer (current-buffer))) ()
  (when (visual-block-p buffer)
    (let ((str (string-without-escape))
          (max-end (apply #'max (mapcar (lambda (ov)
                                          (point-charpos (overlay-end ov)))
                                        (visual-overlays buffer)))))
      (apply-visual-range (lambda (start end)
                            (unless (point< start end)
                              (rotatef start end))
                            (let* ((space-len (- max-end (point-charpos end)))
                                   (spaces (make-string space-len
                                                        :initial-element #\Space)))
                              (insert-string end (concatenate 'string
                                                              spaces
                                                              str))))
                          buffer))
    (vi-visual-end)))

(define-command vi-visual-insert (&optional (buffer (current-buffer))) ()
  (when (visual-block-p buffer)
    (let ((str (string-without-escape)))
      (apply-visual-range (lambda (start end)
                            (unless (point< start end)
                              (rotatef start end))
                            (insert-string start str))
                          buffer))
    (vi-visual-end)))

(define-command vi-visual-swap-points (&optional (buffer (current-buffer))) ()
  (with-point ((start (buffer-mark buffer)))
    (setf (buffer-mark buffer) (buffer-point buffer))
    (move-point (buffer-point buffer) start)))

(define-command vi-visual-opposite-side (&optional (buffer (current-buffer))) ()
  (if (visual-block-p buffer)
      (let ((start-col (point-charpos (buffer-mark buffer)))
            (end-col (point-charpos (buffer-point buffer))))
        (move-to-column (buffer-mark buffer) end-col)
        (move-to-column (buffer-point buffer) start-col))
      (vi-visual-swap-points)))

(defmethod check-marked-using-global-mode ((global-mode vi-mode) buffer)
  (unless (buffer-mark buffer)
    (editor-error "Not mark in this buffer")))

(defmethod set-region-point-using-global-mode ((global-mode vi-mode) (start point) (end point))
  (declare (ignore global-mode))
  (let ((buffer (current-buffer)))
    (when (visual-p buffer)
      (let ((v-range (visual-range buffer)))
        (move-point start (car v-range))
        (move-point end (cadr v-range))))))

(defmethod region-beginning-using-global-mode ((global-mode vi-mode)
                                               &optional (buffer (current-buffer)))
  (if (visual-p buffer)
      (car (visual-range buffer))
      (editor-error "Not in visual mode")))

(defmethod region-end-using-global-mode ((global-mode vi-mode)
                                         &optional (buffer (current-buffer)))
  (if (visual-p buffer)
      (cadr (visual-range buffer))
      (editor-error "Not in visual mode")))

(defun enable-visual-from-hook (buffer)
  (unless (visual-p buffer)
    (enable-visual 'visual-char buffer)))

(defun disable-visual-from-hook(buffer)
  (setf (buffer-state buffer) 'normal))

(defun visual-enable-hook ()
  (add-hook *buffer-mark-activate-hook* 'enable-visual-from-hook)
  (add-hook *buffer-mark-deactivate-hook* 'disable-visual-from-hook))

(defun visual-disable-hook ()
  (remove-hook *buffer-mark-activate-hook* 'enable-visual-from-hook)
  (remove-hook *buffer-mark-deactivate-hook* 'disable-visual-from-hook))

(add-hook *enable-hook* 'visual-enable-hook)
(add-hook *disable-hook* 'visual-disable-hook)

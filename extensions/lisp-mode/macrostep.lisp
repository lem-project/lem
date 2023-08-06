(defpackage :lem-lisp-mode/macrostep
  (:use :cl
        :alexandria
        :lem
        :lem-lisp-mode/internal)
  #+sbcl
  (:lock t))
(in-package :lem-lisp-mode/macrostep)

(define-attribute expand-attribute
  (t :background :base01))

(define-attribute subform-attribute
  (t :underline t :bold t))

(define-minor-mode macrostep-mode
    (:name "Macrostep"
     :keymap *macrostep-mode-keymap*
     :enable-hook 'enable-macrostep
     :disable-hook 'disable-macrostep))

(define-key *lisp-mode-keymap* "C-c Return" 'lisp-macrostep-expand)
(define-key *macrostep-mode-keymap* "q" 'lisp-macrostep-quit)
(define-key *macrostep-mode-keymap* "Tab" 'lisp-macrostep-next)
(define-key *macrostep-mode-keymap* "Shift-Tab" 'lisp-macrostep-previous)
(define-key *macrostep-mode-keymap* "Return" 'lisp-macrostep-expand-next)
(define-key *macrostep-mode-keymap* "Backspace" 'lisp-macrostep-undo)

(defun enable-macrostep ()
  (setf (buffer-read-only-p (current-buffer)) t))

(defun disable-macrostep ()
  (setf (buffer-read-only-p (current-buffer)) nil)
  (clear-macrostep-overlays (current-buffer))
  (clear-expanded-overlays (current-buffer))
  (loop :while (pop-undo (current-buffer))))

(defun subform-overlays (buffer)
  (buffer-value buffer 'subform-overlays))

(defun (setf subform-overlays) (value buffer)
  (setf (buffer-value buffer 'subform-overlays) value))

(defun clear-macrostep-overlays (buffer)
  (map () #'delete-overlay (subform-overlays buffer))
  (setf (subform-overlays buffer) '()))

(defun add-subform-overlay (buffer overlay)
  (push overlay (subform-overlays buffer)))

(defun expanded-overlays (buffer)
  (buffer-value buffer 'expanded-overlays))

(defun (setf expanded-overlays) (value buffer)
  (setf (buffer-value buffer 'expanded-overlays) value))

(defun add-expanded-overlay (buffer overlay)
  (push overlay (expanded-overlays buffer)))

(defun clear-expanded-overlays (buffer)
  (map () #'delete-overlay (expanded-overlays buffer)))

(defun make-subform-overlay (start end)
  (let ((overlay (make-overlay start end 'subform-attribute)))
    (overlay-put overlay :subform t)
    overlay))

(defun get-sorted-subform-overlays (buffer)
  (sort (remove-if-not (lambda (overlay)
                         (overlay-get overlay :subform))
                       (subform-overlays buffer))
        #'point<
        :key #'overlay-start))

(defun search-next-subform-overlay (point)
  (loop :for overlay :in (get-sorted-subform-overlays (point-buffer point))
        :when (point< point (overlay-start overlay))
        :return overlay))

(defun search-previous-subform-overlay (point)
  (loop :for (overlay next-overlay) :on (get-sorted-subform-overlays (point-buffer point))
        :while next-overlay
        :when (point<= (overlay-end overlay) point (overlay-start next-overlay))
        :return overlay))

(defun remove-overlays-within-points (start end)
  (loop :with buffer := (point-buffer start)
        :for overlay :in (get-sorted-subform-overlays buffer)
        :if (point<= start (overlay-start overlay) (overlay-end overlay) end)
        :collect overlay :into garbage-overlays
        :else
        :collect overlay :into alive-overlays
        :finally (map () #'delete-overlay garbage-overlays)
                 (setf (subform-overlays buffer) alive-overlays)))

(defun dump-subforms (buffer)
  (loop :for overlay :in (subform-overlays buffer)
        :collect (cons (position-at-point (overlay-start overlay))
                       (position-at-point (overlay-end overlay)))))

(defun replace-at-points (start end string)
  (remove-overlays-within-points start end)
  (delete-between-points start end)
  (insert-string start string))

(defun positions-to-points (buffer start-pos end-pos)
  (with-point ((start (buffer-point buffer))
               (end (buffer-point buffer)))
    (move-to-position start start-pos)
    (move-to-position end end-pos)
    (values start end)))

(defun empty-undo-stack-p (buffer)
  (null (buffer-value buffer 'undo)))

(defun pop-undo (buffer)
  (when (buffer-value buffer 'undo)
    (let ((*inhibit-read-only* t))
      (destructuring-bind (start-pos end-pos string subforms)
          (pop (buffer-value buffer 'undo))
        (multiple-value-bind (start end)
            (positions-to-points buffer start-pos end-pos)
          (replace-at-points start end string)
          (loop :for (start-pos . end-pos) :in subforms
                :do (multiple-value-bind (start end)
                        (positions-to-points buffer start-pos end-pos)
                      (add-subform-overlay buffer (make-subform-overlay start end)))))))
    t))

(defun push-undo (start end string subforms)
  (let ((buffer (point-buffer start))
        (start-pos (position-at-point start))
        (end-pos (position-at-point end)))
    (push (list start-pos end-pos string subforms)
          (buffer-value buffer 'undo))))

(defun replace-with-macrostep-expand (start end expansion-string subform-info)
  (let ((*inhibit-read-only* t)
        (buffer (point-buffer start)))
    (replace-at-points start end expansion-string)
    (add-expanded-overlay buffer (make-overlay start end 'expand-attribute))
    (loop :for (name kind offset) :in subform-info
          :do (with-point ((point start))
                (character-offset point offset)
                (assert (scan-lists point 1 -1 t))
                (with-point ((start point)
                             (end point))
                  (when (form-offset end 1)
                    (add-subform-overlay buffer (make-subform-overlay start end))))))
    (indent-points start end)))

(defun get-form-points (point)
  (maybe-beginning-of-string point)
  (unless (syntax-open-paren-char-p (character-at point))
    (scan-lists point -1 1))
  (values point
          (form-offset (copy-point point :temporary) 1)))

(defmacro with-form-points ((start end point) &body body)
  (check-type start symbol)
  (check-type end symbol)
  `(multiple-value-bind (,start ,end) (get-form-points ,point)
     (with-point ((,start ,start :right-inserting)
                  (,end ,end :left-inserting))
       ,@body)))

(defun get-context (point)
  (with-point ((start point)
               (end point))
    (loop :while (scan-lists start -1 1 t))
    (form-offset (move-point end start) 1)
    (list (points-to-string start point)
          (points-to-string point end))))

(defun macrostep-expand (point)
  (with-form-points (start end point)
    (let ((string (points-to-string start end))
          (context (get-context point)))
      (destructuring-ecase
          (lisp-eval `(micros/macrostep:macrostep-expand-1 ,string t ',context))
        ((:ok expansion-string subform-info)
         (let ((subforms (dump-subforms (point-buffer point))))
           (replace-with-macrostep-expand start end expansion-string subform-info)
           (push-undo start end string subforms))
         (move-point point start))
        ((:error message)
         (show-message (format nil "Error: ~A" message)))))))

(define-command lisp-macrostep-expand () ()
  (macrostep-expand (current-point))
  (macrostep-mode t))

(define-command lisp-macrostep-quit () ()
  (when (mode-active-p (current-buffer) 'macrostep-mode)
    (macrostep-mode nil)))

(define-command lisp-macrostep-next () ()
  (when (mode-active-p (current-buffer) 'macrostep-mode)
    (when-let (overlay (search-next-subform-overlay (current-point)))
      (move-point (current-point) (overlay-start overlay)))))

(define-command lisp-macrostep-previous () ()
  (when (mode-active-p (current-buffer) 'macrostep-mode)
    (when-let (overlay (search-previous-subform-overlay (current-point)))
      (move-point (current-point) (overlay-start overlay)))))

(define-command lisp-macrostep-expand-next () ()
  (when (mode-active-p (current-buffer) 'macrostep-mode)
    (macrostep-expand (current-point))))

(define-command lisp-macrostep-undo () ()
  (when (mode-active-p (current-buffer) 'macrostep-mode)
    (pop-undo (current-buffer))
    (when (empty-undo-stack-p (current-buffer))
      (macrostep-mode nil))))

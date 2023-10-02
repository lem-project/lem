(in-package :lem-core)

(defgeneric compute-left-display-area-content (mode buffer point)
  (:method (mode buffer point) nil))

(defun call-with-display-error (function)
  (handler-bind ((error (lambda (e)
                          (log:error "~A"
                                     (with-output-to-string (out)
                                       (format out "~A~%" e)
                                       (uiop:print-backtrace :stream out :condition e)))
                          (message "~A" e)
                          (return-from call-with-display-error))))
    (funcall function)))

(defmacro with-display-error (() &body body)
  `(call-with-display-error (lambda () ,@body)))

;;; highlight-line
(define-editor-variable highlight-line nil)

(defun highlight-line-color ()
  (when (background-color)
    (let ((color (parse-color (background-color))))
      (multiple-value-bind (h s v)
          (rgb-to-hsv (color-red color)
                      (color-green color)
                      (color-blue color))
        (multiple-value-bind (r g b)
            (hsv-to-rgb h
                        s
                        (max 0 (- v 2)))
          (format nil "#~2,'0X~2,'0X~2,'0X" r g b))))))

;;; overlays
(defun make-temporary-highlight-line-overlay (buffer)
  (when (and (variable-value 'highlight-line :default (current-buffer))
             (current-theme))
    (alexandria:when-let ((color (highlight-line-color)))
      (make-overlay-line (buffer-point buffer)
                         (make-attribute :background color)
                         :temporary t))))

(defun make-temporary-region-overlay-from-cursor (cursor)
  (let ((mark (cursor-mark cursor)))
    (when (mark-active-p mark)
      (make-overlay cursor
                    (mark-point mark)
                    'region
                    :temporary t))))

(defun make-cursor-overlay (point)
  (make-overlay-cursor
   point
   (if (typep point 'fake-cursor)
       'fake-cursor
       'cursor)))

(defun get-window-overlays (window)
  (let* ((buffer (window-buffer window))
         (overlays (buffer-overlays buffer)))
    (when (eq (current-window) window)
      (dolist (cursor (buffer-cursors buffer))
        (if-push (make-temporary-region-overlay-from-cursor cursor)
                 overlays))
      (if-push (make-temporary-highlight-line-overlay buffer)
               overlays))
    (if (and (eq window (current-window))
             (not (window-cursor-invisible-p window)))
        (append overlays
                (mapcar #'make-cursor-overlay
                        (buffer-cursors (window-buffer window))))
        overlays)))

(defun overlay-attributes (under-attributes over-start over-end over-attribute)
  ;; under-attributes := ((start-charpos end-charpos attribute) ...)
  (let* ((over-attribute (ensure-attribute over-attribute))
         (under-part-attributes (lem-base::subseq-elements under-attributes
                                                           over-start
                                                           over-end))
         (merged-attributes (lem-base::remove-elements under-attributes
                                                       over-start
                                                       over-end)))
    (flet ((add-element (start end attribute)
             (when (< start end)
               (push (list start end (ensure-attribute attribute))
                     merged-attributes))))
      (if (null under-part-attributes)
          (add-element over-start over-end over-attribute)
          (loop :for prev-under := 0 :then under-end-offset
                :for (under-start-offset under-end-offset under-attribute)
                :in under-part-attributes
                :do (add-element (+ over-start prev-under)
                                 (+ over-start under-start-offset)
                                 over-attribute)
                    (add-element (+ over-start under-start-offset)
                                 (+ over-start under-end-offset)
                                 (alexandria:if-let (under-attribute
                                                     (ensure-attribute under-attribute nil))
                                   (merge-attribute under-attribute
                                                    over-attribute)
                                   over-attribute))
                :finally (add-element (+ over-start under-end-offset)
                                      over-end
                                      over-attribute))))
    (lem-base::normalization-elements merged-attributes)))

;;; background color
(defvar *inactive-window-background-color* nil)

(defun get-background-color-of-window (window)
  (cond ((typep window 'floating-window)
         (floating-window-background-color window))
        ((eq window (current-window))
         nil)
        ((eq window (window-parent (current-window)))
         nil)
        ((and *inactive-window-background-color*
              (eq 'window (type-of window)))
         *inactive-window-background-color*)
        (t nil)))

;;; redraw-buffer
(defgeneric redraw-buffer (implementation buffer window force))

(defmethod redraw-buffer :around (implementation buffer window force)
  (with-display-error ()
    (lem-if:redraw-view-before (implementation)
                               (screen-view (window-screen window)))
    (let ((lem-if:*background-color-of-drawing-window*
            (get-background-color-of-window window)))
      (call-next-method))
    (when (window-use-modeline-p window)
      (redraw-modeline window (or (screen-modified-p (window-screen window))
                                  force)))
    (lem-if:redraw-view-after (implementation)
                              (screen-view (window-screen window)))))

(defun redraw-modeline (window force)
  (when (window-use-modeline-p window)
    (let* ((screen (window-screen window))
           (view (screen-view screen))
           (default-attribute (if (eq window (current-window))
                                  'modeline
                                  'modeline-inactive))
           (elements '())
           (left-x 0)
           (right-x (window-width window)))
      (modeline-apply window
                      (lambda (string attribute alignment)
                        (case alignment
                          ((:right)
                           (decf right-x (length string))
                           (push (list right-x string attribute) elements))
                          (otherwise
                           (push (list left-x string attribute) elements)
                           (incf left-x (length string)))))
                      default-attribute)
      (setf elements (nreverse elements))
      (when (or force (not (equal elements (screen-modeline-elements screen))))
        (setf (screen-modeline-elements screen) elements)
        (lem-if:print-modeline (implementation) view 0 0
                               (make-string (window-width window) :initial-element #\space)
                               default-attribute)
        (loop :for (x string attribute) :in elements
              :do (lem-if:print-modeline (implementation) view x 0 string attribute))))))

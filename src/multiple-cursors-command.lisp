(in-package :lem)

(defun process-each-cursors (function)
  (let ((buffer (current-buffer)))
    (dolist (point (sort (copy-list (buffer-fake-cursors buffer)) #'point<))
      (lem-base::with-buffer-point (buffer point)
        (with-current-killring (fake-cursor-killring point)
          (handler-case
              (save-continue-flags
                (funcall function))
            (move-cursor-error ())))))
    (funcall function)))

(defmacro do-each-cursors (() &body body)
  `(process-each-cursors (lambda () ,@body)))

(defmethod execute :around (mode
                            (command movable-advice)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command editable-advice)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command jump-cursor-advice)
                            argument)
  (prog1 (call-next-method)
    (clear-cursors (current-buffer))))

(defmethod execute :around (mode
                            (command delete-previous-char)
                            argument)
  (cond ((mark-active-p (cursor-mark (current-point)))
         (do-each-cursors ()
           (delete-cursor-region (current-point))))
        (t
         (do-each-cursors ()
           (delete-previous-char-1 argument)))))

(defmethod execute :around (mode
                            (command copy-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (copy-cursor-region (current-point))))

(defmethod execute :around (mode
                            (command kill-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (kill-cursor-region (current-point))))

(defmethod execute :around (mode
                            (command yank)
                            argument)
  (let ((*enable-clipboard-p* (and (enable-clipboard-p)
                                   (null (buffer-fake-cursors (current-buffer))))))
    (process-each-cursors #'call-next-method)))

(defmethod execute :around (mode
                            (command mark-set)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command exchange-point-mark)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command quoted-insert)
                            argument)
  (let* ((key (read-key))
         (char (or (key-to-char key) (code-char 0))))
    (do-each-cursors ()
      (self-insert-aux char (or argument 1)))))

(defmethod execute :around (mode
                            (command mark-sexp)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command downcase-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (downcase-cursor-region (current-point))))

(defmethod execute :around (mode
                            (command uppercase-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (uppercase-cursor-region (current-point))))

(define-command add-cursors-to-next-line () ()
  (let ((cursors (buffer-cursors (current-buffer))))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (when (and (line-offset p 1 (point-charpos p))
                           (or (null next-cursor)
                               (not (same-line-p p next-cursor))))
                  (make-fake-cursor p))))))

(defun clear-duplicate-cursors (buffer)
  (loop :for (cursor next-cursor) :on (buffer-cursors buffer)
        :when (and next-cursor (same-line-p cursor next-cursor))
        :do (delete-fake-cursor
             (if (eq cursor (buffer-point buffer))
                 next-cursor
                 cursor))))

(define-condition garbage-collection-cursors (after-executing-command) ())
(defmethod handle-signal ((condition garbage-collection-cursors))
  (clear-duplicate-cursors (current-buffer)))

(define-condition clear-cursor-when-aborted (editor-abort-handler) ())
(defmethod handle-signal ((condition clear-cursor-when-aborted))
  (let ((string (merge-cursor-killrings (current-buffer))))
    (clear-cursors (current-buffer))
    (copy-to-clipboard-with-killring string)))

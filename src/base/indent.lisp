(in-package :lem-base)

(export '(indent-line
          indent-tabs-mode
          calc-indent-function))

(define-editor-variable indent-tabs-mode nil)
(define-editor-variable calc-indent-function 'calc-indent-default)

(defun indent-line-1 (point column)
  (when (null column)
    (return-from indent-line-1 t))
  (when (minusp column)
    (setf column 0))
  (let ((old-column (point-column point))
        (old-indent-string
         (points-to-string (line-start (copy-point point :temporary))
                           (with-point ((point point))
                             (skip-whitespace-forward (line-start point) t)
                             point)))
        (new-indent-string
         (if (value 'indent-tabs-mode :buffer (point-buffer point))
             (multiple-value-bind (div mod)
                 (floor column (tab-size))
               (concatenate 'string
                            (make-string div :initial-element #\tab)
                            (make-string mod :initial-element #\space)))
             (make-string column :initial-element #\space))))
    (cond ((string/= old-indent-string new-indent-string)
           (line-start point)
           (delete-character point (length old-indent-string))
           (insert-string point new-indent-string)
           (if (< old-column column)
               (skip-whitespace-forward (line-start point) t)
               (move-to-column point
                               (max 0 (+ old-column
                                         (- (string-width new-indent-string)
                                            (string-width old-indent-string)))))))
          ((< old-column column)
           (skip-whitespace-forward (line-start point) t))))
  t)

(defun calc-indent-default (point)
  (with-point ((point point))
    (cond ((line-offset point -1)
           (skip-whitespace-forward point t)
           (point-column point))
          (t 0))))

(defun indent-line (point)
  (indent-line-1 point
                 (funcall (value 'calc-indent-function :buffer (point-buffer point))
                          point)))

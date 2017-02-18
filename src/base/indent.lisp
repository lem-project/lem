(in-package :lem-base)

(export '(back-to-indentation
          indent-line
          indent-tabs-mode
          calc-indent-function))

(define-editor-variable indent-tabs-mode nil)
(define-editor-variable calc-indent-function 'calc-indent-default)

(defun back-to-indentation (point)
  (skip-whitespace-forward (line-start point) t)
  point)

(defun indent-line-1 (point column)
  (when (null column)
    (return-from indent-line-1 t))
  (when (minusp column)
    (setf column 0))
  (let ((old-column (point-column point))
        (old-indent-string
         (with-point ((start point)
                      (end point))
           (points-to-string (line-start start)
                             (back-to-indentation end))))
        (new-indent-string
         (if (variable-value 'indent-tabs-mode :buffer point)
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
               (back-to-indentation point)
               (move-to-column point
                               (max 0 (+ old-column
                                         (- (string-width new-indent-string)
                                            (string-width old-indent-string)))))))
          ((< old-column column)
           (back-to-indentation point))))
  t)

(defun calc-indent-default (point)
  (cond ((line-offset point -1)
         (back-to-indentation point)
         (point-column point))
        (t 0)))

(defun indent-line (point)
  (let ((column (funcall (or (variable-value 'calc-indent-function :buffer point)
                             'calc-indent-default)
                         (copy-point point :temporary))))
    (indent-line-1 point column)))

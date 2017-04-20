(in-package :lem-lisp-mode)

(defstruct button
  start
  end
  plist)

(defun button-get (button key &optional default)
  (getf (button-plist button) key default))

(defun insert-button (point text &optional callback &rest plist)
  (insert-string point text
                 'button (make-button :plist plist)
                 'action callback
                 :attribute (getf plist :attribute)))

(defun forward-button (point &optional limit)
  (when (text-property-at point 'button)
    (next-single-property-change point 'button))
  (next-single-property-change point 'button limit))

(defun backward-button (point &optional limit)
  (when (text-property-at point 'button -1)
    (previous-single-property-change point 'button))
  (previous-single-property-change point 'button limit))

(defun move-to-button-start (point)
  (cond ((text-property-at point 'button -1)
         (previous-single-property-change point 'button))
        ((text-property-at point 'button 0)
         point)))

(defun move-to-button-end (point)
  (when (text-property-at point 'button)
    (next-single-property-change point 'button)))

(defun button-at (point)
  (let ((button (text-property-at point 'button)))
    (when button
      (with-point ((point point))
        (setf (button-start button)
              (move-to-button-start point))
        (setf (button-end button)
              (move-to-button-end point))
        button))))

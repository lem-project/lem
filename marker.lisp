(in-package :lem)

(export '(make-marker
          make-marker-current-point
          marker-p
          delete-marker
          marker-buffer
          marker-linum
          marker-column
          marker-point
          marker-insertion-type))

(deftype marker ()
  `(satisfies markerp))

(let ((marker-tag (gensym "MARKER")))
  (defun marker-p (x)
    (and (vectorp x)
         (eq marker-tag (safe-aref x 0))))
  (defun make-marker (buffer point &optional insertion-type)
    (let ((marker (vector marker-tag
                          buffer
                          (point-linum point)
                          (point-column point)
                          insertion-type)))
      (buffer-add-marker buffer marker)
      marker)))

(defun make-marker-current-point (&optional insertion-type)
  (make-marker (window-buffer) (current-point) insertion-type))

(defun delete-marker (marker)
  (buffer-delete-marker
   (marker-buffer marker)
   marker))

(defun marker-buffer (marker)
  (aref marker 1))

(defun marker-linum (marker)
  (aref marker 2))

(defun (setf marker-linum) (new-linum marker)
  (setf (aref marker 2) new-linum))

(defun marker-column (marker)
  (aref marker 3))

(defun (setf marker-column) (new-column marker)
  (setf (aref marker 3) new-column))

(defun marker-insertion-type (marker)
  (aref marker 4))

(defun (setf marker-insertion-type) (new-insertion-type marker)
  (setf (aref marker 4) new-insertion-type))

(defun marker-point (marker)
  (make-point (marker-linum marker)
              (marker-column marker)))

(defun (setf marker-point) (new-point marker)
  (setf (marker-linum marker) (point-linum new-point)
        (marker-column marker) (point-column new-point))
  new-point)

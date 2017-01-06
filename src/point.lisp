(in-package :lem)

(export '(current-point
          pointp
          copy-point
          delete-point
          point-buffer
          point-charpos
          point-kind

          point=
          point/=
          point<
          point<=
          point>
          point>=))

(defclass point ()
  ((buffer
    :initarg :buffer
    :accessor point-buffer
    :type buffer)
   (line
    :initarg :line
    :accessor point-line
    :type line)
   (charpos
    :initarg :charpos
    :accessor point-charpos
    :type fixnum)
   (kind
    :initarg :kind
    :accessor point-kind
    :type (member :temporary :left-inserting :right-inserting))
   (name
    :initarg :name
    :accessor point-name
    :type (or null string))))

(defun current-point ()
  (buffer-point (current-buffer)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "POINT ~A ~A ~S"
            (point-name object)
            (point-charpos object)
            (line-str (point-line object)))))

(defun pointp (x)
  (typep x 'point))

(defun make-point (buffer line charpos &key (kind :right-inserting) name)
  (check-type kind (member :temporary :left-inserting :right-inserting))
  (let ((point (make-instance 'point
                              :buffer buffer
                              :line line
                              :charpos charpos
                              :kind kind
                              :name name)))
    (unless (eq :temporary kind)
      (push point (line-points line))
      (buffer-add-point buffer point))
    point))

(defun copy-point (point &optional kind name)
  (make-point (point-buffer point)
              (point-line point)
	      (point-charpos point)
	      :kind (or kind (point-kind point))
	      :name (or name (point-name point))))

(defun delete-point (point)
  (unless (point-temporary-p point)
    (setf (line-points (point-line point))
          (delete point (line-points (point-line point))))
    (buffer-delete-point (point-buffer point)
                         point)))

(defun point-change-line (point new-line)
  (unless (point-temporary-p point)
    (let ((old-line (point-line point)))
      (do ((scan (line-points old-line) (cdr scan))
           (prev nil scan))
          ((eq (car scan) point)
           (if prev
               (setf (cdr prev) (cdr scan))
               (setf (line-points old-line) (cdr scan)))
           (setf (cdr scan) (line-points new-line)
                 (line-points new-line) scan)))))
  (setf (point-line point) new-line))

(defun point-temporary-p (point)
  (eq (point-kind point) :temporary))

(defun point-line-ord (point)
  (line-ord (point-line point)))

(defun point= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (and (= (point-line-ord point1)
          (point-line-ord point2))
       (= (point-charpos point1)
          (point-charpos point2))))

(defun point/= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (not (point= point1 point2)))

(defun point< (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (or (< (point-line-ord point1) (point-line-ord point2))
      (and (= (point-line-ord point1) (point-line-ord point2))
           (< (point-charpos point1) (point-charpos point2)))))

(defun point<= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (or (point< point1 point2)
      (point= point1 point2)))

(defun point> (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (point< point2 point1))

(defun point>= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (point<= point2 point1))

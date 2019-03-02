(defpackage :lem.go-back
  (:use :cl :lem :lem.sourcelist)
  (:export :*max*
           :select-go-back
           :go-back-global
           :go-back)
  #+sbcl
  (:lock t))
(in-package :lem.go-back)

(defparameter *max* 100)

(defstruct record
  locations
  (len 0))

(defvar *global-record* (make-record))

(defun location-name (location) (first location))
(defun location-linum (location) (second location))
(defun location-charpos (location) (third location))

(defun point-to-location (point)
  (list (buffer-name (point-buffer point))
        (line-number-at-point point)
        (point-charpos point)))

(defun equal-location (location1 location2)
  (and (equal (location-name location1)
              (location-name location2))
       (= (location-linum location1)
          (location-linum location2))))

(defun append-location (record location &optional tail)
  (when (if tail
            (equal-location location
                            (car (last (record-locations record))))
            (equal-location location
                            (car (record-locations record))))
    (return-from append-location))
  (if (<= *max* (record-len record))
      (progn
        (setf (record-locations record)
              (nbutlast (record-locations record)
                        (1+ (- (record-len record) *max*))))
        (setf (record-len record) *max*))
      (incf (record-len record)))
  (if tail
      (alexandria:nconcf (record-locations record)
                         (list location))
      (push location (record-locations record))))

(define-command select-go-back () ()
  (with-sourcelist (sourcelist "*select-locations*" :focus t)
    (loop :for (name linum charpos) :in (remove-duplicates
                                         (record-locations *global-record*)
                                         :test #'equal-location)
          :for buffer := (get-buffer name)
          :when buffer
          :do (let ((name name)
                    (linum linum)
                    (charpos charpos)
                    (filename (or (buffer-filename buffer) name))
                    (linestr
                      (with-point ((p (buffer-start-point buffer)))
                        (move-to-line p linum)
                        (line-string p))))
                (append-sourcelist
                 sourcelist
                 (lambda (point)
                   (insert-string point filename :attribute 'lem.sourcelist:title-attribute)
                   (insert-string point ":")
                   (insert-string point (princ-to-string linum)
                                  :attribute 'lem.sourcelist:position-attribute)
                   (insert-string point ":")
                   (insert-string point linestr))
                 (lambda (set-buffer-fn)
                   (let ((buffer (get-buffer name)))
                     (unless buffer (editor-error "No such buffer: ~A" name))
                     (move-to-line (buffer-point buffer) linum)
                     (line-offset (buffer-point buffer) 0 charpos)
                     (funcall set-buffer-fn buffer))))))))

(defun go-back-internal (record n)
  (when (plusp n)
    (loop :while (record-locations record)
          :for location := (progn
                             (decf (record-len record))
                             (pop (record-locations record)))
          :for (buffer-name line-number charpos) := location
          :do (alexandria:when-let ((buffer (get-buffer buffer-name)))
                (record-location (current-point) t)
                (incf (record-len record))
                (alexandria:nconcf (record-locations record) (list location))
                (when (zerop (decf n))
                  (switch-to-buffer buffer)
                  (let ((p (buffer-point buffer)))
                    (move-to-line p line-number)
                    (line-offset p 0 charpos))
                  (return))))))

(defun buffer-record (buffer)
  (unless (buffer-value buffer 'record)
    (setf (buffer-value buffer 'record) (make-record)))
  (buffer-value buffer 'record))

(define-command go-back-global (n) ("p")
  (go-back-internal *global-record* n))

(define-command go-back (n) ("p")
  (go-back-internal (buffer-record (current-buffer)) n))

(defun record-location (point &optional tail)
  (let ((location (point-to-location point)))
    (append-location *global-record* location tail)
    (append-location (buffer-record point) location tail)))

(add-hook *set-location-hook* 'record-location)

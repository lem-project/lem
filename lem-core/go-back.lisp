(defpackage :lem.go-back
  (:use :cl :lem :lem.sourcelist)
  (:export :select-go-back
           :go-back
           :go-forward))
(in-package :lem.go-back)

(defvar *record-locations* '())
(defvar *len* 0)
(defvar *max* 100)

(defun elt-name (elt) (first elt))
(defun elt-linum (elt) (second elt))
(defun elt-charpos (elt) (third elt))

(defun point-to-elt (point)
  (list (buffer-name (point-buffer point))
        (line-number-at-point point)
        (point-charpos point)))

(defun equal-location (elt1 elt2)
  (and (equal (elt-name elt1)
              (elt-name elt2))
       (= (elt-linum elt1)
          (elt-linum elt2))))

(defun record-location (point &optional tail)
  (let ((new-elt (point-to-elt point)))
    (when (if tail
              (equal-location new-elt
                              (car (last *record-locations*)))
              (equal-location new-elt
                              (car *record-locations*)))
      (return-from record-location))
    (if (<= *max* *len*)
        (progn
          (setf *record-locations* (nbutlast *record-locations* (1+ (- *len* *max*))))
          (setf *len* *max*))
        (incf *len*))
    (if tail
        (alexandria:nconcf *record-locations* (list new-elt))
        (push new-elt *record-locations*))))

(define-command select-go-back () ()
  (with-sourcelist (sourcelist "*select-locations*" :focus t)
    (loop :for (name linum charpos) :in *record-locations*
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
                   (insert-string point filename :attribute 'lem.grep:title-attribute)
                   (insert-string point ":")
                   (insert-string point (princ-to-string linum)
                                  :attribute 'lem.grep:position-attribute)
                   (insert-string point ":")
                   (insert-string point linestr))
                 (lambda (set-buffer-fn)
                   (let ((buffer (get-buffer name)))
                     (unless buffer (editor-error "No such buffer: ~A" name))
                     (move-to-line (buffer-point buffer) linum)
                     (line-offset (buffer-point buffer) 0 charpos)
                     (funcall set-buffer-fn buffer))))))))

(define-command go-back (n) ("p")
  (when (plusp n)
    (loop :while *record-locations*
          :for elt := (progn
                        (decf *len*)
                        (pop *record-locations*))
          :for (buffer-name line-number charpos) := elt
          :do (alexandria:when-let ((buffer (get-buffer buffer-name)))
                (record-location (current-point) t)
                (incf *len*)
                (alexandria:nconcf *record-locations* (list elt))
                (when (zerop (decf n))
                  (switch-to-buffer buffer)
                  (let ((p (buffer-point buffer)))
                    (move-to-line p line-number)
                    (line-offset p 0 charpos))
                  (return))))))

(add-hook *set-location-hook* 'record-location)

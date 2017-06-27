(defpackage :lem.go-back
  (:use :cl :lem :lem.sourcelist)
  (:export :select-go-back
           :go-back
           :go-forward))
(in-package :lem.go-back)

(defvar *record-locations* '())

(defun elt-name (elt) (first elt))
(defun elt-linum (elt) (second elt))
(defun elt-charpos (elt) (third elt))

(defun record-location (point)
  (loop :for (name linum) :in *record-locations*
        :do (when (and (equal name (buffer-name (point-buffer point)))
                       (= linum (line-number-at-point point)))
              (return-from record-location nil)))
  (let ((buffer (point-buffer point)))
    (push (list (buffer-name buffer)
                (line-number-at-point point)
                (point-charpos point))
          *record-locations*)))

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
          :for elt := (pop *record-locations*)
          :for (buffer-name line-number charpos) := elt
          :do (alexandria:when-let ((buffer (get-buffer buffer-name)))
                (alexandria:nconcf *record-locations* (list elt))
                (when (zerop (decf n))
                  (switch-to-buffer buffer)
                  (let ((p (buffer-point buffer)))
                    (move-to-line p line-number)
                    (line-offset p 0 charpos))
                  (return))))))

(define-command go-forward (n) ("p")
  (alexandria:nreversef *record-locations*)
  (go-back n)
  (alexandria:nreversef *record-locations*))

(add-hook *set-location-hook* 'record-location)

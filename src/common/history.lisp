(defpackage :lem/common/history
  (:use :cl)
  (:export :make-history
           :save-file
           :last-history
           :add-history
           :previous-history
           :next-history
           :previous-matching
           :backup-edit-string
           :restore-edit-string)
  #+sbcl
  (:lock t))
(in-package :lem/common/history)

(defstruct (history (:constructor %make-history))
  pathname
  data
  index
  edit-string)

(defun require-additions-to-history-p (input last-input)
  (and (not (equal input last-input))
       (not (equal input ""))))

(defun make-history (&key pathname)
  (let* ((initial-contents
           (when (and pathname (uiop:file-exists-p pathname))
             (uiop:read-file-form pathname)))
         (num-contents (length initial-contents)))
    (%make-history
     :pathname pathname
     :data (make-array num-contents :fill-pointer num-contents :adjustable t :initial-contents initial-contents)
     :index num-contents)))

(defun save-file (history)
  (ensure-directories-exist (history-pathname history))
  (with-open-file (stream (history-pathname history)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (print (coerce (history-data history) 'list) stream)))

(defun last-history (history)
  (when (< 0 (length (history-data history)))
    (aref (history-data history)
          (1- (length (history-data history))))))

(defun add-history (history input)
  (when (require-additions-to-history-p
         input
         (last-history history))
    (vector-push-extend input (history-data history)))
  (setf (history-index history)
        (length (history-data history)))
  input)

(defun previous-history (history)
  (when (< 0 (history-index history))
    (values (aref (history-data history)
                  (decf (history-index history)))
            t)))

(defun next-history (history)
  (when (< (history-index history)
           (1- (length (history-data history))))
    (values (aref (history-data history)
                  (incf (history-index history)))
            t)))

(defun previous-matching (history regexp)
  (loop :for i :downfrom (1- (history-index history)) :to 0
        :do (when (ppcre:scan regexp (aref (history-data history) i))
              (setf (history-index history) i)
              (return (values (aref (history-data history) i)
                              t)))))

(defun backup-edit-string (history input)
  (when (or (>= (history-index history)
                (length (history-data history)))
            (not (equal input
                        (aref (history-data history)
                              (history-index history)))))
    (setf (history-edit-string history) input)
    (setf (history-index history) (length (history-data history)))))

(defun restore-edit-string (history)
  (when (= (history-index history)
           (1- (length (history-data history))))
    (setf (history-index history) (length (history-data history)))
    (values (history-edit-string history)
            t)))

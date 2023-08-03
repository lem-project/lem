(defpackage :lem/loading-spinner
  (:use :cl :lem :alexandria)
  (:export :spinner-value
           :start-loading-spinner
           :stop-loading-spinner
           :with-line-spinner-points
           :get-line-spinners)
  #+sbcl
  (:lock t))
(in-package :lem/loading-spinner)

(defconstant +loading-interval+ 80)

(define-attribute spinner-attribute
  (t :foreground :base0A))

(defclass spinner ()
  ((frames :initform #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
           :reader spinner-frames)
   (frame-index :initform 0
                :accessor spinner-frame-index)
   (timer :initarg :timer
          :reader spinner-timer)
   (loading-message :initarg :loading-message
                    :reader spinner-loading-message)
   (variables :initform (make-hash-table :test 'equal)
              :reader spinner-variables)))

(defvar *spinner-table* (make-hash-table))

(defun buffer-spinner (buffer)
  (buffer-value buffer 'spinner))

(defun (setf buffer-spinner) (spinner buffer)
  (setf (buffer-value buffer 'spinner) spinner))

(defun spinner-value (spinner name &optional default)
  (multiple-value-bind (value foundp)
      (gethash name (spinner-variables spinner))
    (if foundp value default)))

(defun (setf spinner-value) (value spinner name &optional default)
  (declare (ignore default))
  (setf (gethash name (spinner-variables spinner)) value))

(defun spinner-character (spinner)
  (elt (spinner-frames spinner)
       (spinner-frame-index spinner)))

(defun spinner-text (spinner)
  (format nil "~A ~A"
          (spinner-character spinner)
          (or (spinner-loading-message spinner) "")))

(defmethod convert-modeline-element ((spinner spinner) window)
  (let ((attribute (merge-attribute (ensure-attribute 'modeline t)
                                    (ensure-attribute 'spinner-attribute))))
    (values (format nil "~A  " (spinner-text spinner))
            attribute
            :right)))

(defun update-spinner-frame (spinner)
  (setf (spinner-frame-index spinner)
        (mod (1+ (spinner-frame-index spinner))
             (length (spinner-frames spinner)))))

(defgeneric start-loading-spinner (type &key &allow-other-keys))
(defgeneric stop-loading-spinner (spinner))

(defclass modeline-spinner (spinner)
  ((buffer :initarg :buffer
           :reader modeline-spinner-buffer)))

(defmethod start-loading-spinner ((type (eql :modeline)) &key buffer loading-message)
  (check-type buffer buffer)
  (unless (buffer-spinner buffer)
    (let* ((spinner)
           (timer (start-timer (make-timer (lambda () (update-spinner-frame spinner)))
                               +loading-interval+
                               t)))
      (setf spinner
            (make-instance 'modeline-spinner
                           :timer timer
                           :loading-message loading-message
                           :buffer buffer))
      (modeline-add-status-list spinner buffer)
      (setf (buffer-spinner buffer) spinner)
      spinner)))

(defmethod stop-loading-spinner ((spinner modeline-spinner))
  (let ((buffer (modeline-spinner-buffer spinner)))
    (modeline-remove-status-list spinner buffer)
    (stop-timer (spinner-timer spinner))
    (setf (buffer-spinner buffer) nil))
  (values))

(defclass line-spinner (spinner)
  ((overlay :initarg :overlay
            :reader line-spinner-overlay)))

(defun update-line-spinner (spinner)
  (update-spinner-frame spinner)
  (overlay-put (line-spinner-overlay spinner)
               :text
               (spinner-text spinner)))

(defmethod start-loading-spinner ((type (eql :line)) &key point loading-message)
  (start-loading-spinner :region
                         :start point
                         :end point
                         :loading-message loading-message))

(defmethod start-loading-spinner ((type (eql :region)) &key start end loading-message)
  (check-type start point)
  (check-type end point)
  (let* ((spinner)
         (timer (start-timer (make-timer 
                              (lambda ()
                                (update-line-spinner spinner)))
                             +loading-interval+
                             t))
         (overlay (make-overlay start end 'spinner-attribute)))
    (setf spinner
          (make-instance 'line-spinner
                         :timer timer
                         :loading-message loading-message
                         :overlay overlay))
    (overlay-put overlay 'line-spinner spinner)
    (overlay-put overlay :display-line-end t)
    (overlay-put overlay :display-line-end-offset 1)
    (overlay-put overlay :text (spinner-text spinner))
    spinner))

(defmethod stop-loading-spinner ((spinner line-spinner))
  (stop-timer (spinner-timer spinner))
  (let ((overlay (line-spinner-overlay spinner)))
    (delete-overlay overlay))
  (values))

(defmacro with-line-spinner-points ((start end spinner) &body body)
  (once-only (spinner)
    (with-unique-names (overlay)
      `(let* ((,overlay (line-spinner-overlay ,spinner))
              (,start (copy-point (overlay-start ,overlay) :temporary))
              (,end (copy-point (overlay-end ,overlay) :temporary)))
         ,@body))))

(defun get-line-spinners (point)
  (loop :for ov :in (point-overlays point)
        :when (overlay-get ov 'line-spinner)
        :collect :it))

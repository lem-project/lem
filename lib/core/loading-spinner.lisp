(defpackage :lem.loading-spinner
  (:use :cl :lem :alexandria)
  (:export :start-loading-spinner
           :stop-loading-spinner))
(in-package :lem.loading-spinner)

(defclass spinner ()
  ((frames :initform #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
           :reader spinner-frames)
   (frame-index :initform 0
                :accessor spinner-frame-index)
   (timer :initarg :timer
          :reader spinner-timer)
   (loading-message :initarg :loading-message
                    :reader spinner-loading-message)))

(defvar *spinner-table* (make-hash-table))

(defun buffer-spinner (buffer)
  (buffer-value buffer 'spinner))

(defun (setf buffer-spinner) (spinner buffer)
  (setf (buffer-value buffer 'spinner) spinner))

(defmethod convert-modeline-element ((spinner spinner) window)
  (let ((attribute (merge-attribute (ensure-attribute 'modeline t)
                                    (make-attribute :foreground "yellow"))))
    (values (format nil
                    "~A ~A  "
                    (elt (spinner-frames spinner)
                         (spinner-frame-index spinner))
                    (or (spinner-loading-message spinner) ""))
            attribute
            :right)))

(defun update-spinner-frame ()
  (when-let ((spinner (buffer-spinner (current-buffer))))
    (setf (spinner-frame-index spinner)
          (mod (1+ (spinner-frame-index spinner))
               (length (spinner-frames spinner))))))

(defun start-loading-spinner (buffer &key loading-message)
  (unless (buffer-spinner buffer)
    (let* ((timer (start-timer 80 t 'update-spinner-frame))
           (spinner (make-instance 'spinner :timer timer :loading-message loading-message)))
      (modeline-add-status-list spinner buffer)
      (setf (buffer-spinner buffer) spinner))))

(defun stop-loading-spinner (buffer)
  (when-let ((spinner (buffer-spinner buffer)))
    (modeline-remove-status-list spinner buffer)
    (stop-timer (spinner-timer spinner))
    (setf (buffer-spinner buffer) nil)))

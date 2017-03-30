(in-package :lem)

(export '(*modeline-default-format*
          modeline-add-status-list
          modeline-remove-status-list
          modeline-write-ifno
          modeline-name
          modeline-major-mode
          modeline-minor-modes
          modeline-linum
          modeline-column))

(defvar *modeline-default-format*
  '(modeline-write-info
    modeline-name
    modeline-mode-names
    modeline-position
    (modeline-posline nil :right)))

(defvar *modeline-status-list* nil)

(defun modeline-add-status-list (x &optional (buffer nil bufferp))
  (if bufferp
      (pushnew x (buffer-value buffer 'modeline-status-list))
      (pushnew x *modeline-status-list*))
  (values))

(defun modeline-remove-status-list (x &optional (buffer nil bufferp))
  (if bufferp
      (setf (buffer-value buffer 'modeline-status-list)
            (remove x (buffer-value buffer 'modeline-status-list)))
      (setf *modeline-status-list*
            (remove x *modeline-status-list*))))

(defun modeline-write-info (window)
  (let ((buffer (window-buffer window)))
    (cond ((buffer-read-only-p buffer)
           " % ")
          ((buffer-modified-p buffer)
           " * ")
          (t
           " - "))))

(defun modeline-name (window)
  (buffer-name (window-buffer window)))

(defun modeline-mode-names (window)
  (with-output-to-string (*standard-output*)
    (princ " (")
    (princ (mode-name (buffer-major-mode (window-buffer window))))
    (let ((firstp t))
      (dolist (x (append (mapcar #'mode-name (buffer-minor-modes (window-buffer window)))
                         (buffer-value (window-buffer window) 'modeline-status-list)
                         *modeline-status-list*))
        (princ " ")
        (if (functionp x)
            (princ (funcall x window))
            (princ x))
        (setf firstp t)))
    (princ ") ")))

(defun modeline-position (window)
  (with-output-to-string (*standard-output*)
    (princ "(")
    (princ (line-number-at-point (window-point window)))
    (princ ", ")
    (princ (point-column (window-point window)))
    (princ ")")))

(defun modeline-posline (window)
  (cond
    ((<= (buffer-nlines (window-buffer window))
         (window-height window))
     "All  ")
    ((first-line-p (window-view-point window))
     "Top  ")
    ((null (line-offset (copy-point (window-view-point window)
                                    :temporary)
                        (window-height window)))
     "Bot  ")
    (t
     (format nil "~2d%  "
             (floor
              (* 100
                 (float (/ (line-number-at-point (window-view-point window))
                           (buffer-nlines (window-buffer window))))))))))

(defun modeline-apply (window print-fn default-attribute)
  (dolist (item (buffer-value (window-buffer window)
                              'modeline-format
                              *modeline-default-format*))
    (multiple-value-bind (name attribute alignment)
        (if (consp item)
            (values (first item) (or (second item) default-attribute) (or (third item) :left))
            (values item default-attribute :left))
      (when (or (symbolp name) (functionp name))
        (let (attribute-1 alignment-1)
          (multiple-value-setq (name attribute-1 alignment-1) (funcall name window))
          (when attribute-1
            (setf attribute attribute-1))
          (when alignment-1
            (setf alignment alignment-1))))
      (funcall print-fn
               (princ-to-string name)
               attribute
               alignment))))

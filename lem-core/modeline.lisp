(in-package :lem)

(export '(modeline-format
          modeline-add-status-list
          modeline-remove-status-list
          modeline-clear-status-list
          modeline-write-info
          modeline-name
          modeline-mode-names
          modeline-position
          modeline-posline
          convert-modeline-element))

(define-editor-variable modeline-format '(modeline-write-info
                                          modeline-name
                                          modeline-mode-names
                                          modeline-position
                                          (modeline-posline nil :right))
  "")

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

(defun modeline-clear-status-list (&optional (buffer nil bufferp))
  (if bufferp
      (setf (buffer-value buffer 'modeline-status-list) '())
      (setf *modeline-status-list* '())))

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
    (dolist (mode (buffer-minor-modes (window-buffer window)))
      (when (mode-name mode)
        (princ " ")
        (princ (mode-name mode))))
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

(defgeneric convert-modeline-element (element window))

(defmethod convert-modeline-element ((element t) window)
  (princ-to-string element))

(defmethod convert-modeline-element ((element function) window)
  (multiple-value-bind (name attribute alignment)
      (funcall element window)
    (values name attribute alignment)))

(defmethod convert-modeline-element ((element symbol) window)
  (convert-modeline-element (symbol-function element) window))

(defun modeline-apply-1 (window print-fn default-attribute items)
  (dolist (item items)
    (multiple-value-bind (name attribute alignment)
        (if (consp item)
            (values (first item) (or (second item) default-attribute) (or (third item) :left))
            (values item default-attribute :left))
      (let (attribute-1 alignment-1)
        (setf (values name attribute-1 alignment-1)
              (convert-modeline-element name window))
        (when attribute-1 (setf attribute attribute-1))
        (when alignment-1 (setf alignment alignment-1)))
      (funcall print-fn
               (princ-to-string name)
               attribute
               alignment))))

(defun modeline-apply (window print-fn default-attribute)
  (modeline-apply-1 window
                    print-fn
                    default-attribute
                    (or (window-modeline-format window)
                        (variable-value 'modeline-format :default (window-buffer window))))
  (alexandria:when-let ((items (buffer-value (window-buffer window) 'modeline-status-list)))
    (modeline-apply-1 window print-fn default-attribute items))
  (alexandria:when-let ((items *modeline-status-list*))
    (modeline-apply-1 window print-fn default-attribute items)))

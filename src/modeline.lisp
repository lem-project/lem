(in-package :lem)

(define-editor-variable modeline-format '(modeline-write-info
                                          modeline-name
                                          (modeline-posline nil :right)
                                          (modeline-position nil :right)
                                          (modeline-minor-modes nil :right)
                                          (modeline-major-mode nil :right))
  "")

(define-attribute modeline-name-attribute
  (t :foreground "orange"))

(define-attribute modeline-major-mode-attribute
  (t :foreground "#85b8ff"))

(define-attribute modeline-minor-modes-attribute
  (t :foreground "white"))

(define-attribute modeline-position-attribute
  (t :foreground "#FAFAFA" :background "#202020"))

(define-attribute modeline-posline-attribute
  (t :background "#A0A0A0" :foreground "black"))

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
           " 🔒 ")
          ((buffer-modified-p buffer)
           " * ")
          (t
           "   "))))

(defun modeline-name (window)
  (values (buffer-name (window-buffer window))
          'modeline-name-attribute))

(defun modeline-major-mode (window)
  (values (concatenate 'string
                       (mode-name (buffer-major-mode (window-buffer window)))
                       " ")
          'modeline-major-mode-attribute))

(defun modeline-minor-modes (window)
  (values (with-output-to-string (out)
            (dolist (mode (buffer-minor-modes (window-buffer window)))
              (when (mode-name mode)
                (princ (mode-name mode) out)
                (princ " " out))))
          'modeline-minor-modes-attribute))

(defun modeline-position (window)
  (values (format nil
                  " ~D:~D "
                  (line-number-at-point (window-point window))
                  (point-column (window-point window)))
          'modeline-position-attribute))

(defun modeline-posline (window)
  (values (cond
            ((<= (buffer-nlines (window-buffer window))
                 (window-height window))
             "  All  ")
            ((first-line-p (window-view-point window))
             "  Top  ")
            ((null (line-offset (copy-point (window-view-point window)
                                            :temporary)
                                (window-height window)))
             "  Bot  ")
            (t
             (format nil "  ~2d%  "
                     (floor
                      (* 100
                         (float (/ (line-number-at-point (window-view-point window))
                                   (buffer-nlines (window-buffer window)))))))))
          'modeline-posline-attribute))

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
            (values (first item)
                    (if (second item)
                        (merge-attribute (ensure-attribute (second item) nil)
                                         (ensure-attribute default-attribute nil))
                        default-attribute)
                    (or (third item) :left))
            (values item
                    default-attribute
                    :left))
      (let (attribute-1 alignment-1)
        (setf (values name attribute-1 alignment-1)
              (convert-modeline-element name window))
        (when attribute-1
          (setf attribute
                (merge-attribute (ensure-attribute attribute nil)
                                 (ensure-attribute attribute-1 nil))))
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

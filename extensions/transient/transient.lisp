(defpackage :transient
  (:use :cl :lem)
  (:export))

(in-package :transient)

(defvar *transient-popup-window* nil)

(defun show-transient-popup (keymap)
  (let* ((buffer (make-buffer "*transient*" :temporary t :enable-undo-p nil))
         (out (make-buffer-output-stream (buffer-point buffer))))
    (erase-buffer buffer)
    (format out "Keymap: ~A~%" (keymap-name keymap))
    (format out "~%bindings:~%")
    (traverse-keymap keymap
                     (lambda (kseq cmd)
                       (format out "~A ~A~%"
                               (keyseq-to-string kseq)
                               cmd)))
    (buffer-start (buffer-point buffer))
    (let ((width (lem/popup-window::compute-buffer-width buffer))
          (height (lem/popup-window::compute-buffer-height buffer)))
      (if (and *transient-popup-window*
               (not (deleted-window-p *transient-popup-window*)))
          (lem/popup-window::update-popup-window :destination-window *transient-popup-window*
                                                 :source-window (current-window)
                                                 :width width
                                                 :height height)
          (setf *transient-popup-window*
                (lem/popup-window::make-popup-window :source-window (current-window)
                                                     :buffer buffer
                                                     :width width
                                                     :height height
                                                     :style '(:gravity :topright)))))
    (redraw-display)))

(defmethod keymap-activate ((keymap (eql *root-keymap*)))
  (log:info "activated root keymap~%")
  (show-transient-popup keymap))

(defmethod keymap-activate ((keymap keymap))
  (log:info "activated ~A~%" keymap)
  (show-transient-popup keymap))
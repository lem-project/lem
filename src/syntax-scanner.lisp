(in-package :lem-core)

(defvar *syntax-scan-window-recursive-p* nil)

(defun syntax-scan-window (window)
  (check-type window window)
  (when (and (enable-syntax-highlight-p (window-buffer window))
             (null *syntax-scan-window-recursive-p*))
    (let ((*syntax-scan-window-recursive-p* t))
      (with-point ((start (window-view-point window))
                   (end (window-view-point window)))
        (line-start start)
        (unless (move-to-next-virtual-line-n end window (window-height window))
          (buffer-end end))
        (syntax-scan-region start end)))))

(defun syntax-scan-buffer (buffer)
  (check-type buffer buffer)
  (syntax-scan-region (buffer-start-point buffer)
                      (buffer-end-point buffer)))

(defun syntax-scan-when-scroll (window)
  (syntax-scan-window window))

(defun syntax-scan-when-window-size-changed (window)
  (syntax-scan-window window))

(defun syntax-scan-when-buffer-showed (window)
  (syntax-scan-window window))

(defun syntax-scan-when-buffer-edited (start end old-len)
  (declare (ignore old-len))
  (syntax-scan-region start end))

(defvar *syntax-scan-timer* nil)

(defun scheduled-syntax-scan ()
  (syntax-scan-window (current-window)))

(defun init-syntax-scanner ()
  (assert (null *syntax-scan-timer*))
  (setf *syntax-scan-timer* (make-idle-timer 'scheduled-syntax-scan))
  (start-timer *syntax-scan-timer* 100 :repeat t))

(add-hook *window-scroll-functions* 'syntax-scan-when-scroll)
(add-hook *window-size-change-functions* 'syntax-scan-when-window-size-changed)
(add-hook *window-show-buffer-functions* 'syntax-scan-when-buffer-showed)
(add-hook (variable-value 'after-change-functions :global) 'syntax-scan-when-buffer-edited)
(add-hook *find-file-hook* 'syntax-scan-buffer)

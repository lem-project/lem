(in-package :lem-core)

(defvar *syntax-scan-window-recursive-p* nil)

(defun buffer-modified-tick-during-syntax-scan (buffer)
  (buffer-value buffer 'tick-during-syntax-scan))

(defun update-modified-tick-during-syntax-scan (buffer)
  (setf (buffer-value buffer 'tick-during-syntax-scan)
        (buffer-modified-tick buffer)))

(defun need-syntax-scan-p (buffer)
  (not (eql (buffer-modified-tick-during-syntax-scan buffer)
            (buffer-modified-tick buffer))))

(defun syntax-scan-window (window)
  (check-type window window)
  (let ((buffer (window-buffer window)))
    (when (and (need-syntax-scan-p buffer)
               (enable-syntax-highlight-p buffer)
               (null *syntax-scan-window-recursive-p*))
      (update-modified-tick-during-syntax-scan buffer)
      (let ((*syntax-scan-window-recursive-p* t))
        (with-point ((start (window-view-point window))
                     (end (window-view-point window)))
          (line-start start)
          (unless (move-to-next-virtual-line-n end window (window-height window))
            (buffer-end end))
          (syntax-scan-region start end))))))

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
  (syntax-scan-region start end)
  (let ((buffer (point-buffer start)))
    (update-modified-tick-during-syntax-scan buffer)))

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

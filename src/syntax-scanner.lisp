(in-package :lem-core)

(defvar *syntax-scan-window-recursive-p* nil)

;;;; Scanned region tracking for viewport-based syntax highlighting
;;;; Tracks which line ranges have been scanned to avoid redundant work
;;;; while still supporting incremental scanning on scroll.

(defun buffer-scanned-region (buffer)
  "Get the scanned region info: (tick start-line end-line) or NIL."
  (buffer-value buffer 'scanned-region))

(defun (setf buffer-scanned-region) (value buffer)
  (setf (buffer-value buffer 'scanned-region) value))

(defun regions-contiguous-p (old-start old-end new-start new-end)
  "Check if two line ranges are contiguous (adjacent or overlapping)."
  ;; Contiguous if: new range starts before or at old-end+1
  ;;            AND new range ends after or at old-start-1
  (and (<= new-start (1+ old-end))
       (>= new-end (1- old-start))))

(defun update-scanned-region (buffer start-line end-line)
  "Update the scanned region, extending it only if contiguous within the same tick."
  (let ((current-tick (buffer-modified-tick buffer))
        (region (buffer-scanned-region buffer)))
    (if (and region (eql (first region) current-tick))
        ;; Same tick: extend only if contiguous, otherwise replace
        (let ((old-start (second region))
              (old-end (third region)))
          (if (regions-contiguous-p old-start old-end start-line end-line)
              ;; Contiguous: extend the region
              (setf (buffer-scanned-region buffer)
                    (list current-tick
                          (min old-start start-line)
                          (max old-end end-line)))
              ;; Not contiguous: replace with new region
              (setf (buffer-scanned-region buffer)
                    (list current-tick start-line end-line))))
        ;; Different tick: reset the region
        (setf (buffer-scanned-region buffer)
              (list current-tick start-line end-line)))))

(defun viewport-needs-scan-p (buffer start-line end-line)
  "Check if the viewport region needs scanning."
  (let ((current-tick (buffer-modified-tick buffer))
        (region (buffer-scanned-region buffer)))
    (or (null region)
        (not (eql (first region) current-tick))
        (< start-line (second region))
        (> end-line (third region)))))

(defun syntax-scan-window (window)
  "Scan syntax for the visible viewport, skipping if already scanned."
  (check-type window window)
  (let ((buffer (window-buffer window)))
    (when (and (enable-syntax-highlight-p buffer)
               (null *syntax-scan-window-recursive-p*))
      (let ((*syntax-scan-window-recursive-p* t))
        (with-point ((start (window-view-point window))
                     (end (window-view-point window)))
          (line-start start)
          (unless (move-to-next-virtual-line-n end window (window-height window))
            (buffer-end end))
          (let ((start-line (line-number-at-point start))
                (end-line (line-number-at-point end)))
            (when (viewport-needs-scan-p buffer start-line end-line)
              (syntax-scan-region start end)
              (update-scanned-region buffer start-line end-line))))))))

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
  "Rescan the edited region. The scanned-region cache is automatically
   invalidated because buffer-modified-tick changes on edit."
  (declare (ignore old-len))
  (syntax-scan-region start end)
  ;; Update scanned region to include the edited area
  ;; Note: tick has already changed, so this creates a new region entry
  (let* ((buffer (point-buffer start))
         (start-line (line-number-at-point start))
         (end-line (line-number-at-point end)))
    (update-scanned-region buffer start-line end-line)))

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
;; Note: File open uses *window-show-buffer-functions* for viewport-only scanning
;; instead of scanning the entire buffer, which improves performance for large files.

(defpackage :lem-core/commands/multiple-cursors
  (:use :cl :lem-core)
  (:export :add-cursors-to-next-line))
(in-package :lem-core/commands/multiple-cursors)

(define-key *global-keymap* "M-C" 'add-cursors-to-next-line)

(define-command add-cursors-to-next-line () ()
  "Duplicates the cursor under the currently existing cursors."
  (let ((cursors (buffer-cursors (current-buffer))))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (when (and (line-offset p 1 (point-charpos p))
                           (or (null next-cursor)
                               (not (same-line-p p next-cursor))))
                  (make-fake-cursor p))))))

(defun clear-duplicate-cursors (buffer)
  (loop :for (cursor next-cursor) :on (buffer-cursors buffer)
        :when (and next-cursor (same-line-p cursor next-cursor))
        :do (delete-fake-cursor
             (if (eq cursor (buffer-point buffer))
                 next-cursor
                 cursor))))

(define-condition garbage-collection-cursors (after-executing-command) ())
(defmethod handle-signal ((condition garbage-collection-cursors))
  (clear-duplicate-cursors (current-buffer)))

(define-condition clear-cursor-when-aborted (editor-abort-handler) ())
(defmethod handle-signal ((condition clear-cursor-when-aborted))
  (let ((string (merge-cursor-killrings (current-buffer))))
    (clear-cursors (current-buffer))
    (copy-to-clipboard-with-killring string)))

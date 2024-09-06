(defpackage :lem-multiple-cursors
  (:use :cl :lem)
  (:import-from :lem/isearch
                :isearch-start
                :search-next-matched
                :isearch-abort
                :make-add-char-callback)
  (:export :add-cursors-to-next-line
           :add-cursors-to-previous-line
           :mark-next-like-this)
  #+sbcl
  (:lock t))
(in-package :lem-multiple-cursors)

(define-key *global-keymap* "M-C" 'add-cursors-to-next-line)

(define-command add-cursors-to-next-line () ()
  "Duplicates the cursor under the currently existing cursors."
  (add-cursor-to-line-with-offset 1))

(define-command add-cursors-to-previous-line () ()
  "Duplicates the cursor above the currently existing cursors."
  (add-cursor-to-line-with-offset -1))

(define-command mark-next-like-this () ()
  ""
  (if (buffer-mark (current-buffer))
      (mark-like-this-direction (region-beginning-using-global-mode (current-global-mode))
                                (region-end-using-global-mode (current-global-mode))
                                #'search-forward)
      (add-cursors-to-next-line)))

(define-command mark-previous-like-this (start end) (:region)
  ""
  (if (buffer-mark (current-buffer))
      (mark-like-this-direction (region-beginning-using-global-mode (current-global-mode))
                                (region-end-using-global-mode (current-global-mode))
                                #'search-backward)
      (add-cursors-to-previous-line)))

(defun add-cursor-to-line-with-offset (offset)
  (let ((cursors (buffer-cursors (current-buffer))))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (when (and (line-offset p offset (point-charpos p))
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

(defun mark-like-this-direction (start end direction)
  (isearch-start ""
                 (make-add-char-callback direction)
                 direction
                 (if (equal direction #'search-forward)
                     #'search-backward
                     #'search-forward)
                 (points-to-string start end))

  (dolist (point (buffer-cursors (current-buffer)))
    (with-point ((point point))
      (if (search-next-matched point 1)
          (progn (setf cursor (make-fake-cursor point))
                 (setf (point-charpos point) (- (point-charpos point) (- (point-charpos end) (point-charpos start))))
                 (set-cursor-mark cursor point))
          (message "No more matches"))))
  (isearch-abort))

(defun garbage-collection-cursors ()
  (clear-duplicate-cursors (current-buffer)))

(add-hook *post-command-hook* 'garbage-collection-cursors)

(defun clear-cursors-when-aborted ()
  (let ((string (merge-cursor-killrings (current-buffer))))
    (clear-cursors (current-buffer))
    (copy-to-clipboard-with-killring string)))

(add-hook *editor-abort-hook* 'clear-cursors-when-aborted)

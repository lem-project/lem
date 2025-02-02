(defpackage :lem-core/commands/multiple-cursors
  (:use :cl
        :lem-core
        :lem-core/commands/move)
  (:export :add-cursors-to-next-line
           :add-cursors-to-prev-line
           :mark-next-like-this
           :mark-previous-like-this)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/multiple-cursors)

(define-key *global-keymap* "M-C" 'add-cursors-to-next-line)
(define-key *global-keymap* "C->" 'mark-next-like-this)
(define-key *global-keymap* "C-<" 'mark-previous-like-this)

(defun add-cursors-to-offset-line (offset buffer)
  (let ((cursors (buffer-cursors buffer)))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (when (and (line-offset p offset (point-charpos p))
                           (or (null next-cursor)
                               (not (same-line-p p next-cursor))))
                  (make-fake-cursor p))))))

(define-command add-cursors-to-next-line () ()
  "Duplicates the cursor under the currently existing cursors."
  (add-cursors-to-offset-line 1 (current-buffer)))

(define-command add-cursors-to-prev-line () ()
  "Duplicates the cursor above the currently existing cursors."
  (add-cursors-to-offset-line -1 (current-buffer)))

(defun mark-by-direction (beg fin is-forward buffer)
  (let* ((cursors (buffer-cursors buffer))
         (start (if (point< beg fin) beg fin))
         (end (if (point< beg fin) fin beg))
         (str (points-to-string start end))
         (cur-p (current-point))
         (offset-pos (cond
                       ((and (point= start cur-p) is-forward) (length str))
                       ((and (point= end cur-p) (not is-forward)) (* -1 (length str)))
                       (t 0)))
         (offset-region (cond
                          ((point= start cur-p) (length str))
                          ((point= end cur-p) (* -1 (length str)))
                          (t 0)))
         (re (concatenate 'string "(?:" str ")"))
         (search-function (if is-forward #'search-forward-regexp #'search-backward-regexp)))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (character-offset p offset-pos)
                (if (and (apply search-function (list p re))
                         (null next-cursor))
                    (progn
                      (character-offset p (* -1 offset-pos))
                      (set-cursor-mark (make-fake-cursor p) (character-offset p offset-region))
                      (message (concatenate 'string "Marked " (write-to-string (+ (length cursors) 1)))))
                    (message "No more mark"))))))

(define-command mark-next-like-this () ()
  "Duplicate the cursor the next matched string selected in region or next line."
  (if (mark-active-p (cursor-mark (current-point)))
      (let ((start (buffer-mark (current-buffer)))
            (end (buffer-point (current-buffer))))
        (mark-by-direction start end t (current-buffer)))
      (add-cursors-to-offset-line 1 (current-buffer))))

(define-command mark-previous-like-this () ()
  "Duplicate the cursor the previous matched string selected in region or previous line."
  (if (mark-active-p (cursor-mark (current-point)))
      (let ((start (buffer-mark (current-buffer)))
            (end (buffer-point (current-buffer))))
        (mark-by-direction start end nil (current-buffer)))
      (add-cursors-to-offset-line -1 (current-buffer))))

(defun clear-duplicate-cursors (buffer)
  (loop :for (cursor next-cursor) :on (buffer-cursors buffer)
        :when (and next-cursor (point= cursor next-cursor))
        :do (delete-fake-cursor
             (if (eq cursor (buffer-point buffer))
                 next-cursor
                 cursor))))

(defun garbage-collection-cursors ()
  (clear-duplicate-cursors (current-buffer)))

(add-hook *post-command-hook* 'garbage-collection-cursors)

(defun clear-cursors-when-aborted ()
  (let ((string (merge-cursor-killrings (current-buffer))))
    (clear-cursors (current-buffer))
    (copy-to-clipboard-with-killring string)))

(add-hook *editor-abort-hook* 'clear-cursors-when-aborted)

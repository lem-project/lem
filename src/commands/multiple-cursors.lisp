(defpackage :lem-core/commands/multiple-cursors
  (:use :cl
        :lem-core
        :lem-core/commands/move)
  (:export :add-cursors-to-next-line
           :add-cursors-to-prev-line
           :clear-cursors-when-aborted
           :mark-next-like-this
           :mark-previous-like-this)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/multiple-cursors)

(define-key *global-keymap* "M-C" 'add-cursors-to-next-line)
(define-key *global-keymap* "C->" 'mark-next-like-this)
(define-key *global-keymap* "C-<" 'mark-previous-like-this)

(defun add-cursors-to-offset-line (offset buffer)
  (let* ((cursors (buffer-cursors buffer))
         (sorted-cursors (if (> offset 0) cursors (reverse cursors))))
    (loop :for (cursor next-cursor) :on sorted-cursors
          :do (with-point ((p cursor))
                (when (and (line-offset p offset (point-charpos p))
                           (not (find-if (lambda (c) (same-line-p c p)) cursors))
                           (null next-cursor))
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
         (search-function (if is-forward #'search-forward #'search-backward))
         (sorted-cursors (if is-forward (reverse cursors) cursors))
         (cursor (first sorted-cursors)))
    (with-point ((p cursor))
          (character-offset p offset-pos)
          (if (funcall search-function p str)
              (progn
                (character-offset p (* -1 offset-pos))
                (set-cursor-mark (make-fake-cursor p) (character-offset p offset-region))
                (uiop:println (concatenate 'string "Mark set " (write-to-string (+ (length cursors) 1)))))
              (uiop:println "No more matches found")))))

(define-command mark-next-like-this () ()
  "Duplicate the cursor the next matched string selected in region or next line."
  (let* ((buffer (current-buffer))
         (start (buffer-mark buffer))
         (end (buffer-point buffer)))
    (if (mark-active-p (cursor-mark end))
      (mark-by-direction start end t buffer)
      (add-cursors-to-offset-line 1 buffer))))

(define-command mark-previous-like-this () ()
  "Duplicate the cursor the previous matched string selected in region or previous line."
  (let* ((buffer (current-buffer))
         (start (buffer-mark buffer))
         (end (buffer-point buffer)))
    (if (mark-active-p (cursor-mark end))
        (mark-by-direction start end nil buffer)
        (add-cursors-to-offset-line -1 buffer))))

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
  (let* ((buffer (current-buffer))
         (cursors (buffer-cursors buffer))
         (is-marked (find-if (lambda (c) (mark-active-p (cursor-mark c))) cursors)))
    (if is-marked
        (loop :for (cursor) :on cursors
              :do (mark-cancel (cursor-mark cursor)))
        (let ((string (merge-cursor-killrings buffer)))
          (clear-cursors buffer)
          (copy-to-clipboard-with-killring string)))))

(add-hook *editor-abort-hook* 'clear-cursors-when-aborted)

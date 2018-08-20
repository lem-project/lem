(defpackage :lem.rectangle
  (:use :cl :lem)
  (:export)
  #+sbcl
  (:lock t))
(in-package :lem.rectangle)

(defvar *mark-point* nil)
(defvar *overlays* '())

(defvar *rectangle-mark-mode-keymap*
  (make-keymap :name '*rectangle-mark-mode-keymap*
               :undef-hook 'rectangle-self-insert))

(define-minor-mode rectangle-mark-mode
    (:keymap *rectangle-mark-mode-keymap*
     :name "rect")
  (setf *mark-point* (copy-point (current-point) :temporary)))

(define-key *global-keymap* "C-x Space" 'rectangle-mark-mode)
(define-key *rectangle-mark-mode-keymap* 'copy-region 'rectangle-copy)
(define-key *rectangle-mark-mode-keymap* 'kill-region 'rectangle-kill)
(define-key *rectangle-mark-mode-keymap* "C-o" 'rectangle-open)
(define-key *rectangle-mark-mode-keymap* "C-t" 'rectangle-string)
(define-key *rectangle-mark-mode-keymap* 'exchange-point-mark 'rectangle-exchange-point-mark)

(defun remove-overlays ()
  (mapc #'delete-overlay *overlays*)
  (setf *overlays* '()))

(define-command rectangle-end () ()
  (rectangle-mark-mode nil)
  (remove-overlays))

(define-command rectangle-self-insert () ()
  (unread-key-sequence (last-read-key-sequence))
  (rectangle-end))

(defun update-overlay ()
  (remove-overlays)
  (when *mark-point*
    (let ((start-column (and *mark-point* (point-column *mark-point*)))
          (end-column (point-column (current-point))))
      (unless (< start-column end-column)
        (rotatef start-column end-column))
      (apply-region-lines *mark-point*
                          (current-point)
                          (lambda (p)
                            (with-point ((s p) (e p))
                              (move-to-column s start-column)
                              (move-to-column e end-column)
                              (push (make-overlay s e 'region) *overlays*)))))))

(define-command rectangle-copy () ()
  (unless (continue-flag :kill)
    (kill-ring-new))
  (setf *overlays* (sort *overlays* #'point< :key #'overlay-start))
  (kill-push
   (with-output-to-string (out)
     (loop :for ovs :on *overlays*
           :for ov := (car ovs)
           :do (write-string (points-to-string (overlay-start ov) (overlay-end ov)) out)
               (when (cdr ovs)
                 (write-char #\newline out)))))
  (rectangle-end))

(define-command rectangle-kill () ()
  (unless (continue-flag :kill)
    (kill-ring-new))
  (setf *overlays* (sort *overlays* #'point< :key #'overlay-start))
  (with-output-to-string (out)
    (loop :for ovs :on *overlays*
          :for ov := (car ovs)
          :do (delete-between-points (overlay-start ov) (overlay-end ov))
              (write-string (points-to-string (overlay-start ov) (overlay-end ov))
                            out)
              (when (cdr ovs)
                (write-char #\newline out))))
  (rectangle-end))  

(define-command rectangle-open () ()
  (dolist (ov *overlays*)
    (insert-string (overlay-start ov)
                   (make-string (count-characters (overlay-start ov) (overlay-end ov))
                                :initial-element #\space)))
  (move-point (current-point) *mark-point*)
  (rectangle-end))

(defvar *default-string* nil)
(define-command rectangle-string () ()
  (let ((string (prompt-for-string (format nil "String rectangle~:[~; (default ~:*~A)~]: "
                                           *default-string*)
                                   "")))
    (when (and (equal "" string) *default-string*)
      (setf string *default-string*))
    (setf *default-string* string)
    (dolist (ov *overlays*)
      (delete-between-points (overlay-start ov) (overlay-end ov))
      (insert-string (overlay-start ov) string)))
  (rectangle-end))

(define-command rectangle-exchange-point-mark () ()
  (with-point ((point1 *mark-point*)
               (point2 (current-point)))
    (move-point (current-point) point1)
    (move-point *mark-point* point2)))

(defmacro define-rectangle-movable-command (name &rest args)
  (let ((rectangle-command-name (alexandria:symbolicate '#:rectangle- name)))
    `(progn
       (define-key *rectangle-mark-mode-keymap* ',name ',rectangle-command-name)
       (define-command ,rectangle-command-name (arg) ("p")
         (declare (ignorable arg))
         (,name ,@args)
         (update-overlay)))))

(define-rectangle-movable-command next-line arg)
(define-rectangle-movable-command previous-line arg)
(define-rectangle-movable-command forward-char arg)
(define-rectangle-movable-command backward-char arg)
(define-rectangle-movable-command forward-word arg)
(define-rectangle-movable-command previous-word arg)
(define-rectangle-movable-command forward-paragraph arg)
(define-rectangle-movable-command backward-paragraph arg)
(define-rectangle-movable-command move-to-beginning-of-line)
(define-rectangle-movable-command move-to-end-of-line)

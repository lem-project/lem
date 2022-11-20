(in-package :lem)

(define-command (forward-sexp (:advice-classes movable-advice)) (&optional (n 1) no-errors) ("p")
  (with-point ((prev (current-point)))
    (let ((point (form-offset (current-point) n)))
      (or point
          (progn
            (move-point (current-point) prev)
            (if no-errors
                nil
                (scan-error)))))))

(define-command (backward-sexp (:advice-classes movable-advice)) (&optional (n 1) no-errors) ("p")
  (forward-sexp (- n) no-errors))

(define-command (forward-list (:advice-classes movable-advice)) (&optional (n 1) no-errors) ("p")
  (scan-lists (current-point) n 0 no-errors))

(define-command (backward-list (:advice-classes movable-advice)) (&optional (n 1) no-errors) ("p")
  (scan-lists (current-point) (- n) 0 no-errors))

(define-command (down-list (:advice-classes movable-advice)) (&optional (n 1) no-errors) ("p")
  (scan-lists (current-point) n -1 no-errors))

(define-command (backward-up-list (:advice-classes movable-advice)) (&optional (n 1) no-errors) ("p")
  (or (maybe-beginning-of-string (current-point))
      (scan-lists (current-point) (- n) 1 no-errors)))

(define-command mark-sexp () ()
  (cond
    ((continue-flag :mark-sexp)
     (form-offset (mark-point (cursor-mark (current-point))) 1))
    (t
     (save-excursion
       (form-offset (current-point) 1)
       (set-cursor-mark (current-point) (current-point))))))

(define-command (kill-sexp (:advice-classes editable-advice)) (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (let ((end (form-offset (copy-point (current-point) :temporary) 1)))
      (if end
          (with-point ((end end :right-inserting))
            (kill-region (current-point) end))
          (scan-error)))))

(define-command (transpose-sexps (:advice-classes editable-advice)) () ()
  (with-point ((point1 (current-point) :left-inserting)
               (point2 (current-point) :left-inserting))
    (when (and (form-offset point1 -1)
               (form-offset point2 1))
      (alexandria:when-let*
          ((form-string1
            (with-point ((start point1)
                         (end point1 :right-inserting))
              (when (form-offset end 1)
                (prog1 (points-to-string start end)
                  (delete-between-points start end)))))
           (form-string2
            (with-point ((end point2)
                         (start point2))
              (when (form-offset start -1)
                (with-point ((end end :right-inserting))
                  (prog1 (points-to-string start end)
                    (delete-between-points start end)))))))
        (insert-string point1 form-string2)
        (insert-string point2 form-string1)))))

(define-command (kill-around-form (:advice-classes editable-advice)) () ()
  (with-point ((end (current-point) :right-inserting))
    (let ((start (current-point)))
      (unless (form-offset end 1)
        (scan-error))
      (when (symbol-string-at-point start)
        (skip-symbol-backward start))
      (let ((remaining-text (points-to-string start end)))
        (delete-between-points start end)
        (backward-up-list)
        (kill-sexp)
        (save-excursion (insert-string start remaining-text)))
      (form-offset end 1)
      (indent-points start end))))

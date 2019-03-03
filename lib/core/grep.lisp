(defpackage :lem.grep
  (:use :cl :lem)
  (:export :grep)
  #+sbcl
  (:lock t))
(in-package :lem.grep)

(defun grep-parse-line (line)
  (ignore-errors
   (let* ((i (position #\: line))
          (j (position #\: line :start (1+ i)))
          (filename (subseq line 0 i))
          (linum (parse-integer (subseq line (1+ i) j))))
     (when (and (stringp filename) (integerp linum))
       (list filename linum (subseq line j))))))

(defun grep-parse-lines (lines)
  (remove nil
          (mapcar #'grep-parse-line
                  (remove "" lines :test #'string=))))

(defun grep-parse (string)
  (grep-parse-lines (uiop:split-string string :separator '(#\newline))))

(defun grep-with-string (buffer-name revert-fun string)
  (lem.sourcelist:with-sourcelist (sourcelist buffer-name :read-only-p nil :enable-undo-p t)
    (let* ((buffer (get-buffer buffer-name))
           (p (buffer-point buffer)))
      (setf (buffer-value buffer 'grep) t)
      (setf (buffer-value buffer 'lem::revert-buffer-function)
            revert-fun)
      (insert-string p string)
      (buffer-start p)
      (with-point ((p2 p))
        (loop
          (let ((string (line-string p)))
            (ppcre:register-groups-bind (filename line-number-str charpos-str)
                ("^(.*?):(\\d+):(\\d+)?" string)
              (when (and filename line-number-str)
                (let ((pathname (merge-pathnames filename (buffer-directory)))
                      (line-number (parse-integer line-number-str))
                      (charpos (and charpos-str (parse-integer charpos-str))))
                  (move-point p2 (line-start p))
                  (put-text-property p2 (character-offset p (length filename))
                                     :attribute 'lem.sourcelist:title-attribute)
                  (move-point p2 (character-offset p 1))
                  (put-text-property p2 (character-offset p (length line-number-str))
                                     :attribute 'lem.sourcelist:position-attribute)
                  (when charpos
                    (move-point p2 (character-offset p 1))
                    (put-text-property p2 (character-offset p (length charpos-str))
                                       :attribute 'lem.sourcelist:position-attribute))
                  (let ((jump-fn
                          (lambda ()
                            (let* ((buffer (find-file-buffer pathname))
                                   (p (buffer-point buffer)))
                              (move-to-line p line-number)
                              (if charpos
                                  (line-offset p 0 charpos)
                                  (back-to-indentation p))
                              buffer))))
                    (with-point ((start p) (end p) (p p))
                      (line-start start)
                      (line-end end)
                      (character-offset p 1)
                      (put-text-property start end 'jump-fn jump-fn)
                      (put-text-property start p :read-only t)
                      (put-text-property p end 'match-string (points-to-string p end)))
                    (lem.sourcelist:append-jump-function
                     sourcelist
                     (line-start p2)
                     (line-end p)
                     (lambda (set-buffer-fn)
                       (funcall set-buffer-fn (funcall jump-fn)))))))))
          (unless (line-offset p 1) (return))))))
  (redraw-display))

(defun replace-line (point string)
  (with-point ((start point)
               (end point))
    (line-start start)
    (line-end end)
    (delete-between-points start end)
    (insert-string point string)))

(defun next-replace-region (point)
  (unless (or (text-property-at point 'match-string)
              (next-single-property-change point 'match-string))
    (return-from next-replace-region))
  (let ((end (copy-point point :temporary)))
    (next-single-property-change end 'match-string)
    (values (copy-point point :temporary) end)))

(defun replace-diff-with-current-line (point new-string)
  (alexandria:when-let (fn (text-property-at point 'jump-fn))
    (let* ((buffer (funcall fn))
           (point (buffer-point buffer)))
      (replace-line point new-string))))

(defun replace-diff (p)
  (multiple-value-bind (start end)
      (next-replace-region p)
    (unless start (return-from replace-diff nil))
    (let ((old-string (text-property-at start 'match-string))
          (new-string (points-to-string start end)))
      (unless (string= old-string new-string)
        (replace-diff-with-current-line p new-string)))
    (move-point p end)
    t))

(defun replace-all (buffer)
  (with-point ((p (buffer-point buffer)))
    (buffer-start p)
    (loop :while (replace-diff p))))

(define-command grep (string) ((list (prompt-for-string ": " "grep -nH ")))
  (let ((directory (buffer-directory)))
    (labels ((f (&optional a)
               (declare (ignore a))
               (call-background-job
                (lambda ()
                  (with-output-to-string (s)
                    (Uiop:run-program string
                                      :directory directory
                                      :output s
                                      :error-output s
                                      :ignore-error-status t)))
                (alexandria:curry #'grep-with-string
                                  "*grep*"
                                  #'f))))
      (f))))

(define-command grep-replace () ()
  (when (buffer-value (current-buffer) 'grep-buffer t)
    (replace-all (current-buffer))))

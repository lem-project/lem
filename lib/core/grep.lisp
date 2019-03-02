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
  (lem.sourcelist:with-sourcelist (sourcelist buffer-name)
    (let* ((buffer (get-buffer buffer-name))
           (p (buffer-point buffer)))
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
                  (lem.sourcelist:append-jump-function
                   sourcelist
                   (line-start p2)
                   (line-end p)
                   (lambda (set-buffer-fn)
                     (let* ((buffer (find-file-buffer pathname))
                            (p (buffer-point buffer)))
                       (move-to-line p line-number)
                       (if charpos
                           (line-offset p 0 charpos)
                           (back-to-indentation p))
                       (funcall set-buffer-fn buffer))))))))
          (unless (line-offset p 1) (return))))))
  (redraw-display))

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

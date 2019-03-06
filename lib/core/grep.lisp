(defpackage :lem.grep
  (:use :cl :lem)
  (:export :grep)
  #+sbcl
  (:lock t))
(in-package :lem.grep)

(defvar *syntax-table*
  (let ((table (make-syntax-table))
        (tmlanguage (make-tmlanguage
                     :patterns (make-tm-patterns
                                (make-tm-match "^(.*?):(\\d+):(\\d+):(?:.*)"
                                               :captures (vector nil
                                                                 (make-tm-name 'lem.sourcelist:title-attribute)
                                                                 (make-tm-name 'lem.sourcelist:position-attribute)
                                                                 (make-tm-name 'lem.sourcelist:position-attribute)))
                                (make-tm-match "^(.*?):(\\d+):(?:.*)"
                                               :captures (vector nil
                                                                 (make-tm-name 'lem.sourcelist:title-attribute)
                                                                 (make-tm-name 'lem.sourcelist:position-attribute)))))))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode grep-mode nil
    (:name "grep"
     :keymap *grep-mode-keymap*
     :syntax-table *syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'truncate-lines) nil))

(define-key *grep-mode-keymap* "Return" 'grep-jump)
(define-key *grep-mode-keymap* "q" 'quit-window)

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

(defun jump (directory file line-number charpos)
  (let* ((buffer (find-file-buffer (merge-pathnames file directory)))
         (point (buffer-point buffer)))
    (move-to-line point line-number)
    (if charpos
        (line-offset point 0 charpos)
        (back-to-indentation point))
    buffer))

(defun make-jump-function (directory line-string)
  (ppcre:register-groups-bind (file line-number charpos)
      ("^(.*?):(\\d+):(\\d+)?" line-string)
    (when file
      (lambda (set-buffer-fn)
        (funcall set-buffer-fn
                 (jump directory
                       file
                       (parse-integer line-number)
                       (and charpos (parse-integer charpos :junk-allowed t))))))))

(defun cut-path-info (line-string)
  (ppcre:register-groups-bind (string)
      ("^(?:.*?):(?:\\d+):(?:\\d+:)?(.*)" line-string)
    string))

(defun grep-with-string (buffer-name directory revert-fun output-text)
  (lem.sourcelist:with-sourcelist (sourcelist buffer-name :read-only-p nil :enable-undo-p t)
    (let* ((buffer (get-buffer buffer-name))
           (point (buffer-point buffer)))
      (setf (buffer-value buffer 'grep) t)
      (setf (buffer-value buffer 'directory) directory)
      (setf (buffer-value buffer 'lem::revert-buffer-function)
            revert-fun)
      (insert-string point output-text)
      (buffer-start point)
      (with-point ((p point))
        (loop
          (with-point ((start p) (end p))
            (line-start start)
            (line-end end)
            (alexandria:when-let ((fn (make-jump-function directory (line-string start))))
              (put-text-property start end 'old-string (line-string start))
              (lem.sourcelist:append-jump-function sourcelist start end fn)))
          (unless (line-offset p 1) (return))))
      (change-buffer-mode buffer 'grep-mode)))
  (redraw-display))

(defun replace-line (point string)
  (with-point ((start point)
               (end point))
    (line-start start)
    (line-end end)
    (delete-between-points start end)
    (insert-string point string)))

(defun replace-diff-with-current-line (point old-string new-string)
  (ppcre:register-groups-bind (file line-number charpos)
      ("^(.*?):(\\d+):(\\d+:)?" old-string)
    (when (and file line-number)
      (let* ((buffer (jump (buffer-value point 'directory)
                           file
                           (parse-integer line-number)
                           (and charpos (parse-integer charpos))))
             (point (buffer-point buffer)))
        (replace-line point new-string)))))

(defun replace-diff (point)
  (with-point ((p point))
    (line-start p)
    (let ((new-string (cut-path-info (line-string p)))
          (old-string (text-property-at p 'old-string)))
      (when (and new-string
                 old-string
                 (not (string= (cut-path-info old-string) new-string)))
        (replace-diff-with-current-line p old-string new-string))))
  (line-offset point 1))

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
                                  directory
                                  #'f))))
      (f))))

(define-command grep-replace () ()
  (when (buffer-value (current-buffer) 'grep-buffer t)
    (replace-all (current-buffer))))

(defpackage :lem.gtags
  (:use :cl :lem :lem.language-mode)
  (:export :find-definitions))
(in-package :lem.gtags)

(defun parse-line (line)
  (ppcre:register-groups-bind (name line-number file desc)
      ("^(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(.*)$" line)
    (when (and name line-number file desc)
      (list name (parse-integer line-number) file desc))))

(defun global (directory &rest args)
  (with-output-to-string (out)
    (uiop:run-program (format nil "cd ~A; global ~{'~A'~^ ~}" directory args)
                      :output out
                      :ignore-error-status t)))

(defun parse-global-output (text)
  (with-input-from-string (in text)
    (loop :for line := (read-line in nil)
          :while line
          :for parts := (parse-line line)
          :when parts
          :collect parts)))

(defun find-definitions ()
  (let* ((name (symbol-string-at-point (current-point)))
         (text (global (buffer-directory) "-x" name)))
    (loop :for (name line-number file desc) :in (parse-global-output text)
          :collect (make-xref-location :filespec (merge-pathnames file (buffer-directory))
                                       :position (cons line-number 0)
                                       :title desc))))

(define-command gtags-definition-list () ()
  (let ((parts-list (parse-global-output (global (buffer-directory) "-f" (buffer-filename)))))
    (lem.sourcelist:with-sourcelist (sourcelist "*global*")
      (loop :for parts :in parts-list
            :do (destructuring-bind (name line-number file desc) parts
                  (declare (ignore desc))
                  (lem.sourcelist:append-sourcelist
                   sourcelist
                   (lambda (p)
                     (insert-string p name :attribute 'xref-title-attribute))
                   (lambda ()
                     (alexandria:when-let ((buffer (get-buffer file)))
                       (setf (current-window) (pop-to-buffer buffer))
                       (move-to-line (current-point) line-number)))))))))

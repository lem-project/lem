(defpackage :lem.gtags
  (:use :cl :lem :lem.language-mode)
  (:export :find-definitions
           :find-references
           :gtags-definition-list)
  #+sbcl
  (:lock t))
(in-package :lem.gtags)

(defclass content ()
  ((name
    :initarg :name
    :reader content-name)
   (file
    :initarg :file
    :reader content-file)
   (line-number
    :initarg :line-number
    :reader content-line-number)
   (desc
    :initarg :desc
    :reader content-desc)))

(defclass reference-content (content) ())

(defmethod xref-insert-content ((content content) point)
  (insert-string point (content-name content) :attribute 'xref-content-attribute))

(defmethod xref-insert-content ((content reference-content) point)
  (insert-string point (content-file content) :attribute 'lem.sourcelist:title-attribute)
  (insert-string point ":")
  (insert-string point
                 (princ-to-string (content-line-number content))
                 :attribute 'lem.sourcelist:position-attribute)
  (insert-string point ":")
  (insert-string point (content-desc content)))

(defun parse-line (line)
  (ppcre:register-groups-bind (name line-number file desc)
      ("^(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(.*)$" line)
    (when (and name line-number file desc)
      (list name (parse-integer line-number) file desc))))

(defun parts-name (parts) (first parts))
(defun parts-line-number (parts) (second parts))
(defun parts-file (parts) (third parts))
(defun parts-desc (parts) (fourth parts))

(defun global (directory &rest args)
  (with-output-to-string (out)
    (uiop:run-program (cons "global" args)
                      :directory directory
                      :output out
                      :ignore-error-status t)))

(defun parse-global-output (text)
  (with-input-from-string (in text)
    (loop :for line := (read-line in nil)
          :while line
          :for parts := (parse-line line)
          :when parts
          :collect parts)))

(defun result-to-xref-locations (text content-name)
  (loop :for (name line-number file desc) :in (parse-global-output text)
        :collect (make-xref-location :filespec (merge-pathnames file (buffer-directory))
                                     :position (cons line-number 0)
                                     :content (make-instance content-name
                                                             :name name
                                                             :line-number line-number
                                                             :file file
                                                             :desc desc))))

(defun read-name (point prompt)
  (or (symbol-string-at-point point)
      (prompt-for-line prompt "" nil nil 'read-name)))

(defun find-definitions (point)
  (let* ((name (read-name point "gtags -x "))
         (text (global (buffer-directory) "-x" name)))
    (result-to-xref-locations text 'content)))

(defun find-references (point)
  (let* ((name (read-name point "gtags -rx "))
         (text (global (buffer-directory (point-buffer point)) "-rx" name))
         (locations (result-to-xref-locations text 'reference-content)))
    (make-xref-references :locations locations)))

(defun gtags-path ()
  (string-right-trim
   '(#\newline)
   (with-output-to-string (out)
     (uiop:run-program '("global" "-p")
                       :output out
                       :ignore-error-status t))))

(defun fetch-gtags-definitions (basedir)
  (loop :for filename
        :in (sort (append (directory (merge-pathnames "*.c" basedir))
                          (directory (merge-pathnames "*.h" basedir)))
                  #'string<
                  :key #'file-namestring)
        :append (parse-global-output (global basedir "-f" filename))))

(defun gtags-definition-list-cont (basedir parts-list)
  (let ((max-len (loop :for parts :in parts-list
                       :for name := (parts-name parts)
                       :maximize (+ 3 (length name)))))
    (lem.sourcelist:with-sourcelist (sourcelist "*gtags-definitions*")
      (dolist (parts parts-list)
        (let ((name (parts-name parts))
              (file (parts-file parts))
              (linum (parts-line-number parts)))
          (lem.sourcelist:append-sourcelist
           sourcelist
           (lambda (p)
             (insert-string p name)
             (move-to-column p max-len t)
             (insert-string p file :attribute 'lem.sourcelist:title-attribute)
             (insert-string p ":")
             (insert-string p (princ-to-string linum)
                            :attribute 'lem.sourcelist:position-attribute))
           (lambda (set-buffer-fn)
             (alexandria:when-let ((buffer (or (get-buffer file)
                                               (find-file-buffer
                                                (merge-pathnames file basedir)))))
               (funcall set-buffer-fn buffer)
               (move-to-line (current-point) linum))))))))
  (redraw-display))

(define-command gtags-definition-list () ()
  (let ((basedir (buffer-directory)))
    (call-background-job (lambda ()
                           (fetch-gtags-definitions basedir))
                         (alexandria:curry #'gtags-definition-list-cont basedir))))

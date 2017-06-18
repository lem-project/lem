(defpackage :lem.gtags
  (:use :cl :lem :lem.language-mode)
  (:export :find-definitions
           :find-references
           :gtags-definition-list))
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
  (insert-string point (content-file content) :attribute 'lem.grep:title-attribute)
  (insert-string point ":")
  (insert-string point
                 (princ-to-string (content-line-number content))
                 :attribute 'lem.grep:position-attribute)
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

(defun result-to-xref-locations (text content-name)
  (loop :for (name line-number file desc) :in (parse-global-output text)
        :collect (make-xref-location :filespec (merge-pathnames file (buffer-directory))
                                     :position (cons line-number 0)
                                     :content (make-instance content-name
                                                             :name name
                                                             :line-number line-number
                                                             :file file
                                                             :desc desc))))

(defun read-name (prompt)
  (or (symbol-string-at-point (current-point))
      (prompt-for-line prompt "" nil nil 'read-name)))

(defun find-definitions ()
  (let* ((name (read-name "gtags -x "))
         (text (global (buffer-directory) "-x" name)))
    (result-to-xref-locations text 'content)))

(defun find-references ()
  (let* ((name (read-name "gtags -rx "))
         (text (global (buffer-directory) "-rx" name))
         (locations (result-to-xref-locations text 'reference-content)))
    (make-xref-references :locations locations)))

(define-command gtags-definition-list () ()
  (let* ((parts-list (parse-global-output (global (buffer-directory) "-f" (buffer-filename))))
         (names (mapcar #'parts-name parts-list))
         (name (prompt-for-line "> " ""
                                (lambda (str) (completion str names))
                                (lambda (str) (find str names :test #'string=))
                                'gtags-definition-list))
         (i (position name parts-list :key #'parts-name :test #'string=)))
    (when i
      (let* ((parts (elt parts-list i))
             (file (parts-file parts))
             (line-number (parts-line-number parts)))
        (alexandria:when-let ((buffer (get-buffer file)))
          (setf (current-window) (pop-to-buffer buffer))
          (move-to-line (current-point) line-number))))))

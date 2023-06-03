(defpackage :lem/document
  (:use :cl :lem)
  (:export :generate-markdown-file))
(in-package :lem/document)

(defclass chunk ()
  ((items
    :initarg :items
    :initform nil
    :accessor chunk-items)))

(defclass table ()
  ((title
    :initarg :title
    :initform nil
    :accessor table-title)
   (items
    :initarg :items
    :initform nil
    :accessor table-items)))

(defclass <table-row> ()
  ((values
    :initarg :values
    :initform nil
    :accessor table-row-values)))

(defclass table-header (<table-row>)
  ())

(defclass table-item (<table-row>)
  ())

(defclass link ()
  ((alt
    :initarg :alt
    :initform nil
    :accessor link-alt)
   (url
    :initarg :url
    :initform nil
    :accessor link-url)))

(defun extract-defpackage-name (form)
  (assert (and (consp form)
               (member (first form) '(defpackage uiop:define-package))))
  (second form))

(defun collect-global-command-packages ()
  (loop :for component :in (asdf:component-children (asdf:find-component :lem "commands"))
        :collect (find-package
                  (extract-defpackage-name
                   (uiop:read-file-form
                    (asdf:component-pathname component))))))

(defun sort-by-file-location (commands)
  (sort commands
        #'<
        :key (lambda (command)
               (sb-c:definition-source-location-toplevel-form-number
                   (lem-core::command-source-location command)))))

(defun collect-commands-in-package (package)
  (let ((commands '()))
    (do-external-symbols (sym package)
      (let ((command (get-command sym)))
        (when command
          (push command commands))))
    (sort-by-file-location commands)))

(defun command-bindings (command)
  (collect-command-keybindings (command-name command) *global-keymap*))

(defun binding-to-string (binding)
  (format nil "~{~A~^ ~}" binding))

(defun key-bindings (command)
  (format nil "~{~A~^, ~}"
          (loop :for binding :in (command-bindings command)
                :collect (binding-to-string binding))))

(defun description (command)
  (documentation (command-name command) 'function))

(defun category-name (package-name)
  (string-capitalize (car (last (uiop:split-string package-name :separator "/")))))

(defun command-name-with-link (command)
  (make-instance 'link
                 :alt (string-downcase (command-name command))
                 :url "TODO"))

(defun construct-package-documentation (package)
  (make-instance
   'table
   :title (category-name (package-name package))
   :items (cons (make-instance 'table-header :values (list "Command" "Key bindings" "Documentation"))
                (loop :for command :in (collect-commands-in-package package)
                      :collect (make-instance 'table-item
                                              :values (list (command-name-with-link command)
                                                            (key-bindings command)
                                                            (description command)))))))

(defun construct-global-command-documentation ()
  (make-instance 'chunk :items (mapcar #'construct-package-documentation
                                       (collect-global-command-packages))))

(defclass markdown-generator () ())

(defmethod generate ((generator markdown-generator) (element chunk) point)
  (dolist (item (chunk-items element))
    (generate generator item point)
    (insert-character point #\newline)))

(defgeneric content (element)
  (:method ((element link))
    (link-alt element))
  (:method ((element string))
    element))

(defun item-length (item)
  (etypecase item
    (link (length (link-alt item)))
    (string (length item))
    (null 0)))

(defun table-width (item)
  (length (table-row-values (first item))))

(defun compute-table-column-width-list (table)
  (loop :for column-index :from 0 :below (table-width (table-items table))
        :collect (loop :for item :in (table-items table)
                       :maximize (item-length (elt (table-row-values item) column-index)))))

(defmethod generate ((generator markdown-generator) (element table) point)
  (insert-string point (format nil "## ~A~%" (table-title element)))
  (let ((width-list (compute-table-column-width-list element)))
    (loop :for item :in (table-items element)
          :for header := t :then nil
          :do (loop :for content :in (table-row-values item)
                    :for width :in width-list
                    :for first := t :then nil
                    :do (if first
                            (insert-string point "| ")
                            (insert-string point " | "))
                        (let ((column (point-column point)))
                          (when content
                            (insert-string point (content content)))
                          (move-to-column point (+ column width) t)))
              (insert-string point " |")
              (insert-character point #\newline)
              (when header
                (loop :for width :in width-list
                      :for first := t :then nil
                      :do (if first
                              (insert-string point "|-")
                              (insert-string point "-|-"))
                          (insert-string point (make-string width :initial-element #\-)))
                (insert-string point "-|")
                (insert-character point #\newline)))))

(defun generate-markdown-file (filename)
  (let* ((buffer (make-buffer nil :temporary t))
         (point (buffer-point buffer)))
    (erase-buffer buffer)
    (generate (make-instance 'markdown-generator)
              (construct-global-command-documentation)
              point)
    (alexandria:write-string-into-file (buffer-text buffer)
                                       filename
                                       :if-exists :supersede)))


(define-major-mode documentation-mode ()
    (:name "Documentation"
     :keymap *documentation-mode-keymap*))

(defclass buffer-generator () ())

(defmethod generate ((generator buffer-generator) (element chunk) point)
  (dolist (item (chunk-items element))
    (generate generator item point)
    (insert-character point #\newline)))

(defmethod generate ((generator buffer-generator) (element table) point)
  (insert-string point (table-title element) :attribute (make-attribute :bold t :reverse t))
  (insert-character point #\newline)
  (let ((width-list (compute-table-column-width-list element)))
    (loop :for item :in (table-items element)
          :for header := t :then nil
          :do (loop :for content :in (table-row-values item)
                    :for width :in width-list
                    :for first := t :then nil
                    :do (if first
                            (insert-string point "  ")
                            (insert-string point "   "))
                        (let ((column (point-column point)))
                          (when content
                            (insert-string point (content content)))
                          (move-to-column point (+ column width) t)))
              (insert-string point "  ")
              (when header
                (with-point ((start point)
                             (end point))
                  (back-to-indentation start)
                  (line-end end)
                  (put-text-property start end :attribute (make-attribute :underline t :bold t))))
              (insert-character point #\newline))))

(defun generate-keybindings-buffer ()
  (let* ((buffer (make-buffer "*Key Bindings*"))
         (point (buffer-point buffer)))
    (erase-buffer buffer)
    (generate (make-instance 'buffer-generator)
              (construct-global-command-documentation)
              point)
    (buffer-start point)
    (change-buffer-mode buffer 'documentation-mode)))

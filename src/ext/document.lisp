(defpackage :lem/document
  (:use :cl :lem))
(in-package :lem/document)

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

(defun sort-by-file-location (command-and-source-location-pairs)
  (sort command-and-source-location-pairs
        #'<
        :key (lambda (elt)
               (sb-c:definition-source-location-toplevel-form-number (cdr elt)))))

(defun collect-commands-in-package (package)
  (let ((command-and-source-location-pairs '()))
    (do-external-symbols (sym package)
      (let ((command (get-command sym)))
        (when command
          (push (cons (command-name command)
                      (lem-core::command-source-location command))
                command-and-source-location-pairs))))
    (mapcar #'first (sort-by-file-location command-and-source-location-pairs))))

(defun command-bindings (command)
  (collect-command-keybindings command *global-keymap*))

(defun binding-to-string (binding)
  (format nil "~{~A~^ ~}" binding))

(defun key-bindings (command)
  (format nil "~{~A~^, ~}"
          (loop :for binding :in (command-bindings command)
                :collect (binding-to-string binding))))

(defun description (command)
  (documentation command 'function))

(defun category-name (package-name)
  (string-capitalize (car (last (uiop:split-string package-name :separator "/")))))

(defun construct-package-documentation (package)
  (cons (category-name (package-name package))
        (cons (list "Command" "Key bindings" "Documentation")
              (loop :for command :in (collect-commands-in-package package)
                    :collect (list (princ-to-string command)
                                   (key-bindings command)
                                   (description command))))))

(defun construct-global-command-documentation ()
  (mapcar #'construct-package-documentation
          (collect-global-command-packages)))

;;; markdown document generator
(defun table-width (table)
  (length (first table)))

(defun compute-table-column-width-list (table)
  (loop :for column-index :from 0 :below (table-width table)
        :collect (loop :for row :in table
                       :maximize (length (elt row column-index)))))

(defun print-table (point table)
  (let ((width-list (compute-table-column-width-list table)))
    (loop :for row :in table
          :for header := t :then nil
          :do (loop :for content :in row
                    :for width :in width-list
                    :for first := t :then nil
                    :do (if first
                            (insert-string point "| ")
                            (insert-string point " | "))
                        (let ((column (point-column point)))
                          (insert-string point content)
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

(defun generate-all-documents ()
  (let* ((buffer (make-buffer "*Help*"))
         (point (buffer-point buffer)))
    (erase-buffer buffer)
    (loop :for (category . table) :in (construct-global-command-documentation)
          :do (insert-string point (format nil "## ~A~%" category))
              (print-table point table)
              (insert-character point #\newline))))

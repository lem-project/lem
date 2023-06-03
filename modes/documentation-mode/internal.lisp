(defpackage :lem-documentation-mode/internal
  (:use :cl :lem)
  (:export :generate-markdown-file
           :generate-buffer
           :select-link))
(in-package :lem-documentation-mode/internal)

(defstruct location
  file
  position
  line-number)

(defclass chunk ()
  ((items
    :initarg :items
    :accessor chunk-items)))

(defclass table ()
  ((title
    :initarg :title
    :accessor table-title)
   (items
    :initarg :items
    :accessor table-items)))

(defclass <table-row> ()
  ((values
    :initarg :values
    :accessor table-row-values)))

(defclass table-header (<table-row>)
  ())

(defclass table-item (<table-row>)
  ())

(defclass link ()
  ((alt
    :initarg :alt
    :accessor link-alt)
   (url
    :initarg :url
    :accessor link-url)
   (location
    :initarg :location
    :accessor link-location)))

(defun command-definition-location (command)
  (let* ((file (sb-c:definition-source-location-namestring (lem-core::command-source-location command)))
         (n (sb-c:definition-source-location-toplevel-form-number (lem-core::command-source-location command)))
         (buffer (find-file-buffer file :temporary t :syntax-table lem-lisp-syntax:*syntax-table*))
         (point (buffer-point buffer)))
    (buffer-start point)
    (form-offset point n)
    (scan-lists point 1 -1)
    (make-location :file file
                   :position (position-at-point point)
                   :line-number (line-number-at-point point))))

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
  (let ((location (command-definition-location command)))
    (make-instance
     'link
     :alt (string-downcase (command-name command))
     :location location
     :url (format nil
                  "https://github.com/lem-project/lem/blob/~A/~A#L~D"
                  (lem-core::lem-git-revision)
                  (enough-namestring (location-file location) (asdf:system-source-directory :lem))
                  (location-line-number location)))))

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

(defgeneric content (generator element)
  (:method ((generator markdown-generator) (element link))
    (format nil "[~A](~A)" (link-alt element) (link-url element)))
  (:method (generator (element link))
    (link-alt element))
  (:method (generator (element string))
    element))

(defun item-length (generator item)
  (etypecase item
    (link (length (content generator item)))
    (string (length item))
    (null 0)))

(defun table-width (item)
  (length (table-row-values (first item))))

(defun compute-table-column-width-list (generator table)
  (loop :for column-index :from 0 :below (table-width (table-items table))
        :collect (loop :for item :in (table-items table)
                       :maximize (item-length generator (elt (table-row-values item) column-index)))))

(defmethod generate ((generator markdown-generator) (element chunk) point)
  (dolist (item (chunk-items element))
    (generate generator item point)
    (insert-character point #\newline)))

(defmethod generate ((generator markdown-generator) (element table) point)
  (insert-string point (format nil "## ~A~%" (table-title element)))
  (let ((width-list (compute-table-column-width-list generator element)))
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
                            (insert-string point (content generator content)))
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


(defclass buffer-generator () ())

(defmethod insert-content ((generator buffer-generator) point content)
  (insert-string point (content generator content)))

(defmethod insert-content ((generator buffer-generator) point (content link))
  (insert-string point (link-alt content) :link (link-location content)))

(defmethod generate ((generator buffer-generator) (element chunk) point)
  (let* ((width-lists (loop :for item :in (chunk-items element)
                            :do (assert (typep item 'table))
                            :collect (compute-table-column-width-list generator item)))
         (width-list (loop :for i :from 0 :below (length (first width-lists))
                           :collect (loop :for width-list :in width-lists
                                          :maximize (elt width-list i)))))
    (dolist (item (chunk-items element))
      (generate-table generator item point width-list)
      (insert-character point #\newline))))

(defmethod generate-table ((generator buffer-generator) element point width-list)
  (insert-string point (table-title element) :attribute (make-attribute :bold t :reverse t))
  (insert-character point #\newline)
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
                          (insert-content generator point content))
                        (move-to-column point (+ column width) t)))
            (insert-string point "  ")
            (when header
              (with-point ((start point)
                           (end point))
                (back-to-indentation start)
                (line-end end)
                (put-text-property start end :attribute (make-attribute :underline t :bold t))))
            (insert-character point #\newline)))

(defun generate-buffer (buffer-name)
  (let* ((buffer (make-buffer buffer-name))
         (point (buffer-point buffer)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (generate (make-instance 'buffer-generator)
                (construct-global-command-documentation)
                point)
      (buffer-start point))
    buffer))

;;;
(defun go-to-location (location)
  (let ((buffer (find-file-buffer (location-file location))))
    (switch-to-buffer buffer)
    (move-to-position (current-point) (location-position location))))

(defun select-link (point)
  (let ((location (text-property-at point :link)))
    (when location
      (go-to-location location))))

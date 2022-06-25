(defpackage :cl-lsp/defpackage
  (:use :cl)
  (:shadow :defpackage)
  (:export :defpackage))
(in-package :cl-lsp/defpackage)

(defparameter *defpackage-options*
  '(:use
    :export
    :import-from
    :shadow
    :shadowing-import-from
    :documentation
    :intern
    :size
    :nicknames))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun filter-ansi-options (options)
    (loop :for option :in options
          :when (member (first option) *defpackage-options*)
          :collect option))

  (defun filter-non-ansi-options (options)
    (loop :for option :in options
          :unless (member (first option) *defpackage-options*)
          :collect option))

  (defun expand-non-ansi-options (package-name options)
    (let ((forms
            (loop :for option :in options
                  :for form
                     := (ecase (first option)
                          (:lock
                           (when (second option)
                             `(cl-package-locks:lock-package ',package-name)))
                          (:local-nicknames
                           `(progn
                              ,@(loop :for (local-nickname . actual-package-names) :in (rest option)
                                      :append (mapcar (lambda (actual-package)
                                                        `(trivial-package-local-nicknames:add-package-local-nickname
                                                          ',local-nickname
                                                          ',actual-package
                                                          ',package-name))
                                                      actual-package-names)))))
                  :when form
                  :collect form)))
      `(eval-when (:compile-toplevel :load-toplevel :execute) ,@forms))))

(defun translate-defpackage (package-name options)
  (unless (find :lock options :key #'first)
    (setf options (append options (list '(:lock t)))))
  (let* ((ansi-options (filter-ansi-options options))
         (non-ansi-options (filter-non-ansi-options options))
         (extra-form (expand-non-ansi-options package-name non-ansi-options)))
    `(progn
       (cl:defpackage ,package-name
         ,@ansi-options)
       ,extra-form)))

(defmacro defpackage (name &body options)
  (translate-defpackage name options))

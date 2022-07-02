(defpackage :lem-base
  (:use :cl)
  #+sbcl
  (:lock t)

  ;; utils.lisp
  (:export
   :collect-subclasses
   :utf8-bytes
   :bests-if
   :max-if
   :min-if
   :find-tree
   :do-sequence
   :if-push)
  ;; string-width-utils
  (:export :+default-tab-size+
           :control-char
           :wide-char-p
           :char-width
           :string-width
           :wide-index)
  ;; file-utils.lisp
  (:export :expand-file-name
           :tail-of-pathname
           :directory-files
           :list-directory
           :file-size
           :virtual-probe-file
           :with-open-virtual-file)
  ;; errors.lisp
  (:export :editor-condition
           :directory-does-not-exist
           :directory-does-not-exist-directory
           :read-only-error
           :editor-error
           :scan-error
           :editor-interrupt)
  ;; hooks.lisp
  (:export :run-hooks
           :add-hook
           :remove-hook))

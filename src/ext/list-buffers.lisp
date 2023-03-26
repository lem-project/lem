(defpackage :lem/list-buffers
  (:use :cl
        :lem)
  (:import-from :lem/multi-column-list
                :multi-column-list
                :display
                :quit)
  (:export :list-buffers))
(in-package :lem/list-buffers)

(define-key *global-keymap* "C-x C-b" 'list-buffers)

(define-command list-buffers () ()
  (display
   (make-instance 'multi-column-list
                  :columns '("" "Buffer" "File")
                  :items (buffer-list)
                  :select-callback (lambda (component buffer)
                                     (quit component)
                                     (switch-to-buffer buffer))
                  :delete-callback (lambda (component buffer)
                                     (declare (ignore component))
                                     (kill-buffer buffer))
                  :column-function (lambda (component buffer)
                                     (declare (ignore component))
                                     (list (string-trim " " (lem::buffer-attributes buffer))
                                           (buffer-name buffer)
                                           (or (buffer-filename buffer) "")))
                  :use-mark t)))

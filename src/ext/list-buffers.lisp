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
                  :column-function (lambda (component buffer)
                                     (declare (ignore component))
                                     (list (string-trim " " (buffer-attributes buffer))
                                           (buffer-name buffer)
                                           (or (buffer-filename buffer) "")))
                  :items (buffer-list)
                  :select-callback (lambda (component buffer)
                                     (quit component)
                                     (switch-to-buffer buffer))
                  :delete-callback (lambda (component buffer)
                                     (declare (ignore component))
                                     (kill-buffer buffer))
                  :use-mark t)))

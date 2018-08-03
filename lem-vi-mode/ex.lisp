(defpackage :lem-vi-mode.ex
  (:use :cl
        :lem
        :lem-vi-mode.core
        :lem-vi-mode.ex-parser)
  (:export :vi-ex))
(in-package :lem-vi-mode.ex)

(defvar *ex-keymap* (make-keymap :name '*ex-keymap*))

(define-vi-state ex (:keymap *ex-keymap*))

(define-key *ex-keymap* "Return" 'minibuffer-read-line-execute)

(define-command vi-ex () ()
  (let ((directory (lem:buffer-directory)))
    (with-state 'ex
      (execute-ex
       (prompt-for-line ":" ""
                        (lambda (str)
                          (when (alexandria:starts-with-subseq "e " str)
                            ;; Almost same as minibuffer-file-complete in lem-core/completion-file.lisp
                            ;; except item's :start offsets which will be used when selecting a completion item.
                            (mapcar (lambda (filename)
                                      (let ((label (lem.completion-mode::pathname-name* filename)))
                                        (with-point ((s (lem::minibuffer-start-point))
                                                     (e (lem::minibuffer-start-point)))
                                          (lem.completion-mode::make-completion-item
                                           :label label
                                           :start (character-offset
                                                   s
                                                   (+ (length (namestring (uiop:pathname-directory-pathname (subseq str 2)))) 2))
                                           :end (line-end e)))))
                                    (lem.completion-mode::completion-file
                                     (subseq str 2)
                                     directory))))
                        nil 'vi-ex)))))

(defun execute-ex (string)
  (let ((lem-vi-mode.ex-core:*point* (current-point)))
    (eval (parse-ex string))))

(defpackage :lem-vi-mode/ex
  (:use :cl
        :lem
        :lem-vi-mode/core
        :lem-vi-mode/ex-parser)
  (:export :vi-ex))
(in-package :lem-vi-mode/ex)

(defvar *ex-keymap* (make-keymap :name '*ex-keymap*))

(define-vi-state ex () ()
  (:default-initargs
   :keymap *ex-keymap*))

(define-command vi-ex () ()
  (let ((directory (lem:buffer-directory)))
    (with-state 'ex
      (execute-ex
       (prompt-for-string
        ":"
        :completion-function
        (lambda (str)
          (cond
            ((ppcre:scan "^(e|vs|sp)[ \\.]" str)
             (let ((comp-str (ppcre:regex-replace "^(e|vs|sp)\\s*" str "")))
               (if (string= comp-str ".")
                   (list (format nil "~A/" str))
                   ;; Almost same as prompt-file-complete in lem-core/completion-file.lisp
                   ;; except item's :start offsets which will be used when selecting a completion item.
                   (mapcar (lambda (filename)
                             (let ((label (tail-of-pathname filename))
                                   (prefix-len (- (length str) (length comp-str))))
                               (with-point ((s (lem/prompt-window::current-prompt-start-point))
                                            (e (lem/prompt-window::current-prompt-start-point)))
                                 (lem/completion-mode:make-completion-item
                                  :label label
                                  :start (character-offset
                                          s
                                          (+ (length
                                              (namestring
                                               (uiop:pathname-directory-pathname (subseq str prefix-len))))
                                             prefix-len))
                                  :end (line-end e)))))
                           (lem/completion-mode::completion-file
                            comp-str
                            directory)))))
            ((ppcre:scan "^(?:b(?:uffer)?|bd(?:elete)?) " str)
             (let ((comp-str (ppcre:regex-replace "^(?:b(?:uffer)?|bd(?:elete)?)\\s+" str "")))
               (mapcar (lambda (buffer)
                         (let ((prefix-len (- (length str) (length comp-str))))
                           (with-point ((s (lem/prompt-window::current-prompt-start-point))
                                        (e (lem/prompt-window::current-prompt-start-point)))
                             (lem/completion-mode:make-completion-item
                              :label (buffer-name buffer)
                              :detail (let ((filename (buffer-filename buffer)))
                                        (when filename
                                          (enough-namestring filename (probe-file "./"))))
                              :start (character-offset s prefix-len)
                              :end (line-end e)))))
                       (lem/completion-mode::completion-buffer
                        comp-str))))))
        :history-symbol 'vi-ex)))))

(defun execute-ex (string)
  (let ((lem-vi-mode/ex-core:*point* (current-point)))
    (eval (parse-ex string))))

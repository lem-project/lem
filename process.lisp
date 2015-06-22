(in-package :lem)

(define-key *global-keymap* "C-x#" 'filter-buffer)
(defcommand filter-buffer (str) ("sFilter buffer: ")
  (let ((outstr (make-array '(0)
                  :element-type 'character
                  :fill-pointer t)))
    (with-output-to-string (output outstr)
      (with-input-from-string (input
                               (join (string #\newline)
                                 (buffer-take-lines (window-buffer))))
        (shell-command str :output output :input input)))
    (buffer-erase (window-buffer))
    (insert-string outstr)))

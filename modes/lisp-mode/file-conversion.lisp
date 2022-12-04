(in-package :lem-lisp-mode)

(defvar *file-conversion-map* '())

(defun convert-remote-to-local-file (filename)
  (loop :for (remote-file . local-file) :in *file-conversion-map*
        :do (when (alexandria:starts-with-subseq remote-file filename)
              (return (concatenate 'string local-file (subseq filename (length remote-file)))))
        :finally (return filename)))

(defpackage :lem.multiple-cursors
  (:use :cl :lem)
  #+sbcl
  (:lock t))
(in-package :lem.multiple-cursors)

(defmethod execute :around (mode
                            (command lem::movable-advice)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem::jump-cursor-advice)
                            argument)
  (prog1 (call-next-method)
    (lem::clear-cursors (current-buffer))))

(defmethod execute :around (mode
                            (command lem:delete-next-char)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute (mode
                    (command lem:delete-previous-char)
                    argument)
  (cond ((mark-active-p (lem::cursor-mark (current-point)))
         (lem::do-each-cursors ()
           (lem::delete-cursor-region (current-point))))
        (t
         (lem::do-each-cursors ()
           (lem::delete-previous-char-1 argument)))))

(defmethod execute :around (mode
                            (command lem:copy-region)
                            argument)
  (lem::do-each-cursors ()
    (lem::copy-cursor-region (current-point))))

(defmethod execute :around (mode
                            (command lem:kill-region)
                            argument)
  (lem::do-each-cursors ()
    (lem::kill-cursor-region (current-point))))

(defmethod execute :around (mode
                            (command lem:kill-line)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:yank)
                            argument)
  (let ((lem::*enable-clipboard-p* (and (lem:enable-clipboard-p)
                                        (null (lem::buffer-fake-cursors (current-buffer))))))
    (lem::process-each-cursors #'call-next-method)))

(defmethod execute :around (mode
                            (command lem:yank-pop)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:yank-pop-next)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:paste-from-clipboard)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:entab-line)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:detab-line)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:delete-blank-lines)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:just-one-space)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:delete-indentation)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:transpose-characters)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem::increment)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem::decrement)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:mark-set)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:exchange-point-mark)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:quoted-insert)
                            argument)
  (let* ((key (read-key))
         (char (or (key-to-char key) (code-char 0))))
    (lem::do-each-cursors ()
      (lem::self-insert-aux char (or argument 1)))))

(defmethod execute :around (mode
                            (command lem:newline)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:open-line)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:mark-sexp)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:kill-sexp)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:transpose-sexps)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:delete-word)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:downcase-region)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:uppercase-region)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:kill-paragraph)
                            argument)
  (lem::process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command lem:self-insert)
                            argument)
  (lem::process-each-cursors #'call-next-method))

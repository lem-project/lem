(in-package :lem)

(defvar *textbuf-list* nil)

(defun tblist-add (tb)
  (push tb *textbuf-list*)
  tb)

(defun tblist-any-modif-p ()
  (find-if 'textbuf-modif-p *textbuf-list*))

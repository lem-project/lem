(in-package :lem)

(defvar *tblist* nil)

(defun tblist-add (tb)
  (push tb *tblist*)
  tb)

(defun tblist-any-modif-p ()
  (find-if 'textbuf-modif-p *tblist*))

(defun tblist-find (name)
  (find-if (lambda (textbuf)
             (string= name (textbuf-name textbuf)))
    *tblist*))

(add-command 'tblist-select 'select-buffer "C-xb")
(defun tblist-select (buffer arg)
  (let* ((name (mb-readline "Use buffer: "))
         (tb (tblist-find name)))
    (buffer-change-textbuf *current-buffer* tb)
    t))

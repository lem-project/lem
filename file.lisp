(in-package :lem)

(defgeneric file-open (buffer filename))
(defgeneric file-find (buffer arg))

(defmethod file-open ((buffer buffer) filename)
  (let ((textbuf (make-textbuf filename)))
    (with-open-file (in filename)
      (do ((str #1=(read-line in nil) #1#))
	  ((null str))
        (textbuf-append-line textbuf str)))
    (buffer-change-textbuf buffer textbuf)))

(defmethod file-find ((buffer buffer) arg)
  (declare (ignore arg))
  (let ((filename (mb-readline "Find File: ")))
    (file-open buffer filename)
    t))

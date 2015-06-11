(in-package :lem)

(defgeneric file-open (buffer filename))

(defmethod file-open ((buffer buffer) filename)
  (let ((textbuf (make-textbuf filename)))
    (with-open-file (in filename)
      (do ((str #1=(read-line in nil) #1#))
	  ((null str))
        (textbuf-append-line textbuf str)))
    (buffer-change-textbuf buffer textbuf)))

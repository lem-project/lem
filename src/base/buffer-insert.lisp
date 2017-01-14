(in-package :lem-base)

(export '(*inhibit-read-only*
          *before-change-functions*
          *after-change-functions*))

(defvar *inhibit-read-only* nil)
(defvar *before-change-functions* '())
(defvar *after-change-functions* '())

(defun check-read-only-at-point (line charpos offset)
  (unless *inhibit-read-only*
    (when (if (eql offset 0)
              (line-search-property line :read-only charpos)
              (line-search-property-range line
                                          :read-only
                                          charpos
                                          (if (null offset)
                                              nil
                                              (+ charpos offset))))
      (error 'read-only-error))))

(defmacro with-modify-buffer (buffer &body body)
  (alexandria:once-only (buffer)
    `(progn
       (unless *inhibit-read-only*
         (check-read-only-buffer ,buffer))
       (prog1 (progn ,@body)
         (buffer-modify ,buffer)))))

(defun shift-sticky-objects (line charpos n)
  (line-property-insert-pos line charpos n)
  (dolist (p (line-points line))
    (when (etypecase (point-kind p)
            ((eql :left-inserting)
             (<= charpos (point-charpos p)))
            ((eql :right-inserting)
             (< charpos (point-charpos p))))
      (incf (point-charpos p) n))))

(defun shift-sticky-objects-newline (line charpos)
  (line-property-insert-newline line (line-next line) charpos)
  (dolist (p (line-points line))
    (when (etypecase (point-kind p)
            ((eql :left-inserting)
             (<= charpos (point-charpos p)))
            ((eql :right-inserting)
             (< charpos (point-charpos p))))
      (point-change-line p (line-next line))
      (decf (point-charpos p) charpos))))

(defun shift-sticky-objects-subtract (point n)
  (let ((line (point-line point))
        (charpos (point-charpos point)))
    (line-property-delete-pos line charpos n)
    (dolist (p (line-points line))
      (when (< charpos (point-charpos p))
        (setf (point-charpos p)
              (if (> charpos (- (point-charpos p) n))
                  charpos
                  (- (point-charpos p) n)))))))

(defun shift-sticky-objects-subtract-line (point)
  (let* ((line (point-line point))
         (charpos (point-charpos point)))
    (line-property-delete-line line charpos)
    (dolist (p (line-points line))
      (when (< charpos (point-charpos p))
        (setf (point-charpos p) charpos)))
    (when (line-next line)
      (dolist (p (line-points (line-next line)))
        (point-change-line p line)
        (incf (point-charpos p) charpos))
      (line-merge line (line-next line) charpos))))

(defun %insert-newline/point (buffer line charpos)
  (make-line buffer
             line
             (line-next line)
             (subseq (line-str line) charpos))
  (shift-sticky-objects-newline line charpos)
  (incf (buffer-nlines buffer))
  (setf (line-str line)
        (subseq (line-str line) 0 charpos)))

(defgeneric insert-char/point (point char)
  (:method (point char)
    (with-modify-buffer (point-buffer point)
      (check-read-only-at-point (point-line point) (point-charpos point) 0)
      (cond
        ((char= char #\newline)
         (%insert-newline/point (point-buffer point)
                                (point-line point)
				(point-charpos point)))
        (t
         (let ((line (point-line point))
               (charpos (point-charpos point)))
           (shift-sticky-objects line charpos 1)
           (setf (line-str line)
                 (concatenate 'string
                              (subseq (line-str line) 0 charpos)
                              (string char)
                              (subseq (line-str line) charpos))))))
      char)))

(defun %insert-line-string/point (line charpos string)
  (check-read-only-at-point line charpos 0)
  (shift-sticky-objects line charpos (length string))
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 charpos)
                     string
                     (subseq (line-str line) charpos))))

(defgeneric insert-string/point (point string)
  (:method (point string)
    (let ((buffer (point-buffer point)))
      (with-modify-buffer buffer
        (loop :with start := 0
              :for pos := (position #\newline string :start start)
              :for line := (point-line point) :then (line-next line)
              :for charpos := (point-charpos point) :then 0
              :do (cond ((null pos)
                         (%insert-line-string/point line charpos
                                                    (subseq string start))
                         (return))
                        (t
                         (let ((substr (subseq string start pos)))
                           (%insert-line-string/point line charpos substr)
                           (%insert-newline/point buffer
                                                  line
                                                  (+ charpos (length substr)))
                           (setf start (1+ pos))))))))
    string))

(defun %delete-line-between/point (point start end)
  (declare (special killring-stream line))
  (shift-sticky-objects-subtract point (- end start))
  (write-string (line-str line) killring-stream
                :start start
                :end end)
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (subseq (line-str line) end))))

(defun %delete-line-eol/point (point start)
  (declare (special killring-stream line))
  (shift-sticky-objects-subtract-line point)
  (write-string (line-str line) killring-stream :start start)
  (setf (line-str line)
        (subseq (line-str line) 0 start)))

(defun %delete-line/point (point start)
  (declare (special killring-stream line buffer n))
  (shift-sticky-objects-subtract-line point)
  (write-string (line-str line) killring-stream :start start)
  (write-char #\newline killring-stream)
  (unless (eq n 'T)
    (decf n (1+ (- (line-length line) start))))
  (decf (buffer-nlines buffer))
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (line-str (line-next line))))
  (line-free (line-next line)))

(defgeneric delete-char/point (point n)
  (:method (point n)
    (declare (special n))
    (with-modify-buffer (point-buffer point)
      (with-output-to-string (killring-stream)
        (declare (special killring-stream))
        (let ((charpos (point-charpos point))
              (buffer (point-buffer point))
              (line (point-line point)))
          (declare (special buffer line))
          (loop :while (or (eq n 'T) (plusp n))
	     :for eolp := (or (eq n 'T)
			      (> n (- (line-length line) charpos)))
	     :do (check-read-only-at-point (point-line point)
                                           (point-charpos point)
                                           (if (eq n 'T) nil (if eolp n nil)))
	     :do (cond
		   ((not eolp)
		    (%delete-line-between/point point charpos (+ charpos n))
		    (return))
		   ((null (line-next line))
		    (%delete-line-eol/point point charpos)
		    (return))
		   (t
		    (%delete-line/point point charpos)))))))))


(defmacro insert/after-change-function (point arg)
  `(if *after-change-functions*
       (with-point ((start ,point))
         (prog1 (call-next-method)
           (with-point ((end start))
             (character-offset end ,arg)
             (run-hooks *after-change-functions* start end 0))))
       (call-next-method)))

(defmacro delete/after-change-function (point)
  `(if *after-change-functions*
       (let ((string (call-next-method)))
         (with-point ((start ,point)
                      (end ,point))
           (run-hooks *after-change-functions* start end (length string)))
         string)
       (call-next-method)))

(defmethod insert-char/point :around (point char)
  (run-hooks *before-change-functions* point 1)
  (without-interrupts
    (let ((pos (position-at-point point)))
      (prog1 (insert/after-change-function point 1)
        (push-undo (point-buffer point)
                   (lambda (cur-point)
                     (move-to-position cur-point pos)
                     (delete-char/point cur-point 1)
                     t))))))

(defmethod insert-string/point :around (point string)
  (run-hooks *before-change-functions* point (length string))
  (without-interrupts
    (let ((pos (position-at-point point)))
      (prog1 (insert/after-change-function point (length string))
        (push-undo (point-buffer point)
                   (lambda (cur-point)
                     (move-to-position cur-point pos)
                     (delete-char/point cur-point (length string))
                     t))))))

(defmethod delete-char/point :around (point n)
  (run-hooks *before-change-functions* point (- n))
  (without-interrupts
    (let ((pos (position-at-point point))
          (string (delete/after-change-function point)))
      (push-undo (point-buffer point)
                 (lambda (cur-point)
                   (move-to-position cur-point pos)
                   (insert-string/point cur-point string)
                   t))
      string)))

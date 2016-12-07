(in-package :lem)

(export '(*inhibit-read-only*))

(defvar *inhibit-read-only* nil)

(defun get-line/marker (marker)
  (buffer-get-line (marker-buffer marker)
                   (marker-linum marker)))

(defun check-read-only-at-point (marker offset)
  (unless *inhibit-read-only*
    (let ((line (get-line/marker marker))
          (charpos (marker-charpos marker)))
      (when (if (eql offset 0)
                (line-search-property line 'read-only charpos)
                (line-search-property-range line
                                            'read-only
                                            charpos
                                            (if (null offset)
                                                nil
                                                (+ charpos offset))))
        (error 'read-only-error)))))

(defmacro with-modify-buffer (buffer &body body)
  (alexandria:once-only (buffer)
    `(progn
       (unless *inhibit-read-only*
         (check-read-only-buffer ,buffer))
       (prog1 (progn ,@body)
         (buffer-modify ,buffer)))))

(defun shift-sticky-objects (marker n)
  (let ((buffer (marker-buffer marker))
        (line (get-line/marker marker))
        (linum (marker-linum marker))
        (charpos (marker-charpos marker)))
    (line-property-insert-pos line charpos n)
    (dolist (m (buffer-markers buffer))
      (when (and (= linum (marker-linum m))
                 (etypecase (marker-kind m)
                   ((eql :left-inserting)
                    (<= charpos (marker-charpos m)))
                   ((eql :right-inserting)
                    (< charpos (marker-charpos m)))
                   ((eql :temporary)
                    nil)))
        (incf (marker-charpos m) n)))))

(defun shift-sticky-objects-newline (marker)
  (let ((linum (marker-linum marker))
        (charpos (marker-charpos marker))
        (buffer (marker-buffer marker))
        (line (get-line/marker marker)))
    (line-property-insert-newline line (line-next line) charpos)
    (dolist (m (buffer-markers buffer))
      (cond ((and (= (marker-linum m) linum)
                  (etypecase (marker-kind m)
                    ((eql :left-inserting)
                     (<= charpos (marker-charpos m)))
                    ((eql :right-inserting)
                     (< charpos (marker-charpos m)))
                    ((eql :temporary)
                     nil)))
             (incf (marker-linum m))
             (decf (marker-charpos m) charpos))
            ((< linum (marker-linum m))
             (incf (marker-linum m)))))))

(defun shift-sticky-objects-subtract (marker n)
  (let ((line (get-line/marker marker))
        (linum (marker-linum marker))
        (charpos (marker-charpos marker))
        (buffer (marker-buffer marker)))
    (line-property-delete-pos line charpos n)
    (dolist (m (buffer-markers buffer))
      (when (and (= linum (marker-linum m))
                 (< charpos (marker-charpos m)))
        (setf (marker-charpos m)
              (if (> charpos (- (marker-charpos m) n))
                  charpos
                  (- (marker-charpos m) n)))))))

(defun merge-plist (plist1 plist2)
  (let ((new-plist '()))
    (flet ((f (plist)
             (loop :for (k v) :on plist :by #'cddr
                   :do (setf (getf new-plist k)
                             (nconc (getf new-plist k) v)))))
      (f plist1)
      (f plist2))
    new-plist))

(defun shift-sticky-objects-subtract-line (marker nextp)
  (let ((line (get-line/marker marker))
        (linum (marker-linum marker))
        (charpos (marker-charpos marker))
        (buffer (marker-buffer marker)))
    (line-property-delete-line line charpos)
    (dolist (m (buffer-markers buffer))
      (cond ((and (= linum (marker-linum m))
                  (< charpos (marker-charpos m)))
             (setf (marker-charpos m) charpos))
            ((< linum (marker-linum m))
             (decf (marker-linum m)))))
    (when nextp
      (setf (line-plist line)
            (merge-plist
             (line-plist line)
             (loop :for (key elements) :on (line-plist (line-next line)) :by #'cddr
                   :append (let ((new-elements
                                  (loop :for (start end value) :in elements
                                        :collect (list (+ start charpos)
                                                       (+ end charpos)
                                                       value))))
                             (when new-elements
                               (list key new-elements)))))))))

(defun %insert-newline/marker (marker linum charpos)
  (let* ((buffer (marker-buffer marker))
         (line (buffer-get-line buffer linum)))
    (let ((newline (make-line line
                              (line-next line)
                              (subseq (line-str line) charpos))))
      (shift-sticky-objects-newline marker)
      (incf (buffer-nlines buffer))
      (when (eq line (buffer-tail-line buffer))
        (setf (buffer-tail-line buffer) newline))
      (setf (line-str line)
            (subseq (line-str line) 0 charpos)))))

(defgeneric insert-char/marker (marker char)
  (:method (marker char)
    (with-modify-buffer (marker-buffer marker)
      (check-read-only-at-point marker 0)
      (cond
        ((char= char #\newline)
         (%insert-newline/marker marker
                                 (marker-linum marker)
                                 (marker-charpos marker)))
        (t
         (let ((line (get-line/marker marker))
               (charpos (marker-charpos marker)))
           (shift-sticky-objects marker 1)
           (setf (line-str line)
                 (concatenate 'string
                              (subseq (line-str line) 0 charpos)
                              (string char)
                              (subseq (line-str line) charpos))))))
      char)))

(defun %insert-line-string/marker (marker linum charpos string)
  (check-read-only-at-point marker 0)
  (shift-sticky-objects marker (length string))
  (let ((line
         (buffer-get-line (marker-buffer marker)
                          linum)))
    (setf (line-str line)
          (concatenate 'string
                       (subseq (line-str line) 0 charpos)
                       string
                       (subseq (line-str line) charpos)))))

(defgeneric insert-string/marker (marker string)
  (:method (marker string)
    (with-modify-buffer (marker-buffer marker)
      (loop :with start := 0
            :for pos := (position #\newline string :start start)
            :for linum :from (marker-linum marker) :by 1
            :for charpos := (marker-charpos marker) :then 0
            :do (if (null pos)
                    (progn
                      (%insert-line-string/marker marker linum charpos (subseq string start))
                      (return))
                    (let ((substr (subseq string start pos)))
                      (%insert-line-string/marker marker linum charpos substr)
                      (%insert-newline/marker marker linum (+ charpos (length substr)))
                      (setf start (1+ pos))))))
    string))

(defun %delete-line-between/marker (marker start end)
  (declare (special killring-stream line))
  (shift-sticky-objects-subtract marker (- end start))
  (write-string (line-str line) killring-stream
                :start start
                :end end)
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (subseq (line-str line) end))))

(defun %delete-line-eol/marker (marker start)
  (declare (special killring-stream line))
  (shift-sticky-objects-subtract-line marker nil)
  (write-string (line-str line) killring-stream :start start)
  (setf (line-str line)
        (subseq (line-str line) 0 start)))

(defun %delete-line/marker (marker start)
  (declare (special killring-stream line buffer n))
  (shift-sticky-objects-subtract-line marker t)
  (write-string (line-str line) killring-stream :start start)
  (write-char #\newline killring-stream)
  (unless (eq n 'T)
    (decf n (1+ (- (line-length line) start))))
  (decf (buffer-nlines buffer))
  (when (eq (line-next line)
            (buffer-tail-line buffer))
    (setf (buffer-tail-line buffer) line))
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (line-str (line-next line))))
  (line-free (line-next line)))

(defgeneric delete-char/marker (marker n)
  (:method (marker n)
    (declare (special n))
    (with-modify-buffer (marker-buffer marker)
      (with-output-to-string (killring-stream)
        (declare (special killring-stream))
        (let ((charpos (marker-charpos marker))
              (buffer (marker-buffer marker))
              (line (get-line/marker marker)))
          (declare (special buffer line))
          (loop :while (or (eq n 'T) (plusp n))
                :for eolp := (or (eq n 'T)
                                 (> n (- (line-length line) charpos)))
                :do (check-read-only-at-point marker (if (eq n 'T) nil (if eolp n nil)))
                :do (cond
                      ((not eolp)
                       (%delete-line-between/marker marker charpos (+ charpos n))
                       (return))
                      ((null (line-next line))
                       (%delete-line-eol/marker marker charpos)
                       (return))
                      (t
                       (%delete-line/marker marker charpos)))))))))

(defmethod insert-char/marker :around (marker char)
  (let ((point (marker-point marker)))
    (prog1 (call-next-method)
      (push-undo marker
                 (lambda ()
                   (setf (marker-point marker) point)
                   (delete-char/marker marker 1))))))

(defmethod insert-string/marker :around (marker string)
  (let ((point (marker-point marker)))
    (prog1 (call-next-method)
      (push-undo marker
                 (lambda ()
                   (setf (marker-point marker) point)
                   (delete-char/marker marker (length string)))))))

(defmethod delete-char/marker :around (marker n)
  (let* ((point (marker-point marker))
         (string (call-next-method)))
    (push-undo marker
               (lambda ()
                 (setf (marker-point marker) point)
                 (insert-string/marker marker string)))
    string))

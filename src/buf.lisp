(in-package :lem)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defgeneric marker/insert-char (marker char))
(defgeneric marker/insert-string (marker string))
(defgeneric marker/delete-char (marker n))

(defun marker-line (marker)
  (buffer-get-line (marker-buffer marker)
                   (marker-linum marker)))

(defun push-undo (marker fn)
  (let ((buffer (marker-buffer marker)))
    (when (and (buffer-enable-undo-p buffer)
               (not (ghost-buffer-p buffer)))
      (let* ((point (marker-point marker))
             (elt (lambda ()
                    (funcall fn)
                    point)))
        (ecase *undo-mode*
          (:edit
           (push-undo-stack buffer elt)
           (setf (buffer-redo-stack buffer) nil))
          (:redo
           (push-undo-stack buffer elt))
          (:undo
           (push-redo-stack buffer elt)))))))

(defun check-read-only-at-point (marker offset)
  (let ((line (marker-line marker))
        (charpos (marker-charpos marker)))
    (when (if (eql offset 0)
              (line-search-property line 'read-only charpos)
              (line-search-property-range line
                                          'read-only
                                          charpos
                                          (if (null offset)
                                              nil
                                              (+ charpos offset))))
      (error 'readonly))))

(defmacro with-modify-buffer (buffer &body body)
  (alexandria:once-only (buffer)
    `(progn
       (check-read-only-buffer ,buffer)
       (prog1 (progn ,@body)
         (buffer-modify ,buffer)))))

(defun shift-sticky-objects (marker n)
  (let ((buffer (marker-buffer marker))
        (line (marker-line marker))
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
                   (null
                    nil)))
        (incf (marker-charpos m) n)))))

(defun shift-sticky-objects-newline (marker)
  (let ((linum (marker-linum marker))
        (charpos (marker-charpos marker))
        (buffer (marker-buffer marker))
        (line (marker-line marker)))
    (line-property-insert-newline line (line-next line) charpos)
    (dolist (m (buffer-markers buffer))
      (cond ((and (= (marker-linum m) linum)
                  (etypecase (marker-kind m)
                    ((eql :left-inserting)
                     (<= charpos (marker-charpos m)))
                    ((eql :right-inserting)
                     (< charpos (marker-charpos m)))
                    (null
                     nil)))
             (incf (marker-linum m))
             (decf (marker-charpos m) charpos))
            ((< linum (marker-linum m))
             (incf (marker-linum m)))))))

(defun shift-sticky-objects-subtract (marker n)
  (let ((line (marker-line marker))
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

(defun shift-sticky-objects-subtract-line (marker nextp)
  (let ((line (marker-line marker))
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

(defun %marker/insert-newline (marker linum charpos)
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

(defmethod marker/insert-char (marker char)
  (check-type marker marker)
  (check-type char character)
  (with-modify-buffer (marker-buffer marker)
    (check-read-only-at-point marker 0)
    (cond
      ((char= char #\newline)
       (%marker/insert-newline marker
                               (marker-linum marker)
                               (marker-charpos marker)))
      (t
       (let ((line (marker-line marker))
             (charpos (marker-charpos marker)))
         (shift-sticky-objects marker 1)
         (setf (line-str line)
               (concatenate 'string
                            (subseq (line-str line) 0 charpos)
                            (string char)
                            (subseq (line-str line) charpos))))))
    char))

(defun %marker/insert-line-string (marker linum charpos string)
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

(defmethod marker/insert-string (marker string)
  (check-type marker marker)
  (check-type string string)
  (with-modify-buffer (marker-buffer marker)
    (loop :with start := 0
          :for pos := (position #\newline string :start start)
          :for linum :from (marker-linum marker) :by 1
          :for charpos := (marker-charpos marker) :then 0
          :do (if (null pos)
                  (progn
                    (%marker/insert-line-string marker linum charpos (subseq string start))
                    (return))
                  (let ((substr (subseq string start pos)))
                    (%marker/insert-line-string marker linum charpos substr)
                    (%marker/insert-newline marker linum (+ charpos (length substr)))
                    (setf start (1+ pos))))))
  string)

(defun %marker/delete-line-between (marker start end)
  (declare (special killring-stream line))
  (shift-sticky-objects-subtract marker (- end start))
  (write-string (line-str line) killring-stream
                :start start
                :end end)
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (subseq (line-str line) end))))

(defun %marker/delete-line-eol (marker start)
  (declare (special killring-stream line))
  (shift-sticky-objects-subtract-line marker nil)
  (write-string (line-str line) killring-stream :start start)
  (setf (line-str line)
        (subseq (line-str line) 0 start)))

(defun %marker/delete-line (marker start)
  (declare (special killring-stream line buffer n))
  (shift-sticky-objects-subtract-line marker t)
  (write-string (line-str line) killring-stream :start start)
  (write-char #\newline killring-stream)
  (decf n (1+ (- (line-length line) start)))
  (decf (buffer-nlines buffer))
  (when (eq (line-next line)
            (buffer-tail-line buffer))
    (setf (buffer-tail-line buffer) line))
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (line-str (line-next line))))
  (line-free (line-next line)))

(defmethod marker/delete-char (marker n)
  (declare (special n))
  (check-type marker marker)
  (check-type n integer)
  (with-modify-buffer (marker-buffer marker)
    (with-output-to-string (killring-stream)
      (declare (special killring-stream))
      (let ((charpos (marker-charpos marker))
            (buffer (marker-buffer marker))
            (line (marker-line marker)))
        (declare (special buffer line))
        (loop :while (plusp n)
              :for eolp := (> n (- (line-length line) charpos))
              :do (check-read-only-at-point marker (if eolp n nil))
              :do (cond
                    ((not eolp)
                     (%marker/delete-line-between marker charpos (+ charpos n))
                     (return))
                    ((null (line-next line))
                     (%marker/delete-line-eol marker charpos)
                     (return))
                    (t
                     (%marker/delete-line marker charpos))))))))

(defmethod marker/insert-char :around (marker char)
  (let ((point (marker-point marker)))
    (prog1 (call-next-method)
      (push-undo marker
                 (lambda ()
                   (setf (marker-point marker) point)
                   (marker/delete-char marker 1))))))

(defmethod marker/insert-string :around (marker string)
  (let ((point (marker-point marker)))
    (prog1 (call-next-method)
      (push-undo marker
                 (lambda ()
                   (setf (marker-point marker) point)
                   (marker/delete-char marker (length string)))))))

(defmethod marker/delete-char :around (marker n)
  (let* ((point (marker-point marker))
         (string (call-next-method)))
    (push-undo marker
               (lambda ()
                 (setf (marker-point marker) point)
                 (marker/insert-string marker string)))
    string))

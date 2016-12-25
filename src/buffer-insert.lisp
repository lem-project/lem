(in-package :lem)

(export '(*inhibit-read-only*))

(defvar *inhibit-read-only* nil)

(defun get-line/point (marker)
  (buffer-get-line (point-buffer marker)
                   (point-linum marker)))

(defun check-read-only-at-point (marker offset)
  (unless *inhibit-read-only*
    (let ((line (get-line/point marker))
          (charpos (point-charpos marker)))
      (when (if (eql offset 0)
                (line-search-property line 'lem.property:read-only charpos)
                (line-search-property-range line
                                            'lem.property:read-only
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
  (let ((buffer (point-buffer marker))
        (line (get-line/point marker))
        (linum (point-linum marker))
        (charpos (point-charpos marker)))
    (line-property-insert-pos line charpos n)
    (dolist (m (buffer-points buffer))
      (when (and (= linum (point-linum m))
                 (etypecase (point-kind m)
                   ((eql :left-inserting)
                    (<= charpos (point-charpos m)))
                   ((eql :right-inserting)
                    (< charpos (point-charpos m)))
                   ((eql :temporary)
                    nil)))
        (incf (point-charpos m) n)))))

(defun shift-sticky-objects-newline (marker)
  (let ((linum (point-linum marker))
        (charpos (point-charpos marker))
        (buffer (point-buffer marker))
        (line (get-line/point marker)))
    (line-property-insert-newline line (line-next line) charpos)
    (dolist (m (buffer-points buffer))
      (cond ((and (= (point-linum m) linum)
                  (etypecase (point-kind m)
                    ((eql :left-inserting)
                     (<= charpos (point-charpos m)))
                    ((eql :right-inserting)
                     (< charpos (point-charpos m)))
                    ((eql :temporary)
                     nil)))
             (incf (point-linum m))
             (decf (point-charpos m) charpos))
            ((< linum (point-linum m))
             (incf (point-linum m)))))))

(defun shift-sticky-objects-subtract (marker n)
  (let ((line (get-line/point marker))
        (linum (point-linum marker))
        (charpos (point-charpos marker))
        (buffer (point-buffer marker)))
    (line-property-delete-pos line charpos n)
    (dolist (m (buffer-points buffer))
      (when (and (= linum (point-linum m))
                 (< charpos (point-charpos m)))
        (setf (point-charpos m)
              (if (> charpos (- (point-charpos m) n))
                  charpos
                  (- (point-charpos m) n)))))))

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
  (let ((line (get-line/point marker))
        (linum (point-linum marker))
        (charpos (point-charpos marker))
        (buffer (point-buffer marker)))
    (line-property-delete-line line charpos)
    (dolist (m (buffer-points buffer))
      (cond ((and (= linum (point-linum m))
                  (< charpos (point-charpos m)))
             (setf (point-charpos m) charpos))
            ((< linum (point-linum m))
             (decf (point-linum m)))))
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

(defun %insert-newline/point (marker linum charpos)
  (let* ((buffer (point-buffer marker))
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

(defgeneric insert-char/point (marker char)
  (:method (marker char)
    (with-modify-buffer (point-buffer marker)
      (check-read-only-at-point marker 0)
      (cond
        ((char= char #\newline)
         (%insert-newline/point marker
                                 (point-linum marker)
                                 (point-charpos marker)))
        (t
         (let ((line (get-line/point marker))
               (charpos (point-charpos marker)))
           (shift-sticky-objects marker 1)
           (setf (line-str line)
                 (concatenate 'string
                              (subseq (line-str line) 0 charpos)
                              (string char)
                              (subseq (line-str line) charpos))))))
      char)))

(defun %insert-line-string/point (marker linum charpos string)
  (check-read-only-at-point marker 0)
  (shift-sticky-objects marker (length string))
  (let ((line
         (buffer-get-line (point-buffer marker)
                          linum)))
    (setf (line-str line)
          (concatenate 'string
                       (subseq (line-str line) 0 charpos)
                       string
                       (subseq (line-str line) charpos)))))

(defgeneric insert-string/point (marker string)
  (:method (marker string)
    (with-modify-buffer (point-buffer marker)
      (loop :with start := 0
            :for pos := (position #\newline string :start start)
            :for linum :from (point-linum marker) :by 1
            :for charpos := (point-charpos marker) :then 0
            :do (if (null pos)
                    (progn
                      (%insert-line-string/point marker linum charpos (subseq string start))
                      (return))
                    (let ((substr (subseq string start pos)))
                      (%insert-line-string/point marker linum charpos substr)
                      (%insert-newline/point marker linum (+ charpos (length substr)))
                      (setf start (1+ pos))))))
    string))

(defun %delete-line-between/point (marker start end)
  (declare (special killring-stream line))
  (shift-sticky-objects-subtract marker (- end start))
  (write-string (line-str line) killring-stream
                :start start
                :end end)
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (subseq (line-str line) end))))

(defun %delete-line-eol/point (marker start)
  (declare (special killring-stream line))
  (shift-sticky-objects-subtract-line marker nil)
  (write-string (line-str line) killring-stream :start start)
  (setf (line-str line)
        (subseq (line-str line) 0 start)))

(defun %delete-line/point (marker start)
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

(defgeneric delete-char/point (marker n)
  (:method (marker n)
    (declare (special n))
    (with-modify-buffer (point-buffer marker)
      (with-output-to-string (killring-stream)
        (declare (special killring-stream))
        (let ((charpos (point-charpos marker))
              (buffer (point-buffer marker))
              (line (get-line/point marker)))
          (declare (special buffer line))
          (loop :while (or (eq n 'T) (plusp n))
                :for eolp := (or (eq n 'T)
                                 (> n (- (line-length line) charpos)))
                :do (check-read-only-at-point marker (if (eq n 'T) nil (if eolp n nil)))
                :do (cond
                      ((not eolp)
                       (%delete-line-between/point marker charpos (+ charpos n))
                       (return))
                      ((null (line-next line))
                       (%delete-line-eol/point marker charpos)
                       (return))
                      (t
                       (%delete-line/point marker charpos)))))))))

(defmethod insert-char/point :around (marker char)
  (let ((point (copy-point marker :temporary)))
    (prog1 (call-next-method)
      (push-undo marker
                 (lambda ()
                   (move-point marker point)
                   (delete-char/point marker 1)
                   point)))))

(defmethod insert-string/point :around (marker string)
  (let ((point (copy-point marker :temporary)))
    (prog1 (call-next-method)
      (push-undo marker
                 (lambda ()
                   (move-point marker point)
                   (delete-char/point marker (length string))
                   point)))))

(defmethod delete-char/point :around (marker n)
  (let ((point (copy-point marker :temporary))
        (string (call-next-method)))
    (push-undo marker
               (lambda ()
                 (move-point marker point)
                 (insert-string/point marker string)
                 point))
    string))

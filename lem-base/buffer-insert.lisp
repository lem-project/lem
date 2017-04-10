(in-package :lem-base)

(export '(*inhibit-read-only*
          *inhibit-modification-hooks*
          before-change-functions
          after-change-functions))

(defvar *inhibit-read-only* nil)
(defvar *inhibit-modification-hooks* nil)

(define-editor-variable before-change-functions '())
(define-editor-variable after-change-functions '())

(defun check-read-only-at-point (point offset)
  (unless *inhibit-read-only*
    (let ((line (point-line point))
          (charpos (point-charpos point)))
      (when (if (eql offset 0)
                (line-search-property line :read-only charpos)
                (line-search-property-range line
                                            :read-only
                                            charpos
                                            (if (null offset)
                                                nil
                                                (+ charpos offset))))
        (error 'read-only-error)))))

(defmacro with-modify-buffer (buffer &body body)
  (alexandria:once-only (buffer)
    `(without-interrupts
       (unless *inhibit-read-only*
         (check-read-only-buffer ,buffer))
       (prog1 (progn ,@body)
         (buffer-modify ,buffer)))))

(defun line-next-n (line n)
  (loop :repeat n
        :do (setf line (line-next line)))
  line)

(defun shift-markers (point offset-line offset-char)
  (cond ((and (= 0 offset-line)
              (< 0 offset-char))
         (let ((charpos (point-charpos point)))
           (dolist (p (line-points (point-line point)))
             (when (etypecase (point-kind p)
                     ((eql :left-inserting)
                      (<= charpos (point-charpos p)))
                     ((eql :right-inserting)
                      (< charpos (point-charpos p))))
               (incf (point-charpos p) offset-char)))))
        ((< 0 offset-line)
         (let ((linum (point-linum point))
               (charpos (point-charpos point))
               (line (line-next-n (point-line point) offset-line)))
           (dolist (p (buffer-points (point-buffer point)))
             (cond ((and (= linum (point-linum p))
                         (etypecase (point-kind p)
                           ((eql :left-inserting)
                            (<= charpos (point-charpos p)))
                           ((eql :right-inserting)
                            (< charpos (point-charpos p)))))
                    (incf (point-linum p) offset-line)
                    (decf (point-charpos p) charpos)
                    (incf (point-charpos p) offset-char)
                    (point-change-line p (+ linum offset-line) line))
                   ((< linum (point-linum p))
                    (incf (point-linum p) offset-line))))))
        ((and (= 0 offset-line)
              (> 0 offset-char))
         (let ((charpos (point-charpos point))
               (n (- offset-char)))
           (dolist (p (line-points (point-line point)))
             (when (< charpos (point-charpos p))
               (setf (point-charpos p)
                     (if (> charpos (- (point-charpos p) n))
                         charpos
                         (- (point-charpos p) n)))))))
        ((> 0 offset-line)
         (let ((linum (point-linum point))
               (charpos (point-charpos point))
               (line (point-line point))
               (offset-line (abs offset-line))
               (offset-char (abs offset-char)))
           (dolist (p (buffer-points (point-buffer point)))
             (when (<= linum (point-linum p))
               (cond ((<= (- (point-linum p) offset-line)
                          linum)
                      (setf (point-charpos p)
                            (if (= (- (point-linum p) offset-line)
                                   linum)
                                (+ charpos (max 0 (- (point-charpos p) offset-char)))
                                charpos))
                      (point-change-line p linum line)
                      (setf (point-linum p) linum))
                     (t
                      (decf (point-linum p) offset-line)))))))))

(defun %insert-newline/point (buffer line charpos)
  (make-line line
             (line-next line)
             (subseq (line-str line) charpos))
  (line-property-insert-newline line (line-next line) charpos)
  (incf (buffer-nlines buffer))
  (setf (line-str line)
        (subseq (line-str line) 0 charpos)))

(defgeneric insert-char/point (point char)
  (:method (point char)
    (with-modify-buffer (point-buffer point)
      (check-read-only-at-point point 0)
      (cond
        ((char= char #\newline)
         (%insert-newline/point (point-buffer point)
                                (point-line point)
				(point-charpos point))
         (shift-markers point 1 0))
        (t
         (let ((line (point-line point))
               (charpos (point-charpos point)))
           (line-property-insert-pos line charpos 1)
           (shift-markers point 0 1)
           (setf (line-str line)
                 (concatenate 'string
                              (subseq (line-str line) 0 charpos)
                              (string char)
                              (subseq (line-str line) charpos))))))
      char)))

(defun %insert-line-string/point (line charpos string)
  (line-property-insert-pos line charpos (length string))
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 charpos)
                     string
                     (subseq (line-str line) charpos))))

(defgeneric insert-string/point (point string)
  (:method (point string)
    (let ((buffer (point-buffer point)))
      (with-modify-buffer buffer
        (check-read-only-at-point point 0)
        (loop :with start := 0
              :for pos := (position #\newline string :start start)
              :for line := (point-line point) :then (line-next line)
              :for charpos := (point-charpos point) :then 0
              :for offset-line :from 0
              :do (cond ((null pos)
                         (let ((substr (if (= start 0) string (subseq string start))))
                           (%insert-line-string/point line charpos substr)
                           (shift-markers point offset-line (length substr)))
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
  (line-property-delete-pos (point-line point)
                            (point-charpos point)
                            (- end start))
  (write-string (line-str line) killring-stream
                :start start
                :end end)
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (subseq (line-str line) end))))

(defun %delete-line-eol/point (point start)
  (declare (special killring-stream line))
  (line-property-delete-line (point-line point) (point-charpos point))
  (write-string (line-str line) killring-stream :start start)
  (setf (line-str line)
        (subseq (line-str line) 0 start)))

(defun %delete-line/point (point start)
  (declare (special killring-stream line buffer n))
  (line-property-delete-line (point-line point) (point-charpos point))
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
              (line (point-line point))
              (offset-line 0))
          (declare (special buffer line))
          (loop :while (or (eq n 'T) (plusp n))
                :for eolp := (or (eq n 'T)
                                 (> n (- (line-length line) charpos)))
                :do
                (check-read-only-at-point point (if (eq n 'T) nil (if eolp n nil)))
                (cond
                  ((not eolp)
                   (%delete-line-between/point point charpos (+ charpos n))
                   (shift-markers point offset-line (- n))
                   (return))
                  ((null (line-next line))
                   (%delete-line-eol/point point charpos)
                   (shift-markers point offset-line (- charpos (line-length line)))
                   (return))
                  (t
                   (%delete-line/point point charpos)))
                 (decf offset-line)
                 :finally (shift-markers point offset-line 0)))))))


(declaim (inline call-before-change-functions
                 call-after-change-functions))

(defun call-before-change-functions (buffer start n)
  (unless *inhibit-modification-hooks*
    (alexandria:when-let ((hooks (variable-value 'before-change-functions :buffer buffer)))
      (run-hooks hooks start n))
    (alexandria:when-let ((hooks (variable-value 'before-change-functions :global)))
      (run-hooks hooks start n))))

(defun call-after-change-functions (buffer start end old-len)
  (unless *inhibit-modification-hooks*
    (alexandria:when-let ((hooks (variable-value 'after-change-functions :buffer buffer)))
      (run-hooks hooks start end old-len))
    (alexandria:when-let ((hooks (variable-value 'after-change-functions :global)))
      (run-hooks hooks start end old-len))))

(defmacro insert/after-change-function (point arg)
  `(if (and (not *inhibit-modification-hooks*)
            (variable-value 'after-change-functions))
       (with-point ((start ,point))
         (prog1 (call-next-method)
           (with-point ((end start))
             (character-offset end ,arg)
             (call-after-change-functions (point-buffer ,point) start end 0))))
       (call-next-method)))

(defmacro delete/after-change-function (point)
  `(if (and (not *inhibit-modification-hooks*)
            (variable-value 'after-change-functions))
       (let ((string (call-next-method)))
         (with-point ((start ,point)
                      (end ,point))
           (call-after-change-functions (point-buffer ,point) start end (length string)))
         string)
       (call-next-method)))

(defmethod insert-char/point :around (point char)
  (call-before-change-functions (point-buffer point) point 1)
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (insert/after-change-function point 1)
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point)))
        (prog1 (insert/after-change-function point 1)
          (push-undo (point-buffer point)
                     (lambda (cur-point)
                       (move-to-line cur-point linum)
                       (line-offset cur-point 0 charpos)
                       (delete-char/point cur-point 1)
                       t))))))

(defmethod insert-string/point :around (point string)
  (call-before-change-functions (point-buffer point) point (length string))
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (insert/after-change-function point (length string))
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point)))
        (prog1 (insert/after-change-function point (length string))
          (push-undo (point-buffer point)
                     (lambda (cur-point)
                       (move-to-line cur-point linum)
                       (line-offset cur-point 0 charpos)
                       (delete-char/point cur-point (length string))
                       t))))))

(defmethod delete-char/point :around (point n)
  (call-before-change-functions (point-buffer point) point (- n))
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (delete/after-change-function point)
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point))
            (string (delete/after-change-function point)))
        (push-undo (point-buffer point)
                   (lambda (cur-point)
                     (move-to-line cur-point linum)
                     (line-offset cur-point 0 charpos)
                     (insert-string/point cur-point string)
                     t))
        string)))

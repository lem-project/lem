(in-package :lem/buffer/internal)

(defvar *inhibit-read-only* nil
  "If T, disables read-only for `buffer`.")

(defvar *inhibit-modification-hooks* nil
  "If T, prevents `before-change-functions` and `after-change-functions` from being called.")

(define-editor-variable before-change-functions '())
(define-editor-variable after-change-functions '())

(defun check-read-only-at-point (point n)
  (loop :for line := (point-line point) :then (line-next line)
        :for charpos := (point-charpos point) :then 0
        :do (unless line
              (return))
            (when (line-search-property-range line :read-only charpos (+ charpos n))
              (error 'read-only-error))
            (when (>= 0 (decf n (1+ (- (line-length line) charpos))))
              (return))))

(defun call-with-modify-buffer (point n function)
  (without-interrupts
    (let ((buffer (point-buffer point)))
      (unless *inhibit-read-only*
        (check-read-only-buffer buffer)
        (check-read-only-at-point point n))
      (prog1 (funcall function)
        (buffer-modify buffer)))))

(defmacro with-modify-buffer ((point n) &body body)
  `(call-with-modify-buffer ,point ,n (lambda () ,@body)))

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
             (when (or (< linum (point-linum p))
                       (and (= linum (point-linum p))
                            (<= charpos (point-charpos p))))
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
  (lem/buffer/line:insert-newline line charpos)
  (incf (buffer-nlines buffer))
  (values))

(defgeneric insert-char/point (point char)
  (:method (point char)
    (with-modify-buffer (point 0)
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
      (with-modify-buffer (point 0)
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

(defun %delete-line-between/point (point start end line killring-stream)
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

(defun %delete-line-eol/point (point start line killring-stream)
  (line-property-delete-line (point-line point) (point-charpos point))
  (write-string (line-str line) killring-stream :start start)
  (setf (line-str line)
        (subseq (line-str line) 0 start)))

(defun %delete-line/point (point start line killring-stream remaining-deletions)
  (line-property-delete-line (point-line point) (point-charpos point))
  (line-merge (point-line point) (line-next (point-line point)) start)
  (write-string (line-str line) killring-stream :start start)
  (write-char #\newline killring-stream)
  (decf remaining-deletions (1+ (- (line-length line) start)))
  (decf (buffer-nlines (point-buffer point)))
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (line-str (line-next line))))
  (line-free (line-next line))
  remaining-deletions)

(defgeneric delete-char/point (point remaining-deletions)
  (:method (point remaining-deletions)
    (with-modify-buffer (point remaining-deletions)
      (with-output-to-string (killring-stream)
        (let ((charpos (point-charpos point))
              (line (point-line point))
              (offset-line 0))
          (loop :while (plusp remaining-deletions)
                :for eolp := (> remaining-deletions (- (line-length line) charpos))
                :do (cond
                      ((not eolp)
                       (%delete-line-between/point point
                                                   charpos
                                                   (+ charpos remaining-deletions)
                                                   line
                                                   killring-stream)
                       (shift-markers point
                                      offset-line
                                      (- remaining-deletions))
                       (return))
                      ((null (line-next line))
                       (%delete-line-eol/point point charpos line killring-stream)
                       (shift-markers point offset-line (- charpos (line-length line)))
                       (return))
                      (t
                       (setf remaining-deletions
                             (%delete-line/point point
                                                 charpos
                                                 line
                                                 killring-stream
                                                 remaining-deletions))))
                    (decf offset-line)
                :finally (shift-markers point offset-line 0)))))))


(defun call-before-change-functions (point arg)
  (unless *inhibit-modification-hooks*
    (run-hooks (make-per-buffer-hook :var 'before-change-functions :buffer (point-buffer point))
               point arg)))

(defun call-after-change-functions (buffer start end old-len)
  (unless *inhibit-modification-hooks*
    (run-hooks (make-per-buffer-hook :var 'after-change-functions :buffer buffer)
               start end old-len)))

(defun need-to-call-after-change-functions-p ()
  (and (not *inhibit-modification-hooks*)
       (or (variable-value 'after-change-functions)
           (variable-value 'after-change-functions :global))))

(defun insert/after-change-function (point arg call-next-method)
  (if (need-to-call-after-change-functions-p)
      (with-point ((start point))
        (prog1 (funcall call-next-method)
          (with-point ((end start))
            (character-offset end arg)
            (call-after-change-functions (point-buffer point) start end 0))))
      (funcall call-next-method)))

(defun delete/after-change-function (point call-next-method)
  (if (need-to-call-after-change-functions-p)
      (let ((string (funcall call-next-method)))
        (with-point ((start point)
                     (end point))
          (call-after-change-functions (point-buffer point) start end (length string)))
        string)
      (funcall call-next-method)))

(defmethod insert-char/point :around (point char)
  (call-before-change-functions point char)
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (insert/after-change-function point 1 #'call-next-method)
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point)))
        (prog1 (insert/after-change-function point 1 #'call-next-method)
          (without-interrupts
            (push-undo (point-buffer point)
                       (make-edit :insert-string linum charpos (string char))))))))

(defmethod insert-string/point :around (point string)
  (call-before-change-functions point string)
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (insert/after-change-function point (length string) #'call-next-method)
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point)))
        (prog1 (insert/after-change-function point (length string) #'call-next-method)
          (without-interrupts
            (push-undo (point-buffer point)
                       (make-edit :insert-string linum charpos string)))))))

(defmethod delete-char/point :around (point remaining-deletions)
  (call-before-change-functions point remaining-deletions)
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (delete/after-change-function point #'call-next-method)
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point))
            (string (delete/after-change-function point #'call-next-method)))
        (without-interrupts
          (push-undo (point-buffer point)
                     (make-edit :delete-string linum charpos string)))
        string)))

(in-package :lem-base/buffer)

(defvar *inhibit-read-only* nil
  "Tなら`buffer`のread-onlyを無効にします。")

(defvar *inhibit-modification-hooks* nil
  "Tなら`before-change-functions`と`after-change-functions`が実行されません。")

(define-editor-variable before-change-functions '())
(define-editor-variable after-change-functions '())

(defun step-on-read-only (point n)
  (loop :for line := (point-line point) :then (line-next line)
        :for charpos := (point-charpos point) :then 0
        :do (unless line
              (return nil))
            (when (line-search-property-range line :read-only charpos (+ charpos n))
              (return t))
            (when (>= 0 (decf n (1+ (- (line-length line) charpos))))
              (return nil))))

(defun check-read-only-at-point (point n)
  (unless *inhibit-read-only*
    (let ((line (point-line point))
          (charpos (point-charpos point)))
      (when (if (eql n 0)
                (line-search-property line :read-only charpos)
                (step-on-read-only point n))
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

(defun %delete-line-between/point (point start end line)
  (declare (special killring-stream))
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

(defun %delete-line-eol/point (point start line)
  (declare (special killring-stream))
  (line-property-delete-line (point-line point) (point-charpos point))
  (write-string (line-str line) killring-stream :start start)
  (setf (line-str line)
        (subseq (line-str line) 0 start)))

(defun %delete-line/point (point start line)
  (declare (special killring-stream buffer n))
  (line-property-delete-line (point-line point) (point-charpos point))
  (line-merge (point-line point) (line-next (point-line point)) start)
  (write-string (line-str line) killring-stream :start start)
  (write-char #\newline killring-stream)
  (decf n (1+ (- (line-length line) start)))
  (decf (buffer-nlines buffer))
  (setf (line-str line)
        (concatenate 'string
                     (subseq (line-str line) 0 start)
                     (line-str (line-next line))))
  (line-free (line-next line)))

;;TODO: ABCL complains about n that is not bound
#+abcl
(defparameter n nil)

(defgeneric delete-char/point (point n)
  (:method (point n)
    (declare (special n))
    (with-modify-buffer (point-buffer point)
      (check-read-only-at-point point n)
      (with-output-to-string (killring-stream)
        (declare (special killring-stream))
        (let ((charpos (point-charpos point))
              (buffer (point-buffer point))
              (line (point-line point))
              (offset-line 0))
          (declare (special buffer))
          (loop :while (plusp n)
                :for eolp := (> n (- (line-length line) charpos))
                :do (cond
                      ((not eolp)
                       (%delete-line-between/point point charpos (+ charpos n) line)
                       (shift-markers point offset-line (- n))
                       (return))
                      ((null (line-next line))
                       (%delete-line-eol/point point charpos line)
                       (shift-markers point offset-line (- charpos (line-length line)))
                       (return))
                      (t
                       (%delete-line/point point charpos line)))
                    (decf offset-line)
                :finally (shift-markers point offset-line 0)))))))


(declaim (inline call-before-change-functions
                 call-after-change-functions))

(defun call-before-change-functions (buffer start arg)
  (unless *inhibit-modification-hooks*
    (run-hooks (make-per-buffer-hook :var 'before-change-functions :buffer buffer)
               start arg)))

(defun call-after-change-functions (buffer start end old-len)
  (unless *inhibit-modification-hooks*
    (run-hooks (make-per-buffer-hook :var 'after-change-functions :buffer buffer)
               start end old-len)))

(defmacro insert/after-change-function (point arg)
  `(if (and (not *inhibit-modification-hooks*)
            (or (variable-value 'after-change-functions)
                (variable-value 'after-change-functions :global)))
       (with-point ((start ,point))
         (prog1 (call-next-method)
           (with-point ((end start))
             (character-offset end ,arg)
             (call-after-change-functions (point-buffer ,point) start end 0))))
       (call-next-method)))

(defmacro delete/after-change-function (point)
  `(if (and (not *inhibit-modification-hooks*)
            (or (variable-value 'after-change-functions)
                (variable-value 'after-change-functions :global)))
       (let ((string (call-next-method)))
         (with-point ((start ,point)
                      (end ,point))
           (call-after-change-functions (point-buffer ,point) start end (length string)))
         string)
       (call-next-method)))

(defmethod insert-char/point :around (point char)
  (call-before-change-functions (point-buffer point) point char)
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (insert/after-change-function point 1)
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point)))
        (prog1 (insert/after-change-function point 1)
          (without-interrupts
            (push-undo (point-buffer point)
                       (make-edit :insert-char linum charpos char)))))))

(defmethod insert-string/point :around (point string)
  (call-before-change-functions (point-buffer point) point string)
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (insert/after-change-function point (length string))
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point)))
        (prog1 (insert/after-change-function point (length string))
          (without-interrupts
            (push-undo (point-buffer point)
                       (make-edit :insert-string linum charpos string)))))))

(defmethod delete-char/point :around (point n)
  (call-before-change-functions (point-buffer point) point n)
  (if (not (buffer-enable-undo-p (point-buffer point)))
      (delete/after-change-function point)
      (let ((linum (line-number-at-point point))
            (charpos (point-charpos point))
            (string (delete/after-change-function point)))
        (without-interrupts
          (push-undo (point-buffer point)
                     (make-edit :delete-char linum charpos string)))
        string)))


;;; undo/redo
(defparameter *undo-modes* '(:edit :undo :redo))
(defvar *undo-mode* :edit)

(defun buffer-enable-undo-p (&optional (buffer (current-buffer)))
  "`buffer`でアンドゥが有効ならT、それ以外ならNILを返します。"
  (buffer-%enable-undo-p buffer))

(defun buffer-enable-undo (buffer)
  "`buffer`のアンドゥを有効にします。"
  (setf (buffer-%enable-undo-p buffer) t)
  nil)

(defun buffer-disable-undo (buffer)
  "`buffer`のアンドゥを無効にしてアンドゥ用の情報を空にします。"
  (setf (buffer-%enable-undo-p buffer) nil)
  (setf (buffer-edit-history buffer) (make-array 0 :adjustable t :fill-pointer 0))
  (setf (buffer-redo-stack buffer) nil)
  nil)

(defun buffer-enable-undo-boundary-p (&optional (buffer (current-buffer)))
  (buffer-%enable-undo-boundary-p buffer))

(defun buffer-enable-undo-boundary (buffer)
  (setf (buffer-%enable-undo-boundary-p buffer) t)
  nil)

(defun buffer-disable-undo-boundary (buffer)
  (setf (buffer-%enable-undo-boundary-p buffer) nil)
  nil)

(defun buffer-modify (buffer)
  (ecase *undo-mode*
    ((:edit :redo)
     (incf (buffer-%modified-p buffer)))
    ((:undo)
     (decf (buffer-%modified-p buffer))))
  (buffer-mark-cancel buffer))

(defun push-undo-stack (buffer elt)
  (vector-push-extend elt (buffer-edit-history buffer)))

(defun push-redo-stack (buffer elt)
  (push elt (buffer-redo-stack buffer)))

(defun push-undo (buffer edit)
  (when (buffer-enable-undo-p buffer)
    (ecase *undo-mode*
      (:edit
       (push-undo-stack buffer edit)
       (setf (buffer-redo-stack buffer) nil))
      (:redo
       (push-undo-stack buffer edit))
      (:undo
       (push-redo-stack buffer edit)))))

(defun buffer-undo-1 (point)
  (let* ((buffer (point-buffer point))
         (edit-history (buffer-edit-history buffer))
         (elt (and (< 0 (length edit-history)) (vector-pop edit-history))))
    (when elt
      (let ((*undo-mode* :undo))
        (unless (eq elt :separator)
          (apply-inverse-edit elt point))))))

(defun buffer-undo (point)
  (let ((buffer (point-buffer point)))
    (push :separator (buffer-redo-stack buffer))
    (when (eq :separator (last-edit-history buffer))
      (vector-pop (buffer-edit-history buffer)))
    (let ((result0 nil))
      (loop :for result := (buffer-undo-1 point)
            :while result
            :do (setf result0 result))
      (unless result0
        (assert (eq :separator (car (buffer-redo-stack buffer))))
        (pop (buffer-redo-stack buffer)))
      result0)))

(defun buffer-redo-1 (point)
  (let* ((buffer (point-buffer point))
         (elt (pop (buffer-redo-stack buffer))))
    (when elt
      (let ((*undo-mode* :redo))
        (unless (eq elt :separator)
          (apply-inverse-edit elt point))))))

(defun buffer-redo (point)
  (let ((buffer (point-buffer point)))
    (vector-push-extend :separator (buffer-edit-history buffer))
    (let ((result0 nil))
      (loop :for result := (buffer-redo-1 point)
            :while result
            :do (setf result0 result))
      (unless result0
        (assert (eq :separator
                    (last-edit-history buffer)))
        (vector-pop (buffer-edit-history buffer)))
      result0)))

(defun buffer-undo-boundary (&optional (buffer (current-buffer)))
  (when (buffer-enable-undo-boundary-p)
    (unless (eq :separator (last-edit-history buffer))
      (vector-push-extend :separator (buffer-edit-history buffer)))))

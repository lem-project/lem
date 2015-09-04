(in-package :lem)

(export '(*undo-limit*
          buffer
          buffer-name
          buffer-filename
          buffer-modified-p
          buffer-read-only-p
          buffer-major-mode
          buffer-minor-modes
          buffer-nlines
          buffer-put-property
          buffer-get-char
          buffer-line-length
          buffer-line-string
          map-buffer-lines
          buffer-take-lines
          buffer-insert-char
          buffer-insert-newline
          buffer-insert-line
          buffer-delete-char
          buffer-erase
          buffer-directory))

(defstruct (line (:constructor make-line-internal))
  (prev nil)
  (str "")
  (props nil)
  stat
  (next nil))

(defun make-line (prev next str)
  (let ((line (make-line-internal :next next :prev prev :str str)))
    (when next
      (setf (line-prev next) line))
    (when prev
      (setf (line-next prev) line))
    line))

(defun line-set-str (line str)
  (setf (line-str line) str))

(defun line-clear-stat (line)
  (setf (line-stat line) nil))

(macrolet ((def (name stat-name)
                `(progn
                   (defun ,name (line)
                     (eq ,stat-name (line-stat line)))
                   (defun (setf ,name) (stat line)
                     (setf (line-stat line)
                           (if stat
                               ,stat-name
                               nil))))))
  (def line-in-string-p     :in-string)
  (def line-start-string-p  :start-string)
  (def line-end-string-p    :end-string)
  (def line-in-comment-p    :in-comment)
  (def line-start-comment-p :start-comment)
  (def line-end-comment-p   :end-comment))

(defun line-put-property (line start end prop)
  (loop for pos from start below end
    do (setf (line-props line)
             (merge 'list
                    (list (cons pos prop))
                    (line-props line)
                    #'<
                    :key #'car)))
  (setf (line-props line)
        (delete-duplicates (line-props line)
                           :key #'car)))

(defun line-remove-property (line start end prop)
  (setf (line-props line)
        (delete-if #'(lambda (elt)
                       (and (<= start (car elt) (1- end))
                            (eql prop (cdr elt))))
                   (line-props line))))

(defun line-get-property (line pos)
  (cdr (find pos (line-props line) :key #'car)))

(defun line-free (line)
  (when (line-prev line)
    (setf (line-next (line-prev line))
          (line-next line)))
  (when (line-next line)
    (setf (line-prev (line-next line))
          (line-prev line)))
  (setf (line-prev line) nil
        (line-next line) nil
        (line-str line) nil))

(defun line-step-n (line n step-f)
  (do ((l line (funcall step-f l))
       (i 0 (1+ i)))
      ((= i n) l)))

(defun line-forward-n (line n)
  (line-step-n line n 'line-next))

(defun line-backward-n (line n)
  (line-step-n line n 'line-prev))

(define-class buffer () (window-buffer)
  name
  filename
  modified-p
  read-only-p
  enable-undo-p
  major-mode
  minor-modes
  head-line
  tail-line
  cache-line
  cache-linum
  mark-linum
  mark-col
  keep-binfo
  nlines
  undo-size
  undo-stack
  redo-stack
  undo-node
  saved-node
  overlays
  plist)

(defvar *undo-modes* '(:edit :undo :redo))
(defvar *undo-mode* :edit)
(defvar *undo-limit* 10000)

(defun make-buffer (name &key filename read-only-p (enable-undo-p t))
  (let ((buffer (make-instance 'buffer
                               :name name
                               :filename filename
                               :read-only-p read-only-p
                               :enable-undo-p enable-undo-p
                               :major-mode 'fundamental-mode))
        (line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)
    (setf (buffer-cache-linum buffer) 1)
    (setf (buffer-mark-linum buffer) 1)
    (setf (buffer-mark-col buffer) 0)
    (setf (buffer-nlines buffer) 1)
    (setf (buffer-undo-size buffer) 0)
    (setf (buffer-undo-stack buffer) nil)
    (setf (buffer-redo-stack buffer) nil)
    (setf (buffer-undo-node buffer) 0)
    (setf (buffer-saved-node buffer) 0)
    (unless (ghost-buffer-p buffer)
      (push buffer *buffer-list*))
    buffer))

(defmethod print-object ((buffer buffer) stream)
  (format stream "#<BUFFER ~a ~a>"
          (buffer-name buffer)
          (buffer-filename buffer)))

(defun buffer-enable-undo (buffer)
  (setf (buffer-enable-undo-p buffer) t)
  nil)

(defun buffer-disable-undo (buffer)
  (setf (buffer-enable-undo-p buffer) nil)
  (setf (buffer-undo-size buffer) 0)
  (setf (buffer-undo-stack buffer) nil)
  (setf (buffer-redo-stack buffer) nil)
  (setf (buffer-undo-node buffer) 0)
  (setf (buffer-saved-node buffer) 0)
  nil)

(defun buffer-modify (buffer)
  (setf (buffer-modified-p buffer)
        (if (buffer-modified-p buffer)
            (1+ (mod (buffer-modified-p buffer)
                     #.(floor most-positive-fixnum 2)))
            0)))

(defun buffer-save-node (buffer)
  (setf (buffer-saved-node buffer)
        (buffer-undo-node buffer)))

(defun push-undo-stack (buffer elt)
  (cond ((<= (+ *undo-limit* (floor (* *undo-limit* 0.3)))
             (buffer-undo-size buffer))
         (setf (buffer-undo-stack buffer)
               (subseq (buffer-undo-stack buffer)
                       0
                       *undo-limit*))
         (setf (buffer-undo-size buffer)
               (1+ (length (buffer-undo-stack buffer)))))
        (t
         (incf (buffer-undo-size buffer))))
  (let ((interrupt-p))
    (when-interrupted-flag :undo
                           (setq interrupt-p t)
                           (push :separator
                                 (buffer-undo-stack buffer)))
    (push elt (buffer-undo-stack buffer))
    interrupt-p))

(defun push-redo-stack (buffer elt)
  (push elt (buffer-redo-stack buffer)))

(defmacro with-push-undo ((buffer) &body body)
  (let ((gmark-linum (gensym "MARK-LINUM"))
        (gmark-col (gensym "MARK-COL")))
    `(when (and (buffer-enable-undo-p ,buffer)
                (not (ghost-buffer-p ,buffer)))
       (let ((,gmark-linum (buffer-mark-linum ,buffer))
             (,gmark-col (buffer-mark-col ,buffer)))
         (let ((elt #'(lambda ()
                        (setf (buffer-mark-col ,buffer) ,gmark-col)
                        (setf (buffer-mark-linum ,buffer) ,gmark-linum)
                        (progn ,@body))))
           (ecase *undo-mode*
             (:edit
              (when (push-undo-stack ,buffer elt)
                (incf (buffer-undo-node ,buffer)))
              (setf (buffer-redo-stack ,buffer) nil))
             (:redo
              (push-undo-stack ,buffer elt))
             (:undo
              (push-redo-stack ,buffer elt))))))))

(defmacro buffer-read-only-guard (buffer)
  `(when (buffer-read-only-p ,buffer)
     (throw 'abort 'readonly)))

(defun buffer-line-set-property (line-set-fn buffer prop linum
                                 &optional start-column end-column)
  (let ((line (buffer-get-line buffer linum)))
    (funcall line-set-fn
             line
             (or start-column 0)
             (or end-column (length (line-str line)))
             prop)))

(defun buffer-set-property (line-set-fn buffer start end prop)
  (with-points (((start-linum start-column) start)
                ((end-linum end-column) end))
    (cond ((= start-linum end-linum)
           (buffer-line-set-property line-set-fn
                                     buffer
                                     prop
                                     start-linum
                                     start-column
                                     end-column))
          (t
           (buffer-line-set-property line-set-fn
                                     buffer
                                     prop
                                     start-linum
                                     start-column)
           (buffer-line-set-property line-set-fn
                                     buffer
                                     prop
                                     end-linum
                                     0
                                     end-column)
           (loop
             for linum from (1+ start-linum) below end-linum
             do (buffer-line-set-property line-set-fn
                                          buffer
                                          prop
                                          linum))))))

(defun buffer-put-property (buffer start end prop)
  (buffer-set-property #'line-put-property buffer start end prop))

(defun buffer-remove-property (buffer start end prop)
  (buffer-set-property #'line-remove-property buffer start end prop))

(defun buffer-add-overlay (buffer overlay)
  (push overlay (buffer-overlays buffer)))

(defun buffer-delete-overlay (buffer overlay)
  (setf (buffer-overlays buffer)
        (delete overlay (buffer-overlays buffer))))

(defun %buffer-get-line (buffer linum)
  (cond
   ((= linum (buffer-cache-linum buffer))
    (buffer-cache-line buffer))
   ((> linum (buffer-cache-linum buffer))
    (if (< (- linum (buffer-cache-linum buffer))
           (- (buffer-nlines buffer) linum))
        (line-forward-n
         (buffer-cache-line buffer)
         (- linum (buffer-cache-linum buffer)))
        (line-backward-n
         (buffer-tail-line buffer)
         (- (buffer-nlines buffer) linum))))
   (t
    (if (< (1- linum)
           (- (buffer-cache-linum buffer) linum))
        (line-forward-n
         (buffer-head-line buffer)
         (1- linum))
        (line-backward-n
         (buffer-cache-line buffer)
         (- (buffer-cache-linum buffer) linum))))))

(defun buffer-get-line (buffer linum)
  (let ((line (%buffer-get-line buffer linum)))
    (setf (buffer-cache-linum buffer) linum)
    (setf (buffer-cache-line buffer) line)
    line))

(defun buffer-get-char (buffer linum column)
  (let ((line (buffer-get-line buffer linum)))
    (when (line-p line)
      (let ((str (line-str line)))
        (cond
         ((<= 0 column (1- (length str)))
          (aref str column))
         ((= column (length str))
          #\newline))))))

(defun buffer-line-length (buffer linum)
  (length (line-str (buffer-get-line buffer linum))))

(defun buffer-line-string (buffer linum)
  (let ((line (buffer-get-line buffer linum)))
    (when (line-p line)
      (values (line-str line)
              (line-props line)))))

(defun map-buffer (fn buffer &optional start-linum)
  (do ((line (if start-linum
                 (buffer-get-line buffer start-linum)
                 (buffer-head-line buffer))
             (line-next line))
       (linum (or start-linum 1) (1+ linum)))
      ((null line))
    (funcall fn line linum)))

(defun map-buffer-lines (fn buffer &optional start end)
  (let ((head-line
         (if start
             (buffer-get-line buffer start)
             (buffer-head-line buffer))))
    (unless end
      (setq end (buffer-nlines buffer)))
    (do ((line head-line (line-next line))
         (i (or start 1) (1+ i)))
        ((or (null line) (< end i)))
      (funcall fn
               (line-str line)
               (if (line-next line) nil t)
               i))))

(defun buffer-take-lines (buffer &optional linum len)
  (unless linum
    (setq linum 1))
  (unless len
    (setq len (buffer-nlines buffer)))
  (let ((strings))
    (map-buffer-lines
     #'(lambda (str eof-p linum)
         (declare (ignore eof-p linum))
         (push str strings))
     buffer
     linum
     (+ linum len -1))
    (nreverse strings)))

(defun set-prop-display-line (disp-lines
                              propval
                              start-linum
                              linum
                              start-column
                              end-column)
  (let ((i (- linum start-linum)))
    (unless end-column
      (setq end-column (length (car (aref disp-lines i)))))
    (loop
      for col from start-column below end-column
      for elt = (aref disp-lines i)
      do (setf (aref disp-lines i)
               (list* (car elt)
                      (cons col propval)
                      (remove col (cdr elt) :key #'car))))))

(defun display-lines-set-overlays (disp-lines overlays start-linum end-linum)
  (loop
    for overlay in overlays
    for start = (overlay-start overlay)
    for end = (overlay-end overlay)
    do (cond ((and (= (point-linum start) (point-linum end))
                   (<= start-linum (point-linum start) end-linum))
              (set-prop-display-line disp-lines
                                     (overlay-prop overlay)
                                     start-linum
                                     (point-linum start)
                                     (point-column start)
                                     (point-column end)))
             ((and (<= start-linum (point-linum start))
                   (<= (point-linum end) end-linum))
              (set-prop-display-line disp-lines
                                     (overlay-prop overlay)
                                     start-linum
                                     (point-linum start)
                                     (point-column start)
                                     nil)
              (loop
                for linum from (1+ (point-linum start))
                below (point-linum end) do
                (set-prop-display-line disp-lines
                                       (overlay-prop overlay)
                                       start-linum
                                       linum
                                       0
                                       nil))
              (set-prop-display-line disp-lines
                                     (overlay-prop overlay)
                                     start-linum
                                     (point-linum end)
                                     0
                                     (point-column end))))))

(defun buffer-display-lines (buffer disp-lines start-linum nlines)
  (let ((end-linum (+ start-linum nlines))
        (disp-nlines 0))
    (do ((line (buffer-get-line buffer start-linum)
               (line-next line))
         (i 0 (1+ i)))
        ((or (null line)
             (>= i nlines)))
      (incf disp-nlines)
      (setf (aref disp-lines i)
            (cons (line-str line)
                  (line-props line))))
    (loop
      for i from disp-nlines below nlines
      do (setf (aref disp-lines i) nil))
    (display-lines-set-overlays disp-lines
                                (buffer-overlays buffer)
                                start-linum
                                end-linum)
    disp-lines))

(defun buffer-append-line (buffer str)
  (buffer-read-only-guard buffer)
  (buffer-modify buffer)
  (let* ((line (buffer-tail-line buffer))
         (newline (make-line line (line-next line) str)))
    (cond ((and (= 1 (buffer-nlines buffer))
                (zerop (length (line-str (buffer-head-line buffer)))))
           (setf (buffer-head-line buffer) newline)
           (setf (buffer-cache-line buffer) newline))
          (t
           (incf (buffer-nlines buffer))))
    (setf (buffer-tail-line buffer) newline)
    t))

(defun buffer-insert-char (buffer linum col c)
  (buffer-read-only-guard buffer)
  (cond ((char= c #\newline)
         (buffer-insert-newline buffer linum col))
        (t
         (with-push-undo (buffer)
           (buffer-delete-char buffer linum col 1)
           (make-point linum col))
         (buffer-modify buffer)
         (let ((line (buffer-get-line buffer linum)))
           (when (and (= linum (buffer-mark-linum buffer))
                      (<= col (buffer-mark-col buffer)))
             (incf (buffer-mark-col buffer)))
           (line-set-str line
                         (concatenate 'string
                                      (subseq (line-str line) 0 col)
                                      (string c)
                                      (subseq (line-str line) col))))))
  t)

(defun buffer-insert-newline (buffer linum col)
  (buffer-read-only-guard buffer)
  (with-push-undo (buffer)
    (buffer-delete-char buffer linum col 1)
    (make-point linum col))
  (buffer-modify buffer)
  (cond
   ((= (buffer-mark-linum buffer) linum)
    (incf (buffer-mark-linum buffer))
    (decf (buffer-mark-col buffer) col))
   ((< linum (buffer-mark-linum buffer))
    (incf (buffer-mark-linum buffer))))
  (let ((line (buffer-get-line buffer linum)))
    (let ((newline
           (make-line line
                      (line-next line)
                      (subseq (line-str line) col))))
      (when (eq line (buffer-tail-line buffer))
        (setf (buffer-tail-line buffer) newline))
      (line-set-str line (subseq (line-str line) 0 col))))
  (incf (buffer-nlines buffer))
  t)

(defun buffer-insert-line (buffer linum col str)
  (buffer-read-only-guard buffer)
  (let ((line (buffer-get-line buffer linum)))
    (let ((oldstr (line-str line)))
      (with-push-undo (buffer)
        (buffer-delete-char buffer linum col (length str))
        (make-point linum col)))
    (buffer-modify buffer)
    (when (and (= linum (buffer-mark-linum buffer))
               (<= col (buffer-mark-col buffer)))
      (incf (buffer-mark-col buffer) (length str)))
    (line-set-str line
                  (concatenate 'string
                               (subseq (line-str line) 0 col)
                               str
                               (subseq (line-str line) col))))
  t)

(defun buffer-delete-char (buffer linum col n)
  (buffer-read-only-guard buffer)
  (let ((line (buffer-get-line buffer linum))
        (del-lines (list ""))
        (result t))
    (loop while (plusp n) do
      (cond
       ((<= n (- (length (line-str line)) col))
        (when (and (= linum (buffer-mark-linum buffer))
                   (< col (buffer-mark-col buffer)))
          (setf (buffer-mark-col buffer)
                (if (> col (- (buffer-mark-col buffer) n))
                    col
                    (- (buffer-mark-col buffer) n))))
        (buffer-modify buffer)
        (setf (car del-lines)
              (concatenate 'string
                           (car del-lines)
                           (subseq (line-str line) col (+ col n))))
        (line-set-str
         line
         (concatenate 'string
                      (subseq (line-str line) 0 col)
                      (subseq (line-str line) (+ col n))))
        (setq n 0))
       (t
        (cond
         ((and (= linum (buffer-mark-linum buffer))
               (< col (buffer-mark-col buffer)))
          (setf (buffer-mark-col buffer) col))
         ((< linum (buffer-mark-linum buffer))
          (decf (buffer-mark-linum buffer))))
        (setf (car del-lines)
              (concatenate 'string
                           (car del-lines)
                           (subseq (line-str line) col)))
        (push "" del-lines)
        (unless (line-next line)
          (setq result nil)
          (return nil))
        (decf n (1+ (- (length (line-str line)) col)))
        (decf (buffer-nlines buffer))
        (buffer-modify buffer)
        (line-set-str
         line
         (concatenate 'string
                      (subseq (line-str line) 0 col)
                      (line-str (line-next line))))
        (when (eq (line-next line)
                  (buffer-tail-line buffer))
          (setf (buffer-tail-line buffer) line))
        (line-free (line-next line)))))
    (setq del-lines (nreverse del-lines))
    (with-push-undo (buffer)
      (let ((linum linum)
            (col col))
        (do ((rest del-lines (cdr rest)))
            ((null rest))
          (buffer-insert-line buffer linum col (car rest))
          (when (cdr rest)
            (buffer-insert-newline buffer linum (+ col (length (car rest))))
            (incf linum)
            (setq col 0)))
        (make-point linum col)))
    (values result del-lines)))

(defun buffer-erase (buffer)
  (buffer-read-only-guard buffer)
  (buffer-modify buffer)
  (let ((line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)
    (setf (buffer-cache-linum buffer) 1)
    (setf (buffer-keep-binfo buffer) nil)
    (setf (buffer-nlines buffer) 1)
    (setf (buffer-undo-size buffer) 0)
    (setf (buffer-undo-stack buffer) nil)
    (setf (buffer-redo-stack buffer) nil)
    (setf (buffer-undo-node buffer) 0)
    (setf (buffer-saved-node buffer) 0)
    (setf (buffer-overlays buffer) nil)
    (setf (buffer-plist buffer) nil)))

(defun buffer-check-marked (buffer)
  (if (buffer-mark-linum buffer)
      t
      (progn
        (minibuf-print "Not mark in this buffer")
        nil)))

(defun buffer-directory ()
  (if (buffer-filename)
      (file-name-directory
       (buffer-filename))
      (file-name-as-directory (pwd))))

(defun buffer-undo-modified (buffer)
  (when (= (buffer-undo-node buffer)
           (buffer-saved-node buffer))
    (setf (buffer-modified-p buffer) nil)))

(defun buffer-undo-1 (buffer)
  (let ((elt (pop (buffer-undo-stack buffer))))
    (when elt
      (let ((*undo-mode* :undo))
        (unless (eq elt :separator)
          (decf (buffer-undo-size buffer))
          (funcall elt))))))

(defun buffer-undo (buffer)
  (loop while (eq :separator (car (buffer-undo-stack buffer)))
    do (pop (buffer-undo-stack buffer)))
  (prog1 (do ((res #1=(buffer-undo-1 buffer) #1#)
              (pres nil res))
             ((not res)
              (when pres
                (decf (buffer-undo-node buffer))
                (push :separator (buffer-redo-stack buffer))
                (buffer-undo-modified buffer))
              pres))))

(defun buffer-redo-1 (buffer)
  (let ((elt (pop (buffer-redo-stack buffer))))
    (when elt
      (let ((*undo-mode* :redo))
        (unless (eq elt :separator)
          (funcall elt))))))

(defun buffer-redo (buffer)
  (loop while (eq :separator (car (buffer-redo-stack buffer)))
    do (pop (buffer-redo-stack buffer)))
  (prog1 (do ((res #1=(buffer-redo-1 buffer) #1#)
              (pres nil res))
             ((not res)
              (when pres
                (incf (buffer-undo-node buffer))
                (push :separator (buffer-undo-stack buffer))
                (buffer-undo-modified buffer))
              pres))))

(defun buffer-get (buffer indicator &optional default)
  (getf (buffer-plist buffer) indicator default))

(defun buffer-put (buffer indicator value)
  (setf (getf (buffer-plist buffer) indicator) value))

(defun buffer-empty-plist (buffer)
  (setf (buffer-plist buffer) nil))

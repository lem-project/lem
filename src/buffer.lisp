(in-package :lem)

(export '(current-buffer
          bufferp
          buffer-name
          buffer-modified-p
          buffer-read-only-p
          buffer-major-mode
          buffer-minor-modes
          buffer-mark-p
          buffer-mark
          buffer-point
          buffer-nlines
          buffer-truncate-lines
          buffer-enable-undo-p
          buffer-enable-undo
          buffer-disable-undo
          buffer-filename
          buffer-directory
          buffer-unmark
          buffer-undo-boundary
          get-bvar
          clear-buffer-variables
          buffer-add-delete-hook))

(define-class buffer () (current-buffer)
  name
  %filename
  %directory
  %modified-p
  read-only-p
  %enable-undo-p
  major-mode
  minor-modes
  head-line
  tail-line
  cache-line
  cache-linum
  mark-p
  mark-overlay
  mark
  point
  keep-binfo
  nlines
  undo-size
  undo-stack
  redo-stack
  overlays
  points
  truncate-lines
  external-format
  last-write-date
  delete-hooks
  variables)

(defvar *current-buffer*)

(defun current-buffer () *current-buffer*)

(defun (setf current-buffer) (buffer)
  (check-type buffer buffer)
  (setf *current-buffer* buffer))

(defvar *undo-modes* '(:edit :undo :redo))
(defvar *undo-mode* :edit)
(defvar *undo-limit* 100000)

(defun make-buffer (name &key filename read-only-p (enable-undo-p t))
  (let ((buffer (make-instance 'buffer
                               :name name
                               :%filename filename
                               :%directory (when filename (directory-namestring filename))
                               :read-only-p read-only-p
                               :%enable-undo-p enable-undo-p
                               :major-mode 'fundamental-mode))
        (line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)

    (setf (buffer-cache-linum buffer) 1)
    (setf (buffer-mark-p buffer) nil)
    (setf (buffer-mark-overlay buffer) nil)
    (setf (buffer-mark buffer) nil)
    (setf (buffer-keep-binfo buffer) nil)
    (setf (buffer-nlines buffer) 1)

    (setf (buffer-%modified-p buffer) 0)
    (setf (buffer-undo-size buffer) 0)
    (setf (buffer-undo-stack buffer) nil)
    (setf (buffer-redo-stack buffer) nil)
    (setf (buffer-points buffer) nil)
    (setf (buffer-truncate-lines buffer) t)
    (setf (buffer-variables buffer) (make-hash-table :test 'equal))
    (setf (buffer-point buffer)
          (make-point buffer 1 0
		      :name "buffer-point"
		      :kind :left-inserting))
    (add-buffer buffer)
    buffer))

(defun buffer-enable-undo-p (&optional (buffer (current-buffer)))
  (buffer-%enable-undo-p buffer))

(defun buffer-modified-p (&optional (buffer (current-buffer)))
  (/= 0 (buffer-%modified-p buffer)))

(defun bufferp (x)
  (typep x 'buffer))

(defmethod print-object ((buffer buffer) stream)
  (format stream "#<BUFFER ~a ~a>"
          (buffer-name buffer)
          (buffer-filename buffer)))

(defun buffer-clear-keep-binfo (buffer)
  (when (buffer-keep-binfo buffer)
    (destructuring-bind (view-point point)
        (buffer-keep-binfo buffer)
      (delete-point view-point)
      (delete-point point))))

(defun call-buffer-delete-hooks (buffer)
  (mapc #'funcall (buffer-delete-hooks buffer))
  (buffer-clear-keep-binfo buffer)
  (delete-point (buffer-point buffer)))

(defun buffer-enable-undo (buffer)
  (setf (buffer-%enable-undo-p buffer) t)
  nil)

(defun buffer-disable-undo (buffer)
  (setf (buffer-%enable-undo-p buffer) nil)
  (setf (buffer-undo-size buffer) 0)
  (setf (buffer-undo-stack buffer) nil)
  (setf (buffer-redo-stack buffer) nil)
  nil)

(defun buffer-filename (&optional (buffer (current-buffer)))
  (buffer-%filename buffer))

(defun (setf buffer-filename) (filename &optional (buffer (current-buffer)))
  (let ((result (probe-file filename)))
    (unless result
      (error "file does not exist: ~A" filename))
    (setf (buffer-%filename buffer) (namestring result))))

(defun buffer-directory (&optional (buffer (current-buffer)))
  (or (buffer-%directory buffer)
      (namestring (uiop:getcwd))))

(defun (setf buffer-directory) (directory &optional (buffer (current-buffer)))
  (let ((result (uiop:directory-exists-p directory)))
    (unless result
      (error "directory does not exist: ~A" directory))
    (setf (buffer-%directory buffer)
          (namestring result))))

(defun buffer-unmark (buffer)
  (setf (buffer-%modified-p buffer) 0))

(defun buffer-add-overlay (buffer overlay)
  (push overlay (buffer-overlays buffer)))

(defun buffer-delete-overlay (buffer overlay)
  (setf (buffer-overlays buffer)
        (delete overlay (buffer-overlays buffer))))

(defun buffer-add-point (buffer point)
  (push point (buffer-points buffer)))

(defun buffer-delete-point (buffer point)
  (let ((length (length (buffer-points buffer))))
    (prog1 (setf (buffer-points buffer)
                 (delete point (buffer-points buffer)))
      (assert (/= length (length (buffer-points buffer)))))))

(defun buffer-mark-cancel (buffer)
  (when (buffer-mark-p buffer)
    (setf (buffer-mark-p buffer) nil)
    (delete-point (buffer-mark buffer))
    (delete-overlay (buffer-mark-overlay buffer))
    (setf (buffer-mark-overlay buffer) nil)))

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

(defun check-linum (buffer linum)
  (unless (<= 1 linum (buffer-nlines buffer))
    (error "invalid line number: ~A" linum)))

(defun check-point (buffer linum charpos)
  (check-linum buffer linum)
  (unless (<= 0 charpos (buffer-line-length buffer linum))
    (error "invalid character position: ~A" charpos)))

(defun buffer-get-line (buffer linum)
  (check-linum buffer linum)
  (let ((line (%buffer-get-line buffer linum)))
    (setf (buffer-cache-linum buffer) linum)
    (setf (buffer-cache-line buffer) line)
    line))

(defun buffer-get-char (buffer linum charpos)
  (let ((line (buffer-get-line buffer linum)))
    (when (line-p line)
      (let* ((str (line-str line))
             (len (length str)))
        (cond
          ((<= 0 charpos (1- len))
           (char str charpos))
          ((= charpos len)
           #\newline))))))

(defun buffer-line-length (buffer linum)
  (line-length (buffer-get-line buffer linum)))

(defun buffer-line-string (buffer linum)
  (let ((line (buffer-get-line buffer linum)))
    (when (line-p line)
      (line-str line))))

(defun buffer-update-mark-overlay (buffer)
  (when (buffer-mark-p buffer)
    (with-point ((start (buffer-point buffer))
		 (end (buffer-mark buffer)))
      (when (point< end start)
        (rotatef start end))
      (when (buffer-mark-overlay buffer)
        (delete-overlay (buffer-mark-overlay buffer)))
      (setf (buffer-mark-overlay buffer)
            (make-overlay start end *mark-overlay-attribute*)))))

(defun check-read-only-buffer (buffer)
  (when (buffer-read-only-p buffer)
    (error 'read-only-error)))

(defun buffer-modify (buffer)
  (ecase *undo-mode*
    ((:edit :redo)
     (incf (buffer-%modified-p buffer)))
    ((:undo)
     (decf (buffer-%modified-p buffer))))
  (buffer-mark-cancel buffer))

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
  (push elt (buffer-undo-stack buffer)))

(defun push-redo-stack (buffer elt)
  (push elt (buffer-redo-stack buffer)))

(defun push-undo (point fn)
  (let ((buffer (point-buffer point)))
    (when (and (buffer-enable-undo-p buffer)
               (not (ghost-buffer-p buffer)))
      (ecase *undo-mode*
        (:edit
         (push-undo-stack buffer fn)
         (setf (buffer-redo-stack buffer) nil))
        (:redo
         (push-undo-stack buffer fn))
        (:undo
         (push-redo-stack buffer fn))))))

(defun buffer-rename (buffer name)
  (check-type buffer buffer)
  (check-type name string)
  (when (get-buffer name)
    (editor-error "Buffer name `~A' is in use" name))
  (setf (buffer-name buffer) name))

(defun buffer-have-file-p (buffer)
  (and (buffer-filename buffer)
       (uiop:file-pathname-p (buffer-filename buffer))))

(defun buffer-undo-1 (buffer)
  (let ((elt (pop (buffer-undo-stack buffer))))
    (when elt
      (let ((*undo-mode* :undo))
        (unless (eq elt :separator)
          (decf (buffer-undo-size buffer))
          (funcall elt))))))

(defun buffer-undo (buffer)
  (push :separator (buffer-redo-stack buffer))
  (when (eq :separator (car (buffer-undo-stack buffer)))
    (pop (buffer-undo-stack buffer)))
  (let ((point nil))
    (loop :for result := (buffer-undo-1 buffer)
       :while result
       :do (setf point result))
    (unless point
      (assert (eq :separator (car (buffer-redo-stack buffer))))
      (pop (buffer-redo-stack buffer)))
    point))

(defun buffer-redo-1 (buffer)
  (let ((elt (pop (buffer-redo-stack buffer))))
    (when elt
      (let ((*undo-mode* :redo))
        (unless (eq elt :separator)
          (funcall elt))))))

(defun buffer-redo (buffer)
  (push :separator (buffer-undo-stack buffer))
  (let ((point nil))
    (loop :for result := (buffer-redo-1 buffer)
       :while result
       :do (setf point result))
    (unless point
      (assert (eq :separator (car (buffer-undo-stack buffer))))
      (pop (buffer-undo-stack buffer)))
    point))

(defun buffer-undo-boundary (&optional (buffer (current-buffer)))
  (unless (eq :separator (car (buffer-undo-stack buffer)))
    (push :separator (buffer-undo-stack buffer))))

(defun get-bvar (name &key (buffer (current-buffer)) default)
  (multiple-value-bind (value foundp)
      (gethash name (buffer-variables buffer))
    (if foundp value default)))

(defun (setf get-bvar) (value name &key (buffer (current-buffer)) default)
  (declare (ignore default))
  (setf (gethash name (buffer-variables buffer)) value))

(defun clear-buffer-variables (&key (buffer (current-buffer)))
  (clrhash (buffer-variables buffer)))

(defun buffer-add-delete-hook (buffer fn)
  (push fn (buffer-delete-hooks buffer))
  fn)

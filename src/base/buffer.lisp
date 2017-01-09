(in-package :lem-base)

(export '(fundamental-mode
          current-buffer
          make-buffer
          buffer
          bufferp
          buffer-name
          buffer-modified-p
          buffer-read-only-p
          buffer-syntax-table
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
          buffer-mark-cancel
          buffer-rename
          buffer-undo
          buffer-redo
          buffer-undo-boundary
          get-bvar
          clear-buffer-variables
          buffer-add-delete-hook))

(export '(%buffer-keep-binfo
          %buffer-clear-keep-binfo))

(defclass buffer ()
  ((name
    :initform nil
    :initarg :name
    :accessor buffer-name)
   (%filename
    :initform nil
    :initarg :%filename
    :accessor buffer-%filename)
   (%directory
    :initform nil
    :initarg :%directory
    :accessor buffer-%directory)
   (%modified-p
    :initform nil
    :accessor buffer-%modified-p)
   (%enable-undo-p
    :initform nil
    :initarg :%enable-undo-p
    :accessor buffer-%enable-undo-p)
   (read-only-p
    :initform nil
    :initarg :read-only-p
    :accessor buffer-read-only-p)
   (syntax-table
    :initform (fundamental-syntax-table)
    :initarg :syntax-table
    :accessor buffer-syntax-table)
   (major-mode
    :initform nil
    :initarg :major-mode
    :accessor buffer-major-mode)
   (minor-modes
    :initform nil
    :initarg :minor-modes
    :accessor buffer-minor-modes)
   (start-point
    :initform nil
    :initarg :start-point
    :accessor buffer-start-point)
   (end-point
    :initform nil
    :initarg :end-point
    :accessor buffer-end-point)
   (mark-p
    :initform nil
    :initarg :mark-p
    :accessor buffer-mark-p)
   (mark
    :initform nil
    :initarg :mark
    :accessor buffer-mark)
   (point
    :initform nil
    :initarg :point
    :accessor buffer-point)
   (keep-binfo
    :initform nil
    :initarg :keep-binfo
    :accessor %buffer-keep-binfo)
   (nlines
    :initform nil
    :initarg :nlines
    :accessor buffer-nlines)
   (undo-size
    :initform nil
    :initarg :undo-size
    :accessor buffer-undo-size)
   (undo-stack
    :initform nil
    :initarg :undo-stack
    :accessor buffer-undo-stack)
   (redo-stack
    :initform nil
    :initarg :redo-stack
    :accessor buffer-redo-stack)
   (overlays
    :initform nil
    :initarg :overlays
    :accessor buffer-overlays)
   (points
    :initform nil
    :initarg :points
    :accessor buffer-points)
   (truncate-lines
    :initform nil
    :initarg :truncate-lines
    :accessor buffer-truncate-lines)
   (external-format
    :initform nil
    :initarg :external-format
    :accessor buffer-external-format)
   (last-write-date
    :initform nil
    :initarg :last-write-date
    :accessor buffer-last-write-date)
   (delete-hooks
    :initform nil
    :initarg :delete-hooks
    :accessor buffer-delete-hooks)
   (variables
    :initform nil
    :initarg :variables
    :accessor buffer-variables)))

(defvar *current-buffer*)

(defun current-buffer () *current-buffer*)

(defun (setf current-buffer) (buffer)
  (check-type buffer buffer)
  (setf *current-buffer* buffer))

(defvar *undo-modes* '(:edit :undo :redo))
(defvar *undo-mode* :edit)
(defvar *undo-limit* 100000)

(defun make-buffer (name &key filename read-only-p (enable-undo-p t))
  (when (get-buffer name)
    (error "buffer already exists: ~A" name))
  (let ((buffer (make-instance 'buffer
                               :name name
                               :%filename filename
                               :%directory (when filename (directory-namestring filename))
                               :read-only-p read-only-p
                               :%enable-undo-p enable-undo-p
                               :major-mode 'fundamental-mode)))
    (setf (buffer-mark-p buffer) nil)
    (setf (buffer-mark buffer) nil)
    (setf (%buffer-keep-binfo buffer) nil)
    (setf (buffer-nlines buffer) 1)
    (setf (buffer-%modified-p buffer) 0)
    (setf (buffer-undo-size buffer) 0)
    (setf (buffer-undo-stack buffer) nil)
    (setf (buffer-redo-stack buffer) nil)
    (setf (buffer-truncate-lines buffer) t)
    (setf (buffer-variables buffer) (make-hash-table :test 'equal))
    (setf (buffer-points buffer) nil)
    (let ((line (make-line buffer nil nil "")))
      (setf (buffer-start-point buffer)
            (make-point buffer line 0
                        :kind :right-inserting
                        :name "start-point"))
      (setf (buffer-end-point buffer)
            (make-point buffer line 0
                        :kind :left-inserting
                        :name "end-point"))
      (setf (buffer-point buffer)
            (make-point buffer line 0
                        :name "buffer-point"
                        :kind :left-inserting)))
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

(defun %buffer-clear-keep-binfo (buffer)
  (when (%buffer-keep-binfo buffer)
    (destructuring-bind (view-point point)
        (%buffer-keep-binfo buffer)
      (delete-point view-point)
      (delete-point point))))

(defun call-buffer-delete-hooks (buffer)
  (mapc #'funcall (buffer-delete-hooks buffer))
  (%buffer-clear-keep-binfo buffer)
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
  (setf (buffer-%filename buffer) filename))

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
    t))

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

(defun push-undo (buffer fn)
  (when (and (buffer-enable-undo-p buffer)
             (not (ghost-buffer-p buffer)))
    (ecase *undo-mode*
      (:edit
       (push-undo-stack buffer fn)
       (setf (buffer-redo-stack buffer) nil))
      (:redo
       (push-undo-stack buffer fn))
      (:undo
       (push-redo-stack buffer fn)))))

(defun buffer-rename (buffer name)
  (check-type buffer buffer)
  (check-type name string)
  (when (get-buffer name)
    (editor-error "Buffer name `~A' is in use" name))
  (setf (buffer-name buffer) name))

(defun buffer-undo-1 (point)
  (let* ((buffer (point-buffer point))
         (elt (pop (buffer-undo-stack buffer))))
    (when elt
      (let ((*undo-mode* :undo))
        (unless (eq elt :separator)
          (decf (buffer-undo-size buffer))
          (funcall elt point))))))

(defun buffer-undo (point)
  (let ((buffer (point-buffer point)))
    (push :separator (buffer-redo-stack buffer))
    (when (eq :separator (car (buffer-undo-stack buffer)))
      (pop (buffer-undo-stack buffer)))
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
          (funcall elt point))))))

(defun buffer-redo (point)
  (let ((buffer (point-buffer point)))
    (push :separator (buffer-undo-stack buffer))
    (let ((result0 nil))
      (loop :for result := (buffer-redo-1 point)
            :while result
            :do (setf result0 result))
      (unless result0
        (assert (eq :separator (car (buffer-undo-stack buffer))))
        (pop (buffer-undo-stack buffer)))
      result0)))

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

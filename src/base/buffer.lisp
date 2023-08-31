(in-package :lem-base)

(defparameter +primordial-buffer-name+ "*tmp*")

(defclass buffer ()
  ((name
    :initarg :name
    :accessor buffer-%name)
   (temporary
    :initarg :temporary
    :reader buffer-temporary-p
    :type boolean)
   (variables
    :initform (make-hash-table :test 'equal)
    :accessor buffer-variables)
   ;; only used in text buffer
   (%filename
    :initform nil
    :accessor buffer-%filename)
   (%directory
    :initarg :%directory
    :initform nil
    :accessor buffer-%directory
    :documentation "The buffer's directory. See: `buffer-directory'.")
   (%modified-p
    :initform 0
    :reader buffer-modified-tick
    :accessor buffer-%modified-p
    :type integer)
   (%enable-undo-p
    :initarg :%enable-undo-p
    :accessor buffer-%enable-undo-p
    :type boolean)
   (%enable-undo-boundary-p
    :initarg :%enable-undo-boundary-p
    :initform t
    :accessor buffer-%enable-undo-boundary-p)
   (read-only-p
    :initarg :read-only-p
    :accessor buffer-read-only-p
    :type boolean)
   (syntax-table
    :initform (fundamental-syntax-table)
    :initarg :syntax-table
    :accessor buffer-syntax-table
    :type syntax-table)
   (major-mode
    :initform 'fundamental-mode
    :initarg :major-mode
    :accessor buffer-major-mode)
   (minor-modes
    :initform nil
    :accessor buffer-minor-modes
    :type list)
   (start-point
    :initform nil
    :writer set-buffer-start-point
    :reader buffer-start-point)
   (end-point
    :writer set-buffer-end-point
    :reader buffer-end-point)
   (%mark
    :type mark
    :initform (make-instance 'mark)
    :reader buffer-mark-object)
   (point
    :initform nil
    :reader buffer-point
    :writer set-buffer-point)
   (keep-binfo
    :initform nil
    :accessor %buffer-keep-binfo)
   (points
    :initform nil
    :accessor buffer-points)
   (nlines
    :initform 1
    :accessor buffer-nlines
    :type (integer 1 *))
   (edit-history
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor buffer-edit-history)
   (redo-stack
    :initform '()
    :accessor buffer-redo-stack)
   (encoding
    :initform nil
    :accessor buffer-encoding)
   (last-write-date
    :initform nil
    :accessor buffer-last-write-date)))

(defclass text-buffer (buffer)
  ())

(defmethod buffer-mark ((buffer buffer))
  (mark-point (buffer-mark-object buffer)))

(defmethod buffer-mark-p ((buffer buffer))
  (mark-active-p (buffer-mark-object buffer)))

;; workaround for windows
#+win32
(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
  "set default buffer encoding to utf-8"
  (setf (buffer-encoding buffer) (encoding :utf-8 :lf)))

(setf (documentation 'buffer-point 'function) "`buffer`の現在の`point`を返します。")
(setf (documentation 'buffer-mark 'function) "`buffer`の現在のマークの`point`を返します。")
(setf (documentation 'buffer-start-point 'function) "`buffer`の最初の位置の`point`を返します。")
(setf (documentation 'buffer-end-point 'function) "`buffer`の最後の位置の`point`を返します。")

(defvar *current-buffer*)

(defun primordial-buffer ()
  (make-buffer +primordial-buffer-name+))

(defun current-buffer ()
  "現在の`buffer`を返します。"
  (unless (boundp '*current-buffer*)
    (setf *current-buffer*
          (primordial-buffer)))
  *current-buffer*)

(defun (setf current-buffer) (buffer)
  "現在の`buffer`を変更します。"
  (check-type buffer buffer)
  (setf *current-buffer* buffer))

(defparameter *undo-modes* '(:edit :undo :redo))
(defvar *undo-mode* :edit)

(defun last-edit-history (buffer)
  (when (< 0 (fill-pointer (buffer-edit-history buffer)))
    (aref (buffer-edit-history buffer)
          (1- (fill-pointer (buffer-edit-history buffer))))))

(defun make-buffer-start-point (point)
  (copy-point point :right-inserting))

(defun make-buffer-end-point (point)
  (copy-point point :left-inserting))

(defgeneric make-buffer-point (point)
  (:method (point)
    (copy-point point :left-inserting)))

(defun make-buffer (name &key temporary read-only-p (enable-undo-p t) directory
                              (syntax-table (fundamental-syntax-table)))
  "If the buffer of name `name` exists in Lem's buffer list, return it, otherwise create it.
`read-only-p` sets this buffer read-only.
`enable-undo-p` enables undo.
`syntax-table` specifies the syntax table for the buffer.
If `temporary` is non-NIL, it creates a buffer and doesn't add it in the buffer list.
Options that can be specified by arguments are ignored if `temporary` is NIL and the buffer already exists.
"
  (unless temporary
    (uiop:if-let ((buffer (get-buffer name)))
      (return-from make-buffer buffer)))
  (let ((buffer (make-instance 'text-buffer
                               :name name
                               :read-only-p read-only-p
                               :%directory directory
                               :%enable-undo-p enable-undo-p
                               :temporary temporary
                               :syntax-table syntax-table)))
    (let* ((temp-point (make-point buffer 1 (make-empty-line) 0 :kind :temporary))
           (start-point (make-buffer-start-point temp-point))
           (end-point (make-buffer-end-point temp-point))
           (point (make-buffer-point temp-point)))
      (set-buffer-start-point start-point buffer)
      (set-buffer-end-point end-point buffer)
      (set-buffer-point point buffer))
    (unless temporary (add-buffer buffer))
    buffer))

(defun bufferp (x)
  "`x`が`buffer`ならT、それ以外ならNILを返します。"
  (typep x 'buffer))

(defun buffer-modified-p (&optional (buffer (current-buffer)))
  "`buffer`が変更されていたらT、それ以外ならNILを返します。"
  (/= 0 (buffer-%modified-p buffer)))

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

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :identity t :type t)
    (format stream "~A ~A"
            (buffer-name buffer)
            (buffer-filename buffer))))

(defun %buffer-clear-keep-binfo (buffer)
  (when (%buffer-keep-binfo buffer)
    (destructuring-bind (view-point point)
        (%buffer-keep-binfo buffer)
      (delete-point view-point)
      (delete-point point))))

(defun buffer-free (buffer)
  (%buffer-clear-keep-binfo buffer)
  (delete-point (buffer-point buffer))
  (set-buffer-point nil buffer))

(defun deleted-buffer-p (buffer)
  (null (buffer-point buffer)))

(defun buffer-name (&optional (buffer (current-buffer)))
  "`buffer`の名前を返します。"
  (buffer-%name buffer))

(defun buffer-filename (&optional (buffer (current-buffer)))
  "`buffer`のファイル名を返します。"
  (alexandria:when-let (filename (buffer-%filename buffer))
    (namestring filename)))

(defun (setf buffer-filename) (filename &optional (buffer (current-buffer)))
  (setf (buffer-directory buffer) (directory-namestring filename))
  (setf (buffer-%filename buffer) filename))

(defun buffer-directory (&optional (buffer (current-buffer)))
  "`buffer`のディレクトリを返します。"
  (or (buffer-%directory buffer)
      (namestring (uiop:getcwd))))

(defun (setf buffer-directory) (directory &optional (buffer (current-buffer)))
  (let ((result (uiop:directory-exists-p directory)))
    (unless result
      (error 'directory-does-not-exist :directory directory))
    (setf (buffer-%directory buffer)
          (namestring result))))

(defun buffer-unmark (buffer)
  "`buffer`の変更フラグを下ろします。"
  (setf (buffer-%modified-p buffer) 0))

(defun buffer-mark-cancel (buffer)
  (mark-cancel (buffer-mark-object buffer)))

(defun buffer-attributes (buffer)
  (concatenate 'string
               " "
               (cond ((buffer-read-only-p buffer)
                      "% ")
                     ((buffer-modified-p buffer)
                      "* ")
                     (t
                      "  "))))

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

(defun buffer-rename (buffer name)
  "`buffer`の名前を`name`に変更します。"
  (check-type buffer buffer)
  (check-type name string)
  (when (get-buffer name)
    (editor-error "Buffer name `~A' is in use" name))
  (setf (buffer-%name buffer) name))

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

(defun buffer-value (buffer name &optional default)
  "`buffer`のバッファ変数`name`に束縛されている値を返します。
`buffer`の型は`buffer`または`point`です。
変数が設定されていない場合は`default`を返します。"
  (setf buffer (ensure-buffer buffer))
  (multiple-value-bind (value foundp)
      (gethash name (buffer-variables buffer))
    (if foundp value default)))

(defun (setf buffer-value) (value buffer name &optional default)
  "`buffer`のバッファ変数`name`に`value`を束縛します。
`buffer`の型は`buffer`または`point`です。"
  (declare (ignore default))
  (setf buffer (ensure-buffer buffer))
  (setf (gethash name (buffer-variables buffer)) value))

(defun buffer-unbound (buffer name)
  "`buffer`のバッファ変数`name`の束縛を消します。"
  (remhash name (buffer-variables buffer)))

(defun clear-buffer-variables (&key (buffer (current-buffer)))
  "`buffer`に束縛されているすべてのバッファ変数を消します。"
  (clrhash (buffer-variables buffer)))

(defun call-with-buffer-point (buffer point function)
  (let ((original-point (buffer-point buffer)))
    (set-buffer-point point buffer)
    (unwind-protect (funcall function)
      (set-buffer-point original-point buffer))))

(defmacro with-buffer-point ((buffer point) &body body)
  `(call-with-buffer-point ,buffer ,point (lambda () ,@body)))

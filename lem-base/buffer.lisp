(in-package :lem-base)

(annot:enable-annot-syntax)

(export '(fundamental-mode
          current-buffer
          make-buffer
          buffer
          bufferp
          buffer-start-point
          buffer-end-point
          buffer-name
          buffer-version
          buffer-modified-p
          buffer-read-only-p
          buffer-syntax-table
          buffer-major-mode
          buffer-minor-modes
          buffer-mark-p
          buffer-mark
          buffer-point
          buffer-nlines
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
          buffer-value
          buffer-unbound
          clear-buffer-variables))

(export '(%buffer-keep-binfo
          %buffer-clear-keep-binfo))

(defparameter +original-buffer-name+ "*tmp*")

(defclass buffer ()
  ((name
    :initform nil
    :initarg :name
    :accessor buffer-%name)
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
    :reader buffer-version
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
    :writer set-buffer-start-point
    :reader buffer-start-point)
   (end-point
    :initform nil
    :initarg :end-point
    :writer set-buffer-end-point
    :reader buffer-end-point)
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
   (points
    :initform nil
    :accessor buffer-points)
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
   (external-format
    :initform nil
    :initarg :external-format
    :accessor buffer-external-format)
   (last-write-date
    :initform nil
    :initarg :last-write-date
    :accessor buffer-last-write-date)
   (variables
    :initform nil
    :initarg :variables
    :accessor buffer-variables))
  (:documentation
   @lang(:jp "`buffer`はバッファ名、ファイル名、テキスト、テキストを指す位置等が入った、
文書を管理するオブジェクトです。  
複数の`buffer`はリストで管理されています。")))

(setf (documentation 'buffer-point 'function) @lang(:jp "`buffer`の現在の`point`を返します。"))
(setf (documentation 'buffer-mark 'function) @lang(:jp "`buffer`の現在のマークの`point`を返します。"))
(setf (documentation 'buffer-start-point 'function) @lang(:jp "`buffer`の最初の位置の`point`を返します。"))
(setf (documentation 'buffer-end-point 'function) @lang(:jp "`buffer`の最後の位置の`point`を返します。"))

(defvar *current-buffer*)

(defun current-buffer ()
  @lang(:jp "現在の`buffer`を返します。")
  (unless (boundp '*current-buffer*)
    (setf *current-buffer*
          (get-buffer-create +original-buffer-name+)))
  *current-buffer*)

(defun (setf current-buffer) (buffer)
  @lang(:jp "現在の`buffer`を変更します。")
  (check-type buffer buffer)
  (setf *current-buffer* buffer))

(defvar *undo-modes* '(:edit :undo :redo))
(defvar *undo-mode* :edit)
(defvar *undo-limit* 100000)

(defun make-buffer (name &key filename read-only-p (enable-undo-p t)
                         (syntax-table (fundamental-syntax-table)))
  "新しい`buffer`を作って返します。  
既に`name`と同じ名前のバッファがある場合はエラーになります。"
  (when (get-buffer name)
    (error "buffer already exists: ~A" name))
  (let ((buffer (make-instance 'buffer
                               :name name
                               :%filename filename
                               :%directory (when filename (directory-namestring filename))
                               :read-only-p read-only-p
                               :%enable-undo-p enable-undo-p
                               :major-mode 'fundamental-mode
                               :syntax-table syntax-table)))
    (setf (buffer-mark-p buffer) nil)
    (setf (buffer-mark buffer) nil)
    (setf (%buffer-keep-binfo buffer) nil)
    (setf (buffer-nlines buffer) 1)
    (setf (buffer-%modified-p buffer) 0)
    (setf (buffer-undo-size buffer) 0)
    (setf (buffer-undo-stack buffer) nil)
    (setf (buffer-redo-stack buffer) nil)
    (setf (buffer-variables buffer) (make-hash-table :test 'equal))
    (let ((line (make-line nil nil "")))
      (set-buffer-start-point (make-point buffer 1 line 0 :kind :right-inserting)
                              buffer)
      (set-buffer-end-point (make-point buffer 1 line 0
                                        :kind :left-inserting)
                            buffer)
      (setf (buffer-point buffer)
            (make-point buffer 1 line 0
                        :kind :left-inserting)))
    (add-buffer buffer)
    buffer))

(defun bufferp (x)
  @lang(:jp "`x`が`buffer`ならT、それ以外ならNILを返します。")
  (typep x 'buffer))

(defun buffer-modified-p (&optional (buffer (current-buffer)))
  @lang(:jp "`buffer`が変更されていたらT、それ以外ならNILを返します。")
  (/= 0 (buffer-%modified-p buffer)))

(defun buffer-enable-undo-p (&optional (buffer (current-buffer)))
  @lang(:jp "`buffer`でアンドゥが有効ならT、それ以外ならNILを返します。")
  (buffer-%enable-undo-p buffer))

(defun buffer-enable-undo (buffer)
  @lang(:jp "`buffer`のアンドゥを有効にします。")
  (setf (buffer-%enable-undo-p buffer) t)
  nil)

(defun buffer-disable-undo (buffer)
  @lang(:jp "`buffer`のアンドゥを無効にしてアンドゥ用の情報を空にします。")
  (setf (buffer-%enable-undo-p buffer) nil)
  (setf (buffer-undo-size buffer) 0)
  (setf (buffer-undo-stack buffer) nil)
  (setf (buffer-redo-stack buffer) nil)
  nil)

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

(defun buffer-free (buffer)
  (%buffer-clear-keep-binfo buffer)
  (delete-point (buffer-point buffer)))

(defun buffer-name (&optional (buffer (current-buffer)))
  @lang(:jp "`buffer`の名前を返します。")
  (buffer-%name buffer))

(defun buffer-filename (&optional (buffer (current-buffer)))
  @lang(:jp "`buffer`のファイル名を返します。")
  (buffer-%filename buffer))

(defun (setf buffer-filename) (filename &optional (buffer (current-buffer)))
  (setf (buffer-%filename buffer) filename))

(defun buffer-directory (&optional (buffer (current-buffer)))
  @lang(:jp "`buffer`のディレクトリを返します。")
  (or (buffer-%directory buffer)
      (namestring (uiop:getcwd))))

(defun (setf buffer-directory) (directory &optional (buffer (current-buffer)))
  (let ((result (uiop:directory-exists-p directory)))
    (unless result
      (error "directory does not exist: ~A" directory))
    (setf (buffer-%directory buffer)
          (namestring result))))

(defun buffer-unmark (buffer)
  @lang(:jp "`buffer`の変更フラグを下ろします。")
  (setf (buffer-%modified-p buffer) 0))

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
  @lang(:jp "`buffer`の名前を`name`に変更します。")
  (check-type buffer buffer)
  (check-type name string)
  (when (get-buffer name)
    (editor-error "Buffer name `~A' is in use" name))
  (setf (buffer-%name buffer) name))

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

(defun buffer-value (buffer name &optional default)
  @lang(:jp "`buffer`のバッファ変数`name`に束縛されている値を返します。  
`buffer`の型は`buffer`または`point`です。  
変数が設定されていない場合は`default`を返します。")
  (setf buffer (ensure-buffer buffer))
  (multiple-value-bind (value foundp)
      (gethash name (buffer-variables buffer))
    (if foundp value default)))

(defun (setf buffer-value) (value buffer name &optional default)
  @lang(:jp "`buffer`のバッファ変数`name`に`value`を束縛します。  
`buffer`の型は`buffer`または`point`です。")
  (declare (ignore default))
  (setf buffer (ensure-buffer buffer))
  (setf (gethash name (buffer-variables buffer)) value))

(defun buffer-unbound (buffer name)
  @lang(:jp "`buffer`のバッファ変数`name`の束縛を消します。")
  (remhash name (buffer-variables buffer)))

(defun clear-buffer-variables (&key (buffer (current-buffer)))
  @lang(:jp "`buffer`に束縛されているすべてのバッファ変数を消します。")
  (clrhash (buffer-variables buffer)))

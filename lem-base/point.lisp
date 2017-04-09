(in-package :lem-base)

(annot:enable-annot-syntax)

(export '(current-point
          point
          pointp
          copy-point
          delete-point
          point-buffer
          point-charpos
          point-kind
          point=
          point/=
          point<
          point<=
          point>
          point>=))

(defclass point ()
  ((buffer
    :initarg :buffer
    :reader point-buffer
    :type buffer)
   (linum
    :initarg :linum
    :accessor point-linum
    :type fixnum)
   (line
    :initarg :line
    :accessor point-line
    :type line)
   (charpos
    :initarg :charpos
    :accessor point-charpos
    :type fixnum)
   (kind
    :initarg :kind
    :reader point-kind
    :type (member :temporary :left-inserting :right-inserting)))
  (:documentation
   @lang(:jp "`point`はバッファ内のテキストの位置を指すオブジェクトです。  
`buffer`とその位置の行、行頭からの0始まりのオフセット`charpos`をもっています。  
`point`には`kind`があり、バッファ内に挿入、削除した後の位置が`kind`の値によって変わります。  
`kind`が`:temporary`の時は`point`を一時的な読み取りに使います。  
作成、削除時のオーバーヘッドが低く、明示的に削除する必要もありませんが、
その位置より前を編集した後はその`point`は正しく使用できません。  
`kind`が`:left-inserting`または`:right-inserting`の時はそれより前の位置を編集したときに、
編集した長さだけ位置を調整します。  
`point`と同じ位置に挿入すると
`:right-inserting`では元の位置のままで、`:left-inserting`では移動します。  
`:left-inserting`または`:right-inserting`の場合は、使用後に`delete-point`で明示的に削除するか、
`with-point`を使う必要があります。
")))

(setf (documentation 'point-buffer 'function)
      @lang(:jp "`point`が指す`buffer`を返します。"))

(setf (documentation 'point-kind 'function)
      @lang(:jp "`point`の種類(`:temporary`、`:left-inserting`または`:right-inserting`)を返します。"))

(defun current-point ()
  @lang(:jp "現在の`point`を返します。")
  (buffer-point (current-buffer)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "POINT ~A ~S"
            (point-charpos object)
            (line-str (point-line object)))))

(defun pointp (x)
  @lang(:jp "`x`が`point`ならT、それ以外ならNILを返します。")
  (typep x 'point))

(defun make-point (buffer linum line charpos &key (kind :right-inserting))
  (check-type kind (member :temporary :left-inserting :right-inserting))
  (let ((point (make-instance 'point
                              :buffer buffer
                              :linum linum
                              :line line
                              :charpos charpos
                              :kind kind)))
    (unless (eq :temporary kind)
      (push point (line-points line))
      (push point (buffer-points buffer)))
    point))

(defun copy-point (point &optional kind)
  @lang(:jp "`point`のコピーを作って返します。
`kind`は`:temporary`、`:left-inserting`または `right-inserting`です。
省略された場合は`point`と同じ値です。")
  (make-point (point-buffer point)
              (point-linum point)
              (point-line point)
	      (point-charpos point)
	      :kind (or kind (point-kind point))))

(defun delete-point (point)
  @lang(:jp "`point`を削除します。
`point-kind`が:temporaryの場合はこの関数を使う必要はありません。")
  (unless (point-temporary-p point)
    (setf (line-points (point-line point))
          (delete point (line-points (point-line point))))
    (let ((buffer (point-buffer point)))
      (setf (buffer-points buffer)
            (delete point (buffer-points buffer))))
    (values)))

(defun point-change-line (point new-linum new-line)
  (unless (point-temporary-p point)
    (let ((old-line (point-line point)))
      (if (line-alive-p old-line)
          (do ((scan (line-points old-line) (cdr scan))
               (prev nil scan))
              ((eq (car scan) point)
               (if prev
                   (setf (cdr prev) (cdr scan))
                   (setf (line-points old-line) (cdr scan)))
               (setf (cdr scan) (line-points new-line)
                     (line-points new-line) scan)))
          (push point (line-points new-line)))))
  (setf (point-linum point) new-linum)
  (setf (point-line point) new-line))

(defun point-temporary-p (point)
  (eq (point-kind point) :temporary))

(defun point= (point1 point2)
  @lang(:jp "`point1`と`point2`が同じ位置にあるならT、それ以外はNILを返します。")
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (and (= (point-linum point1)
          (point-linum point2))
       (= (point-charpos point1)
          (point-charpos point2))))

(defun point/= (point1 point2)
  @lang(:jp "`point1`と`point2`が同じ位置ではないならT、それ以外はNILを返します。")
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (not (point= point1 point2)))

(defun point< (point1 point2)
  @lang(:jp "`point1`が`point2`よりも前にあるならT、それ以外はNILを返します。")
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (or (< (point-linum point1) (point-linum point2))
      (and (= (point-linum point1) (point-linum point2))
           (< (point-charpos point1) (point-charpos point2)))))

(defun point<= (point1 point2)
  @lang(:jp "`point1`が`point2`と同じ位置、または前にあるならT、それ以外はNILを返します。")
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (or (point< point1 point2)
      (point= point1 point2)))

(defun point> (point1 point2)
  @lang(:jp "`point1`が`point2`よりも後にあるならT、それ以外はNILを返します。")
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (point< point2 point1))

(defun point>= (point1 point2)
  @lang(:jp "`point1`が`point2`と同じ位置、または後にあるならT、それ以外はNILを返します。")
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (point<= point2 point1))

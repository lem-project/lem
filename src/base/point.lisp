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
    :type buffer
    :documentation @lang(:jp "`point`が指す`buffer`を返します。"))
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
    :type (member :temporary :left-inserting :right-inserting)
    :documentation
    @lang(:jp "`point`の種類(`:temporary`, `:left-inserting`, `:right-inserting`)を返します。")))
  (:documentation
   @lang(:jp "`point`はバッファ内のテキストの位置を指すオブジェクトです。  
`buffer`とその位置の行、行頭からの0始まりのオフセット`charpos`をもっています。  
`point`には`kind`があり、バッファ内に挿入、削除したときに位置を調整する動作を制御します。  
`kind`が`:temporary`の時は`point`を一時的な読み取りに使います。  
作成、削除時のオーバーヘッドが低く、明示的に削除する必要もありませんが、
その位置より前を編集した後は正しく使用できません。  
`kind`が`:left-inserting`または`:right-inserting`の時はそれより前の位置を編集したときに、
編集した長さだけ位置を移動します。  
`point`と同じ位置に挿入したときは`:right-inserting`は元の位置のままで、`:left-inserting`の時は移動します。  
`:left-inserting`または`:right-inserting`の時はバッファがその`point`を管理しているので、
使用後は`delete-point`で明示的に削除するか`with-point`を使ってください。
")))

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

(defun make-point (buffer line charpos &key (kind :right-inserting))
  (check-type kind (member :temporary :left-inserting :right-inserting))
  (let ((point (make-instance 'point
                              :buffer buffer
                              :line line
                              :charpos charpos
                              :kind kind)))
    (unless (eq :temporary kind)
      (push point (line-points line)))
    point))

(defun copy-point (point &optional kind)
  (make-point (point-buffer point)
              (point-line point)
	      (point-charpos point)
	      :kind (or kind (point-kind point))))

(defun delete-point (point)
  (unless (point-temporary-p point)
    (setf (line-points (point-line point))
          (delete point (line-points (point-line point))))))

(defun point-change-line (point new-line)
  (unless (point-temporary-p point)
    (let ((old-line (point-line point)))
      (do ((scan (line-points old-line) (cdr scan))
           (prev nil scan))
          ((eq (car scan) point)
           (if prev
               (setf (cdr prev) (cdr scan))
               (setf (line-points old-line) (cdr scan)))
           (setf (cdr scan) (line-points new-line)
                 (line-points new-line) scan)))))
  (setf (point-line point) new-line))

(defun point-temporary-p (point)
  (eq (point-kind point) :temporary))

(defun point-line-ord (point)
  (line-ord (point-line point)))

(defun point= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (and (= (point-line-ord point1)
          (point-line-ord point2))
       (= (point-charpos point1)
          (point-charpos point2))))

(defun point/= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (not (point= point1 point2)))

(defun point< (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (or (< (point-line-ord point1) (point-line-ord point2))
      (and (= (point-line-ord point1) (point-line-ord point2))
           (< (point-charpos point1) (point-charpos point2)))))

(defun point<= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (or (point< point1 point2)
      (point= point1 point2)))

(defun point> (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (point< point2 point1))

(defun point>= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (point<= point2 point1))

(in-package :lem-base)

(annot:enable-annot-syntax)

(export '(save-excursion
          with-point
          with-buffer-read-only
          without-interrupts))

(defmacro save-excursion (&body body)
  @lang(:jp "現在の`point`と`mark`を保存し、`body`の評価後に復元し`body`の結果を返します。  
`body`でエラーがあっても復元されます。")
  `(invoke-save-excursion (lambda () ,@body)))

(defmacro with-point (bindings &body body)
  @lang(:jp "このマクロは`body`内で使う各`point`を`bindings`で作り、
`body`を抜けると各`point`を削除して`body`の値を返します。  
`body`でエラーがあっても各`point`は削除されます。  
`bindings`の形式は(`var` `point` &optional `kind`)のリストです。  
`kind`は省略可能でデフォルトで`:temporary`です。  
```
例
\(with-point ((p3 expr1)
             (p1 expr2 :left-inserting)
             (p2 expr3 :right-inserting))
  ...)
```
")
  (let ((cleanups
         (mapcan (lambda (b)
                   (destructuring-bind (var point &optional (kind :temporary)) b
                     (declare (ignore point))
                     (unless (eq :temporary kind)
                       `((delete-point ,var)))))
                 bindings)))
    `(let ,(mapcar (lambda (b)
                     (destructuring-bind (var point &optional (kind :temporary)) b
                       `(,var (copy-point ,point ,kind))))
                   bindings)
       ,(if cleanups
            `(unwind-protect (progn ,@body)
               ,@cleanups)
            `(progn ,@body)))))

(defmacro with-buffer-read-only (buffer flag &body body)
  (let ((gbuffer (gensym "BUFFER"))
        (gtmp (gensym "GTMP")))
    `(let* ((,gbuffer ,buffer)
            (,gtmp (buffer-read-only-p ,gbuffer)))
       (setf (buffer-read-only-p ,gbuffer) ,flag)
       (unwind-protect (progn ,@body)
         (setf (buffer-read-only-p ,gbuffer) ,gtmp)))))

(defmacro without-interrupts (&body body)
  `(#+ccl ccl:without-interrupts
    #+sbcl sb-sys:without-interrupts
    #-(or ccl sbcl) progn
    ,@body))

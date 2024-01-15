(in-package :lem-base)

(defmacro with-point (bindings &body body)
  "This macro creates each `point` to be used in `body` with `bindings`.
When you exit `body`, it removes each `point` and returns the value of `body`.
If there is an error in `body`, each `point` is removed.
The format of `bindings` is a list of (`var` `point` &optional `kind`).
Example:
\(with-point ((p3 expr1)
             (p1 expr2 :left-inserting)
             (p2 expr3 :right-inserting))
  ...)
```
"
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

(defvar *interrupts-enabled* t)
(defvar *interrupted* nil)

(defmacro %without-interrupts (&body body)
  `(#+sbcl sb-sys:without-interrupts
    #+ccl ccl:without-interrupts
    #-(or sbcl ccl) progn
    ,@body))

(defmacro without-interrupts (&body body)
  (let ((prev-enabled (gensym)))
    `(let ((,prev-enabled *interrupts-enabled*)
           (*interrupts-enabled* nil))
       (prog1 (progn ,@body)
         (when (and *interrupted* ,prev-enabled)
           (%without-interrupts
             (setf *interrupted* nil)
             (error 'editor-interrupt)))))))

;; 別のスレッドから(bt:interrupt-thread thread #'interrupt)で使う関数
(defun interrupt (&optional force)
  (cond
    (force
     (error 'editor-interrupt))
    (*interrupts-enabled*
     (error 'editor-interrupt))
    (t
     (setf *interrupted* t))))

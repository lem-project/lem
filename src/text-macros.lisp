(in-package :lem)

(export '(save-excursion
          with-point
          with-buffer-read-only
          define-buffer-local-and-global-hook))

(defmacro save-excursion (&body body)
  `(invoke-save-excursion (lambda () ,@body)))

(defmacro with-point (bindings &body body)
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

(defmacro define-buffer-local-and-global-hook (name)
  (let ((global-name (intern (format nil "*~A*" name)))
        (keyword-name (intern (format nil "*~A*" name) :keyword)))
    `(progn

       (export ',global-name)

       (defvar ,global-name nil)

       (defun ,name (&key buffer-local-p)
         (if buffer-local-p
             (get-bvar ,keyword-name)
             (or (get-bvar ,keyword-name)
                 ,global-name)))

       (defun (setf ,name) (value &key buffer-local-p)
         (if buffer-local-p
             (setf (get-bvar ,keyword-name) value)
             (if (get-bvar ,keyword-name)
                 (setf (get-bvar ,keyword-name) value)
                 (setf ,global-name value)))))))

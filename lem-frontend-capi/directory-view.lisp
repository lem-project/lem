(in-package :lem-capi)

(defclass directory-view (capi:tree-view)
  ((callback
    :initform nil
    :initarg :callback
    :reader directory-view-callback))
  (:default-initargs
   :roots (list (cons t (user-homedir-pathname)))
   :leaf-node-p-function (lambda (node) (uiop:file-pathname-p (cdr node)))
   :callback-type :element-data
   :selection-callback (lambda (self node)
                         (when-let (f (directory-view-callback self))
                           (funcall f (cdr node))))
   :print-function (lambda (node)
                     (destructuring-bind (rootp . pathname) node
                       (if rootp
                           (namestring pathname)
                           (if (uiop:directory-pathname-p pathname)
                               (enough-namestring
                                pathname
                                (uiop:pathname-parent-directory-pathname
                                 pathname))
                               (enough-namestring pathname
                                                  (make-pathname :directory (pathname-directory pathname)))))))
   :children-function (lambda (node)
                        (destructuring-bind (rootp . pathname) node
                          (declare (ignore rootp))
                          (when (uiop:directory-pathname-p pathname)
                            (mapcar (lambda (pathname)
                                      (cons nil pathname))
                                    (sort (lem-base:list-directory pathname)
                                          #'string<
                                          :key #'princ-to-string)))))))

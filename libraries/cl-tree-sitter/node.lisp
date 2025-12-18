(in-package :cl-tree-sitter/node)

;;;; Tree Operations

(defun tree-root-node (tree)
  "Get the root node of a syntax tree."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-tree-root-node (types:ts-tree-ptr tree) buffer)
    (make-instance 'types:ts-node :tree tree :buffer buffer)))

(defun tree-edit (tree edit)
  "Apply an edit to a tree for incremental parsing.
   EDIT should be a ts-input-edit structure."
  (cffi:with-foreign-object (edit-ptr '(:struct ffi:ts-input-edit))
    ;; Fill in the edit struct
    (setf (cffi:foreign-slot-value edit-ptr '(:struct ffi:ts-input-edit) 'ffi::start-byte)
          (types:ts-input-edit-start-byte edit))
    (setf (cffi:foreign-slot-value edit-ptr '(:struct ffi:ts-input-edit) 'ffi::old-end-byte)
          (types:ts-input-edit-old-end-byte edit))
    (setf (cffi:foreign-slot-value edit-ptr '(:struct ffi:ts-input-edit) 'ffi::new-end-byte)
          (types:ts-input-edit-new-end-byte edit))
    ;; Fill points
    (let ((start-point (cffi:foreign-slot-pointer edit-ptr '(:struct ffi:ts-input-edit)
                                                  'ffi::start-point)))
      (when (types:ts-input-edit-start-point edit)
        (setf (cffi:foreign-slot-value start-point '(:struct ffi:ts-point) 'ffi::row)
              (types:ts-point-row (types:ts-input-edit-start-point edit)))
        (setf (cffi:foreign-slot-value start-point '(:struct ffi:ts-point) 'ffi::column)
              (types:ts-point-column (types:ts-input-edit-start-point edit)))))
    (let ((old-end-point (cffi:foreign-slot-pointer edit-ptr '(:struct ffi:ts-input-edit)
                                                    'ffi::old-end-point)))
      (when (types:ts-input-edit-old-end-point edit)
        (setf (cffi:foreign-slot-value old-end-point '(:struct ffi:ts-point) 'ffi::row)
              (types:ts-point-row (types:ts-input-edit-old-end-point edit)))
        (setf (cffi:foreign-slot-value old-end-point '(:struct ffi:ts-point) 'ffi::column)
              (types:ts-point-column (types:ts-input-edit-old-end-point edit)))))
    (let ((new-end-point (cffi:foreign-slot-pointer edit-ptr '(:struct ffi:ts-input-edit)
                                                    'ffi::new-end-point)))
      (when (types:ts-input-edit-new-end-point edit)
        (setf (cffi:foreign-slot-value new-end-point '(:struct ffi:ts-point) 'ffi::row)
              (types:ts-point-row (types:ts-input-edit-new-end-point edit)))
        (setf (cffi:foreign-slot-value new-end-point '(:struct ffi:ts-point) 'ffi::column)
              (types:ts-point-column (types:ts-input-edit-new-end-point edit)))))
    ;; Apply the edit
    (ffi:ts-tree-edit (types:ts-tree-ptr tree) edit-ptr))
  tree)

(defun tree-copy (tree)
  "Create a shallow copy of a tree."
  (make-instance 'types:ts-tree
                 :ptr (ffi:ts-tree-copy (types:ts-tree-ptr tree))
                 :source (types:ts-tree-source tree)))

;;;; Node Accessors

(defun node-type (node)
  "Get the type name of a node."
  (ffi:ts-node-type (types:ts-node-buffer node)))

(defun node-symbol (node)
  "Get the symbol ID of a node."
  (ffi:ts-node-symbol (types:ts-node-buffer node)))

(defun node-start-byte (node)
  "Get the start byte offset of a node."
  (ffi:ts-node-start-byte (types:ts-node-buffer node)))

(defun node-end-byte (node)
  "Get the end byte offset of a node."
  (ffi:ts-node-end-byte (types:ts-node-buffer node)))

(defun node-start-point (node)
  "Get the start position of a node as (row . column)."
  (cffi:with-foreign-object (point '(:struct ffi:ts-point))
    (ffi:ts-node-start-point (types:ts-node-buffer node) point)
    (make-point (ffi:ts-point-row point)
                (ffi:ts-point-column point))))

(defun node-end-point (node)
  "Get the end position of a node as (row . column)."
  (cffi:with-foreign-object (point '(:struct ffi:ts-point))
    (ffi:ts-node-end-point (types:ts-node-buffer node) point)
    (make-point (ffi:ts-point-row point)
                (ffi:ts-point-column point))))

(defun node-range (node)
  "Get the byte range of a node as (start-byte . end-byte)."
  (cons (node-start-byte node) (node-end-byte node)))

(defun node-child-count (node)
  "Get the number of children of a node."
  (ffi:ts-node-child-count (types:ts-node-buffer node)))

(defun node-named-child-count (node)
  "Get the number of named children of a node."
  (ffi:ts-node-named-child-count (types:ts-node-buffer node)))

(defun node-child (node index)
  "Get a child node by index."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-node-child (types:ts-node-buffer node) index buffer)
    (if (ffi:ts-node-is-null buffer)
        (progn (cffi:foreign-free buffer) nil)
        (make-instance 'types:ts-node
                       :tree (types:ts-node-tree node)
                       :buffer buffer))))

(defun node-named-child (node index)
  "Get a named child node by index."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-node-named-child (types:ts-node-buffer node) index buffer)
    (if (ffi:ts-node-is-null buffer)
        (progn (cffi:foreign-free buffer) nil)
        (make-instance 'types:ts-node
                       :tree (types:ts-node-tree node)
                       :buffer buffer))))

(defun node-children (node)
  "Get all children of a node as a list."
  (loop :for i :below (node-child-count node)
        :for child := (node-child node i)
        :when child :collect child))

(defun node-named-children (node)
  "Get all named children of a node as a list."
  (loop :for i :below (node-named-child-count node)
        :for child := (node-named-child node i)
        :when child :collect child))

(defun node-parent (node)
  "Get the parent of a node."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-node-parent (types:ts-node-buffer node) buffer)
    (if (ffi:ts-node-is-null buffer)
        (progn (cffi:foreign-free buffer) nil)
        (make-instance 'types:ts-node
                       :tree (types:ts-node-tree node)
                       :buffer buffer))))

(defun node-next-sibling (node)
  "Get the next sibling of a node."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-node-next-sibling (types:ts-node-buffer node) buffer)
    (if (ffi:ts-node-is-null buffer)
        (progn (cffi:foreign-free buffer) nil)
        (make-instance 'types:ts-node
                       :tree (types:ts-node-tree node)
                       :buffer buffer))))

(defun node-prev-sibling (node)
  "Get the previous sibling of a node."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-node-prev-sibling (types:ts-node-buffer node) buffer)
    (if (ffi:ts-node-is-null buffer)
        (progn (cffi:foreign-free buffer) nil)
        (make-instance 'types:ts-node
                       :tree (types:ts-node-tree node)
                       :buffer buffer))))

(defun node-next-named-sibling (node)
  "Get the next named sibling of a node."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-node-next-named-sibling (types:ts-node-buffer node) buffer)
    (if (ffi:ts-node-is-null buffer)
        (progn (cffi:foreign-free buffer) nil)
        (make-instance 'types:ts-node
                       :tree (types:ts-node-tree node)
                       :buffer buffer))))

(defun node-prev-named-sibling (node)
  "Get the previous named sibling of a node."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-node-prev-named-sibling (types:ts-node-buffer node) buffer)
    (if (ffi:ts-node-is-null buffer)
        (progn (cffi:foreign-free buffer) nil)
        (make-instance 'types:ts-node
                       :tree (types:ts-node-tree node)
                       :buffer buffer))))

;;;; Node Predicates

(defun node-null-p (node)
  "Check if a node is null."
  (ffi:ts-node-is-null (types:ts-node-buffer node)))

(defun node-named-p (node)
  "Check if a node is named (not anonymous)."
  (ffi:ts-node-is-named (types:ts-node-buffer node)))

(defun node-missing-p (node)
  "Check if a node is missing (inserted by error recovery)."
  (ffi:ts-node-is-missing (types:ts-node-buffer node)))

(defun node-extra-p (node)
  "Check if a node is extra (e.g., comments)."
  (ffi:ts-node-is-extra (types:ts-node-buffer node)))

(defun node-has-error-p (node)
  "Check if a node or its descendants have errors."
  (ffi:ts-node-has-error (types:ts-node-buffer node)))

(defun node-string (node)
  "Get S-expression representation of a node."
  (ffi:ts-node-string (types:ts-node-buffer node)))

;;;; Tree Cursor

(defmacro with-tree-cursor ((cursor node) &body body)
  "Execute BODY with a tree cursor starting at NODE.
   The cursor is automatically cleaned up on exit."
  (let ((cursor-ptr (gensym "CURSOR-PTR"))
        (node-var (gensym "NODE")))
    `(let ((,node-var ,node))
       (cffi:with-foreign-object (,cursor-ptr '(:struct ffi:ts-tree-cursor))
         (ffi:ts-tree-cursor-new (types:ts-node-buffer ,node-var) ,cursor-ptr)
         (let ((,cursor ,cursor-ptr))
           (unwind-protect
                (progn ,@body)
             (ffi:ts-tree-cursor-delete ,cursor)))))))

(defun cursor-current-node (cursor tree)
  "Get the current node of a cursor."
  (let ((buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (ffi:ts-tree-cursor-current-node cursor buffer)
    (make-instance 'types:ts-node :tree tree :buffer buffer)))

(defun cursor-current-field-name (cursor)
  "Get the field name of the current node."
  (ffi:ts-tree-cursor-current-field-name cursor))

(defun cursor-goto-parent (cursor)
  "Move cursor to parent. Returns T if successful."
  (ffi:ts-tree-cursor-goto-parent cursor))

(defun cursor-goto-next-sibling (cursor)
  "Move cursor to next sibling. Returns T if successful."
  (ffi:ts-tree-cursor-goto-next-sibling cursor))

(defun cursor-goto-first-child (cursor)
  "Move cursor to first child. Returns T if successful."
  (ffi:ts-tree-cursor-goto-first-child cursor))

;;;; Point and Edit Construction

(defun make-point (row column)
  "Create a point (row, column position)."
  (types:make-ts-point row column))

(defun point-row (point)
  "Get the row of a point."
  (types:ts-point-row point))

(defun point-column (point)
  "Get the column of a point."
  (types:ts-point-column point))

(defun make-input-edit (start-byte old-end-byte new-end-byte
                        &key start-point old-end-point new-end-point)
  "Create an input edit for incremental parsing."
  (types:make-ts-input-edit start-byte old-end-byte new-end-byte
                            start-point old-end-point new-end-point))

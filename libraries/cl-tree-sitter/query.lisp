(in-package :tree-sitter/query)

;;;; Query Error Types

(deftype query-error-type ()
  '(member nil :syntax :node-type :field :capture :structure :language))

(defun error-type-from-code (code)
  "Convert tree-sitter error code to keyword."
  (case code
    (0 nil)
    (1 :syntax)
    (2 :node-type)
    (3 :field)
    (4 :capture)
    (5 :structure)
    (6 :language)
    (otherwise :unknown)))

;;;; Query Compilation

(define-condition query-compile-error (error)
  ((source :initarg :source :reader query-compile-error-source)
   (offset :initarg :offset :reader query-compile-error-offset)
   (error-type :initarg :error-type :reader query-compile-error-type))
  (:report (lambda (c stream)
             (format stream "Query compile error (~A) at offset ~D in: ~A"
                     (query-compile-error-type c)
                     (query-compile-error-offset c)
                     (query-compile-error-source c)))))

(defun query-compile (language source)
  "Compile a query from SOURCE string for LANGUAGE.
   Returns a ts-query object or signals query-compile-error."
  (let ((lang-ptr (etypecase language
                    (types:ts-language (types:ts-language-ptr language))
                    (cffi:foreign-pointer language))))
    (multiple-value-bind (query-ptr error-offset error-type)
        (ffi:ts-query-new lang-ptr source)
      (if query-ptr
          ;; Build capture names vector
          (let* ((capture-count (ffi:ts-query-capture-count query-ptr))
                 (capture-names (make-array capture-count :element-type 'string)))
            (dotimes (i capture-count)
              (setf (aref capture-names i)
                    (or (ffi:ts-query-capture-name-for-id query-ptr i) "")))
            (make-instance 'types:ts-query
                           :ptr query-ptr
                           :language language
                           :capture-names capture-names))
          ;; Error
          (error 'query-compile-error
                 :source source
                 :offset error-offset
                 :error-type (error-type-from-code error-type))))))

(defun query-delete (query)
  "Explicitly delete a query (normally handled by GC)."
  (trivial-garbage:cancel-finalization query)
  (ffi:ts-query-delete (types:ts-query-ptr query)))

(defun query-pattern-count (query)
  "Get the number of patterns in a query."
  (ffi:ts-query-pattern-count (types:ts-query-ptr query)))

(defun query-capture-count (query)
  "Get the number of capture names in a query."
  (ffi:ts-query-capture-count (types:ts-query-ptr query)))

(defun get-capture-name-by-index (query index)
  "Get a capture name by index from a query."
  (aref (types:ts-query-capture-names query) index))

;;;; Query Cursor

(defmacro with-query-cursor ((cursor) &body body)
  "Execute BODY with a query cursor bound to CURSOR."
  `(let ((,cursor (ffi:ts-query-cursor-new)))
     (unwind-protect
          (progn ,@body)
       (ffi:ts-query-cursor-delete ,cursor))))

(defun query-exec (cursor query node)
  "Execute a query on a node using a cursor."
  (ffi:ts-query-cursor-exec cursor
                            (types:ts-query-ptr query)
                            (types:ts-node-buffer node)))

;;;; Query Match Iteration

(defun query-matches (query node)
  "Get all matches of a query on a node."
  (with-query-cursor (cursor)
    (query-exec cursor query node)
    (cffi:with-foreign-object (match-ptr '(:struct ffi:ts-query-match))
      (loop :while (ffi:ts-query-cursor-next-match cursor match-ptr)
            :collect (extract-match query node match-ptr)))))

(defun query-captures (query node)
  "Get all captures of a query on a node as a flat list."
  (with-query-cursor (cursor)
    (query-exec cursor query node)
    (cffi:with-foreign-objects ((match-ptr '(:struct ffi:ts-query-match))
                                (capture-index :uint32))
      (loop :while (ffi:ts-query-cursor-next-capture cursor match-ptr capture-index)
            :collect (extract-single-capture query node match-ptr
                                             (cffi:mem-ref capture-index :uint32))))))

(defun query-captures-in-range (query node start-byte end-byte)
  "Get captures within a byte range."
  (with-query-cursor (cursor)
    (ffi:ts-query-cursor-set-byte-range cursor start-byte end-byte)
    (query-exec cursor query node)
    (cffi:with-foreign-objects ((match-ptr '(:struct ffi:ts-query-match))
                                (capture-index :uint32))
      (loop :while (ffi:ts-query-cursor-next-capture cursor match-ptr capture-index)
            :collect (extract-single-capture query node match-ptr
                                             (cffi:mem-ref capture-index :uint32))))))

;;;; Match/Capture Extraction

(defun extract-match (query node match-ptr)
  "Extract a match structure from foreign memory."
  (let* ((pattern-index (cffi:foreign-slot-value match-ptr '(:struct ffi:ts-query-match)
                                                 'ffi::pattern-index))
         (capture-count (cffi:foreign-slot-value match-ptr '(:struct ffi:ts-query-match)
                                                 'ffi::capture-count))
         (captures-ptr (cffi:foreign-slot-value match-ptr '(:struct ffi:ts-query-match)
                                                'ffi::captures))
         (captures (loop :for i :below capture-count
                         :collect (extract-capture-at query node captures-ptr i))))
    (types:make-query-match pattern-index captures)))

(defun extract-capture-at (query node captures-ptr index)
  "Extract a single capture from the captures array."
  (let* ((capture-ptr (cffi:mem-aptr captures-ptr '(:struct ffi:ts-query-capture) index))
         (capture-index (cffi:foreign-slot-value capture-ptr '(:struct ffi:ts-query-capture)
                                                 'ffi::index))
         (node-ptr (cffi:foreign-slot-pointer capture-ptr '(:struct ffi:ts-query-capture)
                                              'ffi::node))
         ;; Copy the node data
         (node-buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (cffi:foreign-funcall "memcpy"
                          :pointer node-buffer
                          :pointer node-ptr
                          :size (cffi:foreign-type-size '(:struct ffi:ts-node-raw))
                          :pointer)
    (let ((captured-node (make-instance 'types:ts-node
                                        :tree (types:ts-node-tree node)
                                        :buffer node-buffer)))
      (types:make-query-capture captured-node
                                capture-index
                                (get-capture-name-by-index query capture-index)))))

(defun extract-single-capture (query node match-ptr capture-index)
  "Extract a single capture from a match at the given index."
  (let* ((captures-ptr (cffi:foreign-slot-value match-ptr '(:struct ffi:ts-query-match)
                                                'ffi::captures)))
    (extract-capture-at query node captures-ptr capture-index)))

;;;; Match/Capture Accessors

(defun match-pattern-index (match)
  "Get the pattern index of a match."
  (types:query-match-pattern-index match))

(defun match-captures (match)
  "Get the captures of a match."
  (types:query-match-captures match))

(defun capture-node (capture)
  "Get the node of a capture."
  (types:query-capture-node capture))

(defun capture-index (capture)
  "Get the index of a capture."
  (types:query-capture-index capture))

(defun capture-name (capture)
  "Get the name of a capture."
  (types:query-capture-name capture))

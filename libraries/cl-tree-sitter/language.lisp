(in-package :tree-sitter/language)

;;;; Language Registry

(defvar *languages* (make-hash-table :test 'equal)
  "Registry of loaded languages.")

(defun register-language (name language)
  "Register a language under NAME."
  (setf (gethash name *languages*) language))

(defun get-language (name)
  "Get a registered language by name."
  (gethash name *languages*))

(defun list-languages ()
  "List all registered language names."
  (alexandria:hash-table-keys *languages*))

;;;; Language Loading

(define-condition language-load-error (error)
  ((name :initarg :name :reader language-load-error-name)
   (path :initarg :path :reader language-load-error-path)
   (reason :initarg :reason :reader language-load-error-reason))
  (:report (lambda (c stream)
             (format stream "Failed to load language ~A from ~A: ~A"
                     (language-load-error-name c)
                     (language-load-error-path c)
                     (language-load-error-reason c)))))

(defun load-language (name library-path &key (symbol-name nil) (register t))
  "Load a tree-sitter language from a shared library.
   NAME: Language name (e.g., \"json\", \"c\")
   LIBRARY-PATH: Path to the grammar shared library
   SYMBOL-NAME: C function name (default: tree_sitter_NAME)
   REGISTER: If true, register the language for later use"
  (let ((sym-name (or symbol-name
                      (format nil "tree_sitter_~A" (substitute #\_ #\- name)))))
    ;; Load the library
    (handler-case
        (cffi:load-foreign-library library-path)
      (error (e)
        (error 'language-load-error
               :name name
               :path library-path
               :reason (format nil "Failed to load library: ~A" e))))
    ;; Get the language function
    (let ((lang-fn (cffi:foreign-symbol-pointer sym-name)))
      (unless lang-fn
        (error 'language-load-error
               :name name
               :path library-path
               :reason (format nil "Symbol ~A not found" sym-name)))
      ;; Call the function to get the language pointer
      (let ((lang-ptr (cffi:foreign-funcall-pointer lang-fn () :pointer)))
        (when (or (null lang-ptr) (cffi:null-pointer-p lang-ptr))
          (error 'language-load-error
                 :name name
                 :path library-path
                 :reason "Language function returned null"))
        ;; Create language object
        (let ((language (make-instance 'types:ts-language
                                       :ptr lang-ptr
                                       :name name)))
          (when register
            (register-language name language))
          language)))))

(defun find-grammar-in-ld-library-path (name)
  "Search for a tree-sitter grammar in LD_LIBRARY_PATH.
   Nix grammars are stored as 'parser' files in directories.
   Handles both /lib paths and direct grammar directory paths."
  (let ((ld-path (uiop:getenv "LD_LIBRARY_PATH")))
    (when ld-path
      (dolist (dir (uiop:split-string ld-path :separator ":"))
        (let ((dir-pathname (uiop:ensure-directory-pathname dir)))
          ;; Check for Nix-style grammar (tree-sitter-NAME-grammar pattern)
          (when (search (format nil "tree-sitter-~A" name) dir)
            ;; Check parser in current directory
            (let ((parser-path (merge-pathnames "parser" dir-pathname)))
              (when (probe-file parser-path)
                (return-from find-grammar-in-ld-library-path (namestring parser-path))))
            ;; Check parser in parent directory (for /lib paths from makeLibraryPath)
            (when (string= "lib" (car (last (pathname-directory dir-pathname))))
              (let* ((parent-dir (make-pathname :directory (butlast (pathname-directory dir-pathname))
                                                :defaults dir-pathname))
                     (parser-path (merge-pathnames "parser" (uiop:ensure-directory-pathname parent-dir))))
                (when (probe-file parser-path)
                  (return-from find-grammar-in-ld-library-path (namestring parser-path))))))
          ;; Check for parser file directly in directory (generic case)
          (let ((parser-path (merge-pathnames "parser" dir-pathname)))
            (when (and (search name dir) (probe-file parser-path))
              (return-from find-grammar-in-ld-library-path (namestring parser-path)))))))))

(defun load-language-from-system (name &key (symbol-name nil) (register t))
  "Load a language from the system library path.
   Attempts common naming conventions for grammar libraries."
  ;; First try to find Nix-style grammar in LD_LIBRARY_PATH
  (alexandria:when-let ((nix-path (find-grammar-in-ld-library-path name)))
    (handler-case
        (return-from load-language-from-system
          (load-language name nix-path
                         :symbol-name symbol-name
                         :register register))
      (language-load-error () nil)))
  ;; Fall back to traditional library names
  (let ((lib-names (list (format nil "libtree-sitter-~A.so" name)
                         (format nil "tree-sitter-~A.so" name)
                         (format nil "libtree-sitter-~A.dylib" name)
                         (format nil "tree-sitter-~A.dylib" name))))
    (dolist (lib-name lib-names)
      (handler-case
          (return-from load-language-from-system
            (load-language name lib-name
                           :symbol-name symbol-name
                           :register register))
        (language-load-error () nil)))
    ;; All attempts failed
    (error 'language-load-error
           :name name
           :path "(system)"
           :reason "Could not find grammar library in system path")))

;;;; Language Accessors

(defun language-version (language)
  "Get the ABI version of a language."
  (ffi:ts-language-version (types:ts-language-ptr language)))

(defun language-symbol-count (language)
  "Get the number of symbols in a language."
  (ffi:ts-language-symbol-count (types:ts-language-ptr language)))

(defun language-symbol-name (language symbol-id)
  "Get the name of a symbol by ID."
  (ffi:ts-language-symbol-name (types:ts-language-ptr language) symbol-id))

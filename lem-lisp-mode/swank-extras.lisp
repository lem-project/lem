(ql:quickload '(:arnesi) :silent t)

(defpackage :swank/lem-extras
  (:use :cl)
  (:export :walk))
(in-package :swank/lem-extras)

(defstruct global-env
  package
  export-symbols
  definitions)

(defvar *global-env*)
(defvar *file-position* nil)

(defun append-definition (name type form)
  (when (and (symbolp name)
             (not (null (symbol-package name))))
    (push (list type (string name) form *file-position*)
          (global-env-definitions *global-env*))
    nil))

(defun append-export-symbols (symbols)
  (dolist (s symbols)
    (when (and (symbolp s)
               (not (member s (global-env-export-symbols *global-env*) :test #'equal)))
      (push (string s) (global-env-export-symbols *global-env*)))))

(defun copy-hashtable (ht)
  (let ((ht2 (make-hash-table :test (hash-table-test ht))))
    (maphash (lambda (k v)
               (setf (gethash k ht2) v))
             ht)
    ht2))

(defvar *walker-handlers*
  (let ((arnesi::*walker-handlers*
          (copy-hashtable arnesi::*walker-handlers*)))

    (arnesi:defwalker-handler in-package (form parent env)
      (let ((package (find-package (second form))))
        (when package
          (setf (global-env-package *global-env*) package))))

    (arnesi:defwalker-handler defconstant (form parent env)
      (append-definition (second form) :defconstant form))

    (arnesi:defwalker-handler defparameter (form parent env)
      (append-definition (second form) :defparameter form))

    (arnesi:defwalker-handler defvar (form parent env)
      (append-definition (second form) :defvar form))

    (arnesi:defwalker-handler defmacro (form parent env)
      (append-definition (second form) :defmacro form))

    (arnesi:defwalker-handler defsetf (form parent env)
      (let ((name (second form)))
        (append-definition name :defsetf form)))

    (arnesi:defwalker-handler defun (form parent env)
      (let ((name (second form)))
        (if (and (consp name)
                 (eq 'setf (first name))
                 (symbolp (second name)))
            (append-definition name :defsetf form)
            (append-definition name :defun form))))

    (arnesi:defwalker-handler defmethod (form parent env)
      (append-definition (second form) :defmethod form))

    (arnesi:defwalker-handler defgeneric (form parent env)
      (append-definition (second form) :defgeneric form))

    (arnesi:defwalker-handler deftype (form parent env)
      (append-definition (second form) :deftype form))

    (arnesi:defwalker-handler defclass (form parent env)
      (when (<= 4 (length form))
        (destructuring-bind (name parents slots &rest options) (cdr form)
          (declare (ignore parents options))
          (append-definition name :defclass form)
          (dolist (slot slots)
            (let* ((slot-options (cdr slot))
                   (reader (getf slot-options :reader))
                   (writer (getf slot-options :writer))
                   (accessor (getf slot-options :accessor)))
              (when reader (append-definition reader :reader form))
              (when writer (append-definition writer :writer form))
              (when accessor (append-definition accessor :accessor form)))))))

    (arnesi:defwalker-handler defstruct (form parent env)
      (append-definition (second form) :type form)
      (funcall (gethash 'arnesi::application arnesi::*walker-handlers*)
               form parent env))

    (arnesi:defwalker-handler defpackage (form parent env)
      (append-export-symbols (cdr (assoc :export (cddr form)))))

    (arnesi:defwalker-handler export (form parent env)
      (when (cdr form)
        (let ((arg (cadr form)))
          (when (and (consp arg)
                     (eq 'quote (car arg))
                     (null (cddr arg)))
            (append-export-symbols (uiop:ensure-list (cadr arg)))))))

    (arnesi:defwalker-handler declare (form parent env)
      (declare (ignore form)))

    (copy-hashtable arnesi::*walker-handlers*)))

(defun normalize-definitions (definitions)
  (let ((ht (make-hash-table)))
    (loop :for (name . type) :in definitions
          :do (pushnew name (gethash type ht)))
    (let ((new-definitions '()))
      (maphash (lambda (type names)
                 (push (cons type (mapcar #'symbol-name names)) new-definitions))
               ht)
      (nreverse new-definitions))))

(defun walk (text)
  (let ((*global-env* (make-global-env :package (when (boundp 'swank::*buffer-package*)
                                                  swank::*buffer-package*)))
        (arnesi::*walker-handlers* *walker-handlers*))
    (with-input-from-string (stream text)
      (loop :with eof-value := '#:eof
            :for form := (ignore-errors
                          (let ((*package* (or (global-env-package *global-env*)
                                               *package*)))
                            (read stream nil eof-value)))
            :until (eq eof-value form)
            :do (handler-bind ((arnesi:return-from-unknown-block
                                 (lambda (c)
                                   (declare (ignore c))
                                   (invoke-restart 'arnesi::add-block))))
                  (let ((*file-position* (file-position stream)))
                    (arnesi:walk-form form)))))
    (nreverse (global-env-definitions *global-env*))))

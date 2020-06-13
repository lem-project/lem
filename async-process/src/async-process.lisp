(defpackage :async-process
  (:use :cl)
  (:export
   :delete-process
   :process-send-input
   :process-receive-output
   :process-alive-p
   :create-process))
(in-package :async-process)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun system (cmd)
    (ignore-errors (string-right-trim '(#\Newline) (uiop:run-program cmd :output :string)))))

(pushnew (asdf:system-relative-pathname :async-process
                                        (format nil "../static/~A/"
                                                (cond ((uiop/os:featurep '(:and :windows :x86-64))
                                                       "x86_64/windows")
                                                      ((uiop/os:featurep :windows) "x86/windows")
                                                      ((uiop/os:featurep :unix)
                                                       (format nil "~A/~A"
                                                               (system "uname -m")
                                                               (system "uname"))))))
         cffi:*foreign-library-directories*
         :test #'uiop:pathname-equal)

(cffi:define-foreign-library async-process
  (:unix "libasyncprocess.so")
  (:windows "libasyncprocess.dll"))

(cffi:use-foreign-library async-process)

(defclass process ()
  ((process :reader process-process :initarg :process)
   (encode :accessor process-encode :initarg :encode)))

(cffi:defcfun ("create_process" %create-process) :pointer
  (command :pointer)
  (nonblock :boolean)
  (path :string))

(cffi:defcfun ("delete_process" %delete-process) :void
  (process :pointer))

(cffi:defcfun ("process_pid" %process-pid) :int
  (process :pointer))

(cffi:defcfun ("process_send_input" %process-send-input) :void
  (process :pointer)
  (string :string))

(cffi:defcfun ("process_receive_output" %process-receive-output) :string
  (process :pointer))

(cffi:defcfun ("process_alive_p" %process-alive-p) :boolean
  (process :pointer))

(defun create-process (command &key nonblock (encode cffi:*default-foreign-encoding*) directory)
  (when (and directory (not (uiop:directory-exists-p directory)))
    (error "Directory ~S does not exist" directory))
  (let* ((command (uiop:ensure-list command))
         (length (length command)))
    (cffi:with-foreign-object (argv :string (1+ length))
      (loop :for i :from 0
            :for c :in command
            :do (setf (cffi:mem-aref argv :string i) c))
      (setf (cffi:mem-aref argv :string length) (cffi:null-pointer))
      (let ((p (%create-process argv nonblock (if directory
                                                  (namestring directory)
                                                  (cffi:null-pointer)))))
        (if (cffi:null-pointer-p p)
            (error "create-process failed: ~S" command)
            (make-instance 'process :process p :encode encode))))))

(defun delete-process (process)
  (%delete-process (process-process process)))

(defun process-pid (process)
  (%process-pid (process-process process)))

(defun process-send-input (process string)
  (let ((cffi:*default-foreign-encoding* (process-encode process)))
    (%process-send-input (process-process process) string)))

(defun process-receive-output (process)
  (let ((cffi:*default-foreign-encoding* (process-encode process)))
    (%process-receive-output (process-process process))))

(defun process-alive-p (process)
  (%process-alive-p (process-process process)))

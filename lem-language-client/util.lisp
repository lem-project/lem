(in-package :lem-language-client)

(defun getpid ()
  #+sbcl (sb-posix:getpid)
  #+ccl (ccl::getpid)
  #+lispworks (system::getpid))

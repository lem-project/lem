(defpackage :lem-tests/popup-window
  (:use :cl :lem-tests/deftest)
  (:import-from :lem-fake-interface
                :fake-interface))
(in-package :lem-tests/popup-window)

(defvar *popup-parameters* nil)

(defmethod lem.popup-window::display-popup-message-using-popup-parameters
    ((implementation fake-interface) popup-parameters)
  (setf *popup-parameters* popup-parameters))

(deftest display-popup-window
  (lem:with-interface (make-instance 'fake-interface)
    (let (*popup-parameters*)
      (lem:display-popup-message "hello")
      (let ((popup-parameters *popup-parameters*))
        (ok (lem.popup-window::popup-parameters-p popup-parameters))
        (ok (eql (lem.popup-window::popup-parameters-timeout popup-parameters)
                 lem::*default-popup-message-timeout*))
        (ok (eq (lem.popup-window::popup-parameters-gravity popup-parameters)
                :cursor))))))

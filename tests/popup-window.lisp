(defpackage :lem-tests/popup-window
  (:use :cl :lem-tests/deftest)
  (:import-from :lem-fake-interface
                :fake-interface)
  (:import-from :lem-tests/utilities
                :with-mock-functions))
(in-package :lem-tests/popup-window)

(deftest display-popup-window
  (let ((lem::*implementation* (make-instance 'fake-interface)))
    (lem::setup-first-frame)
    (let (popup-parameters)
      (with-mock-functions ((lem.popup-window::display-popup-buffer-default-impl
                                (arg)
                              (setf popup-parameters arg)))
        (lem:display-popup-message "hello"))
      (ok (lem.popup-window::popup-parameters-p popup-parameters))
      (ok (eql (lem.popup-window::popup-parameters-timeout popup-parameters)
               lem::*default-popup-message-timeout*))
      (ok (eq (lem.popup-window::popup-parameters-gravity popup-parameters)
              :cursor)))))

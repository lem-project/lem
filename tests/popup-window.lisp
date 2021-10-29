(defpackage :lem-tests/popup-window
  (:use :cl :lem-tests/deftest)
  (:import-from :lem-fake-interface
                :fake-interface
                :with-fake-interface))
(in-package :lem-tests/popup-window)

(defun find-popup-window ()
  (let ((windows (remove-if-not (lambda (window)
                                  (typep window 'lem.popup-window::popup-window))
                                (lem:frame-floating-windows (lem:current-frame)))))
    (assert (= 1 (length windows)))
    (first windows)))

(deftest display-popup-window
  (with-fake-interface ()
    (lem:display-popup-message "hello")
    (let ((popup-window (find-popup-window)))
      (ok (eql 1 (lem:floating-window-border popup-window)))
      (ok (eq (lem:current-window)
              (lem.popup-window::popup-window-source-window popup-window)))
      (ok (= 6 (lem.popup-window::popup-window-base-width popup-window)))
      (ok (= 1 (lem.popup-window::popup-window-base-height popup-window)))
      (ok (typep (lem.popup-window::popup-window-gravity popup-window)
                 'lem.popup-window::gravity-cursor)))))

(defpackage :lem-prolog/tests
  (:use :cl :rove :lem))
(in-package :lem-prolog/tests)

(deftest plain-query-output-matches-prolog-toplevel
  (testing "plain ?- queries keep Scryer's answer spacing verbatim"
    (with-current-buffers ()
      (let* ((buffer (make-buffer "*prolog-plain-query-test*"
                                  :temporary t
                                  :enable-undo-p nil))
             (session (lem-prolog.run-prolog::make-prolog-session))
             (answer (format nil "   X = 1~%;  X = 2~%;  X = 3~%;  X = 4~%;  X = 5.")))
        (setf (current-buffer) buffer)
        (insert-string (buffer-point buffer)
                       (format nil "?- member(X, [1,2,3,4,5]).~%"))
        (setf (lem-prolog.run-prolog::prolog-session-marker session)
              (make-buffer-point (buffer-end-point buffer)))
        (lem-prolog.run-prolog::prolog-set-query-prefix session #("" ""))
        (lem-prolog.run-prolog::prolog-insert-output session answer)
        (ok (string= (buffer-text buffer)
                     (format nil "?- member(X, [1,2,3,4,5]).~%~a" answer)))))))

(deftest percent-marked-query-output-uses-default-prefix
  (testing "%?- queries retain the configured interaction prefix"
    (with-current-buffers ()
      (let* ((buffer (make-buffer "*prolog-prefixed-query-test*"
                                  :temporary t
                                  :enable-undo-p nil))
             (session (lem-prolog.run-prolog::make-prolog-session)))
        (setf (current-buffer) buffer)
        (insert-string (buffer-point buffer)
                       (format nil "%?- member(X, [1,2]).~%"))
        (setf (lem-prolog.run-prolog::prolog-session-marker session)
              (make-buffer-point (buffer-end-point buffer)))
        (lem-prolog.run-prolog::prolog-set-query-prefix session #("" "%"))
        (lem-prolog.run-prolog::prolog-insert-output
         session (format nil "   X = 1~%;  X = 2."))
        (ok (string= (buffer-text buffer)
                     (format nil "%?- member(X, [1,2]).~%%@    X = 1~%%@ ;  X = 2.")))))))

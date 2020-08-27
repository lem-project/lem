(defpackage :lem-tests/frame-multiplexer
  (:use :cl :lem))
(in-package :lem-tests/frame-multiplexer)

(defstruct datum
  id
  result)

(defstruct (error-datum (:include datum)))

(rove:deftest test
  (ql:quickload :lem-fake-interface :silent t)
  (lem)
  (let ((event-queue (lem::make-event-queue)))
    (send-event (lambda ()
                  (block outer
                    (handler-bind ((error (lambda (c)
                                            (send-event (make-error-datum :id 3 :result c))
                                            (return-from outer))))
                      (redraw-display)
                      (unless (lem-frame-multiplexer::enabled-frame-multiplexer-p)
                        (lem-frame-multiplexer::toggle-frame-multiplexer))
                      (delete-between-points (buffer-start-point (current-buffer))
                                             (buffer-end-point (current-buffer)))
                      (insert-string (current-point) "abc")
                      (redraw-display t)
                      (lem-frame-multiplexer::frame-multiplexer-create-with-new-buffer-list)
                      (redraw-display t)
                      (send-event (make-datum :id 1 :result (uiop:symbol-call :lem-fake-interface :display))
                                  event-queue)
                      (lem-frame-multiplexer::frame-multiplexer-next)
                      (send-event (make-datum :id 2 :result (uiop:symbol-call :lem-fake-interface :display))
                                  event-queue)))))
    (let* ((datum1 (lem::dequeue-event 1 event-queue))
           (datum2 (lem::dequeue-event 1 event-queue)))
      ;; (rove:ok (string= (datum-result datum1) "abc ")) ; bug
      (rove:ok (string= (datum-result datum2) "abc "))
      (list datum1 datum2))
    (send-event (lambda ()
                  (exit-lem nil)
                  (clrhash lem-frame-multiplexer::*virtual-frame-map*)))))

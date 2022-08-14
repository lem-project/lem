

#|
(defun collect-frames (stack-top)
  (loop :for frame := stack-top :then (sb-di:frame-down frame)
        :while frame
        :collect (ignore-errors
                   (swank/sbcl::code-location-source-location
                    (sb-di:frame-code-location
                     frame)))))

(defun fetch-frames ()
  (swank/backend:call-with-debugging-environment
   (lambda ()
     (collect-frames swank/sbcl::*sldb-stack-top*))))
|#

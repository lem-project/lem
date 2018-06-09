(in-package :lem-capi)

(defclass lem-panel (capi:simple-layout)
  ((editor-pane
    :initarg :editor-pane
    :reader lem-panel-editor-pane)))

(defmethod initialize-instance ((lem-panel lem-panel) &rest initargs)
  (let ((lem-pane (make-instance 'lem-pane)))
    (apply #'call-next-method lem-panel
           :description (list lem-pane)
           :editor-pane lem-pane
           initargs)))

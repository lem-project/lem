(in-package :lem-capi)

(defclass lem-panel (capi:simple-layout)
  ((editor-pane
    :initarg :editor-pane
    :reader lem-panel-editor-pane)))

(defmethod initialize-instance ((lem-panel lem-panel) &rest initargs)
  (let ((editor-pane (make-instance 'editor-pane)))
    (apply #'call-next-method lem-panel
           :description (list editor-pane)
           :editor-pane editor-pane
           initargs)))

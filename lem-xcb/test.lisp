(in-package :lem)
;;
;; Not used! Overrides interface methods, just to see what's happening
;;
(defparameter *ethread* nil)

(defparameter *q* *standard-output*)
(defparameter *vw* 1)
(defmethod interface-invoke ((implementation (eql :xcb)) function)
  (let ((result nil))
    (unwind-protect
         (progn
	   (format t "interface-invoke function ~A ~A~&" function
		   (setf *ethread* (funcall function))
		   ))
      ;(lem.term:term-finalize)
      )
    (when (and (typep result 'exit-editor)
               (exit-editor-value result))
      (format *q* "~&~A~%" (exit-editor-value result)))))

(defmethod interface-display-background-mode ((implementation (eql :xcb)))
  (format *q* "i:background-mode~&")
  :dark)

(defmethod interface-update-foreground ((implementation (eql :xcb)) color-name)
  (break)
  (format *q* "i:update-foreground ~A~&" color-name)
)

(defmethod interface-update-background ((implementation (eql :xcb)) color-name)
  (format *q* "i:update-background ~A~&" color-name)
)

(defmethod interface-display-width ((implementation (eql :xcb)))
  (format *q* "i:display-width ~&" )
  64)

(defmethod interface-display-height ((implementation (eql :xcb)))
  (format *q* "i:display-height ~&" )
  
  24
  )

(defmethod interface-make-view
    ((implementation (eql :xcb)) window x y width height use-modeline)
  (format *q* "i:update-make-view win:~A x:~A y:~A w:~A h:~A modeline ~A~&"
	  window x y width height use-modeline)
  (incf *vw*))

(defmethod interface-delete-view ((implementation (eql :xcb)) view)
  (format *q* "i:interface-delete-view ~A~&" view)
)

(defmethod interface-clear ((implementation (eql :xcb)) view)
  (format *q* "i:interface-clear view:~A~&" view)
)

(defmethod interface-set-view-size ((implementation (eql :xcb)) view width height)
    (format *q* "i:interface-set-view-size: view: ~A w: ~A h:~A~&" view width height)
)

(defmethod interface-set-view-pos ((implementation (eql :xcb)) view x y)
  (format *q* "i:interface-set-pos: view: ~A x: ~A y ~A~&" view x y))

(defmethod interface-print ((implementation (eql :xcb)) view x y string attribute)
  (format *q* "i:interface-print: view: ~A x: ~A y ~A |~A| ~A~&" view x y string attribute)
  (format *q* "ensure-attr ~A~&" (lem:ensure-attribute attribute nil))
)

(defmethod interface-print-modeline ((implementation (eql :xcb)) view x y string attribute)
  (format *q* "i:interface-print-modeline: view ~A x: ~A y ~A |~A| ~A~&" view x y string attribute)
)
(defmethod interface-clear-eol ((implementation (eql :xcb)) view x y)
  (format *q* "i:interface-clear-eol view ~A x: ~A y ~A~&" view x y )
)

(defmethod interface-clear-eob ((implementation (eql :xcb)) view x y)
  (format *q* "i:interface-clear-eob view ~A x: ~A y ~A~&" view x y ))

(defmethod interface-move-cursor ((implementation (eql :xcb)) view x y)
  (format *q* "i:interface-move-cursor view ~A x: ~A y ~A~&" view x y ))

(defmethod interface-redraw-view-after ((implementation (eql :xcb)) view focus-window-p)
  (format *q* "i:interface-redraw after: view ~A : ~A~&" view focus-window-p)
)

(defmethod interface-update-display ((implementation (eql :xcb)))
  (format *q* "i:interface-redraw-display: ~&")
)

(defmethod interface-scroll ((implementation (eql :xcb)) view n)
    (format *q* "i:interface-scroll: view ~A ~A~&" view n )
)

(setf *implementation* :xcb)

(setq *native-scroll-support* nil)

(defpackage :lem-jsonrpc
  (:use :cl
        :lem-jsonrpc/utils
        :lem-jsonrpc/view)
  (:local-nicknames (:display :lem-core/display))
  (:export :program))
(in-package :lem-jsonrpc)

(defvar *mode*)
(defvar *port*)
(defvar *editor-thread*)

(pushnew :lem-jsonrpc *features*)

(defclass jsonrpc (lem:implementation)
  ((server :initform (jsonrpc:make-server)
           :reader jsonrpc-server)
   (display-width :initform 80
                  :accessor jsonrpc-display-width)
   (display-height :initform 24
                   :accessor jsonrpc-display-height)
   (background-color :accessor jsonrpc-background-color)
   (foreground-color :accessor jsonrpc-foreground-color))
  (:default-initargs
   :name :jsonrpc
   :redraw-after-modifying-floating-window nil
   :window-left-margin 0))

(defmethod resize-display ((jsonrpc jsonrpc) width height)
  (setf (jsonrpc-display-width jsonrpc) width
        (jsonrpc-display-height jsonrpc) height))

(defmethod notify ((jsonrpc jsonrpc) method argument)
  (pdebug "notify: ~A ~A" method (pretty-json argument))
  (let ((jsonrpc/connection:*connection*
          (jsonrpc/transport/interface:transport-connection
           (jsonrpc/class:jsonrpc-transport (jsonrpc-server jsonrpc)))))
    (jsonrpc:notify (jsonrpc-server jsonrpc) method argument)))

(defun ready (jsonrpc loaded-fn)
  (lambda (params)
    (log:info "ready" (pretty-json params))
    (with-error-handler ()
      (let ((width (gethash "width" params))
            (height (gethash "height" params))
            (foreground (gethash "foreground" params))
            (background (gethash "background" params)))
        (resize-display jsonrpc width height)
        (alexandria:when-let (color (lem:parse-color background))
          (setf (jsonrpc-background-color jsonrpc) color))
        (alexandria:when-let (color (lem:parse-color foreground))
          (setf (jsonrpc-foreground-color jsonrpc) color))
        (funcall loaded-fn)
        (hash "width" width
              "height" height)))))

(defmethod lem-if:invoke ((jsonrpc jsonrpc) function)
  (let ((ready nil))
    (setf *editor-thread*
          (funcall function
                   (lambda ()
                     (loop :until ready))))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "ready"
                    (ready jsonrpc
                           (lambda ()
                             (setf ready t))))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "input"
                    (lambda (args)
                      (input-callback jsonrpc args)))

    (lem:add-hook lem:*exit-editor-hook*
                  (lambda ()
                    (notify jsonrpc "exit" nil)
                    (uiop:quit 0)))

    (ecase *mode*
      (:tcp
       (jsonrpc:server-listen (jsonrpc-server jsonrpc)
                              :mode :tcp
                              :port *port*))
      (:stdio
       (jsonrpc:server-listen (jsonrpc-server jsonrpc)
                              :mode :stdio)))))

(defmethod lem-if:get-background-color ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  (jsonrpc-background-color jsonrpc))

(defmethod lem-if:get-foreground-color ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  (jsonrpc-foreground-color jsonrpc))

(defmethod lem-if:update-foreground ((jsonrpc jsonrpc) color-name)
  (with-error-handler ()
    (log:info jsonrpc color-name)
    (notify jsonrpc "update-foreground" color-name)))

(defmethod lem-if:update-background ((jsonrpc jsonrpc) color-name)
  (with-error-handler ()
    (log:info jsonrpc color-name)
    (notify jsonrpc "update-background" color-name)))

(defmethod lem-if:update-cursor-shape ((jsonrpc jsonrpc) cursor-type)
  (log:info jsonrpc cursor-type)
  ;; TODO
  )

(defmethod lem-if:display-width ((jsonrpc jsonrpc))
  (with-error-handler ()
    (log:info jsonrpc)
    (jsonrpc-display-width jsonrpc)))

(defmethod lem-if:display-height ((jsonrpc jsonrpc))
  (with-error-handler ()
    (log:info jsonrpc)
    (jsonrpc-display-height jsonrpc)))

(defmethod lem-if:display-title ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  ;; TODO
  )

(defmethod lem-if:set-display-title ((jsonrpc jsonrpc) title)
  (log:info jsonrpc)
  ;; TODO
  )

(defmethod lem-if:display-fullscreen-p ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  ;; TODO
  )

(defmethod lem-if:set-display-fullscreen-p ((jsonrpc jsonrpc) fullscreen-p)
  (log:info jsonrpc)
  ;; TODO
  )

(defmethod lem-if:make-view ((jsonrpc jsonrpc) window x y width height use-modeline)
  (log:info jsonrpc window x y width height use-modeline)
  (let ((view (make-view :window window
                         :x x
                         :y y
                         :width width
                         :height height
                         :use-modeline use-modeline
                         :kind (if (lem:floating-window-p window)
                                   "popup"
                                   "tile"))))
    (notify jsonrpc "make-view" view)
    view))

(defmethod lem-if:view-width ((jsonrpc jsonrpc) view)
  (log:info jsonrpc view)
  (view-width view))

(defmethod lem-if:view-height ((jsonrpc jsonrpc) view)
  (log:info jsonrpc view)
  (view-height view))

(defmethod lem-if:delete-view ((jsonrpc jsonrpc) view)
  (log:info jsonrpc view)
  (with-error-handler ()
    (notify jsonrpc "delete-view" (hash "viewInfo" view))))

(defmethod lem-if:clear ((jsonrpc jsonrpc) view)
  (log:info jsonrpc view)
  (with-error-handler ()
    (notify jsonrpc "clear" (hash "viewInfo" view))))

(defmethod lem-if:set-view-size ((jsonrpc jsonrpc) view width height)
  (log:info jsonrpc view width height)
  (with-error-handler ()
    (setf (view-width view) width
          (view-height view) height)
    (notify jsonrpc
            "resize-view"
            (hash "viewInfo" view
                    "width" width
                    "height" height))))

(defmethod lem-if:set-view-pos ((jsonrpc jsonrpc) view x y)
  (log:info jsonrpc view x y)
  (with-error-handler ()
    (setf (view-x view) x
          (view-y view) y)
    (notify jsonrpc
            "move-view"
            (hash "viewInfo" view
                    "x" x
                    "y" y))))

(defmethod lem-if:redraw-view-before ((jsonrpc jsonrpc) view)
  (log:info jsonrpc view)
  )

(defmethod lem-if:redraw-view-after ((jsonrpc jsonrpc) view)
  (log:info jsonrpc view)
  (with-error-handler ()
    (notify jsonrpc "touch" (hash "viewInfo" view))))

(defmethod lem-if:will-update-display ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  )

(defmethod lem-if:update-display ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  (with-error-handler ()
    (let ((view (lem:window-view (lem:current-window)))
          (x (lem:last-print-cursor-x (lem:current-window)))
          (y (lem:last-print-cursor-y (lem:current-window))))
      (notify jsonrpc
              "move-cursor"
              (hash "viewInfo" view "x" x "y" y)))
    (notify jsonrpc "touch" nil)))

#+(or)
(defmethod lem-if:display-popup-menu ((jsonrpc jsonrpc) items
                                       &key action-callback
                                            print-spec
                                            style
                                            max-display-items))
#+(or)(defmethod lem-if:popup-menu-update
    ((jsonrpc jsonrpc) popup-menu items &key print-spec max-display-items keep-focus))
#+(or)(defmethod lem-if:popup-menu-quit ((jsonrpc jsonrpc) popup-menu))
#+(or)(defmethod lem-if:popup-menu-down ((jsonrpc jsonrpc) popup-menu))
#+(or)(defmethod lem-if:popup-menu-up ((jsonrpc jsonrpc) popup-menu))
#+(or)(defmethod lem-if:popup-menu-first ((jsonrpc jsonrpc) popup-menu))
#+(or)(defmethod lem-if:popup-menu-last ((jsonrpc jsonrpc) popup-menu))
#+(or)(defmethod lem-if:popup-menu-select ((jsonrpc jsonrpc) popup-menu))
#+(or)(defmethod lem-if:display-context-menu ((jsonrpc jsonrpc) context-menu style))

(defmethod lem-if:clipboard-paste ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  ;; TODO
  )

(defmethod lem-if:clipboard-copy ((jsonrpc jsonrpc) text)
  (log:info jsonrpc text)
  ;; TODO
  )

(defmethod lem-if:increase-font-size ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  ;; TODO
  )
(defmethod lem-if:decrease-font-size ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  ;; TODO
  )

(defmethod lem-if:resize-display-before ((jsonrpc jsonrpc))
  (log:info jsonrpc))

(defmethod lem-if:get-font-list ((jsonrpc jsonrpc))
  (log:info jsonrpc))

(defmethod lem-if:get-mouse-position ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  (values 0 0))

(defmethod lem-if:get-char-width ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  ;; TODO
  1)
(defmethod lem-if:get-char-height ((jsonrpc jsonrpc))
  (log:info jsonrpc)
  ;; TODO
  1)

;;;;
(defun bool (x) (if x 'yason:true 'yason:false))

(defun ensure-rgb (color)
  (if (typep color 'lem-core::color)
      (lem:color-to-hex-string color)
      color))

(defmethod yason:encode ((attribute lem:attribute) &optional (stream *standard-output*))
  (with-error-handler ()
    (yason:with-output (stream)
      (yason:with-object ()
        (yason:encode-object-element "foreground" (ensure-rgb (lem:attribute-foreground attribute)))
        (yason:encode-object-element "background" (ensure-rgb (lem:attribute-background attribute)))
        (yason:encode-object-element "reverse" (bool (lem:attribute-reverse attribute)))
        (yason:encode-object-element "bold" (bool (lem:attribute-bold attribute)))
        (yason:encode-object-element "underline" (bool (lem:attribute-underline attribute)))))))

(defmethod yason:encode ((view view) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (view-id view))
      (yason:encode-object-element "x" (view-x view))
      (yason:encode-object-element "y" (view-y view))
      (yason:encode-object-element "width" (view-width view))
      (yason:encode-object-element "height" (view-height view))
      (yason:encode-object-element "use_modeline" (view-use-modeline view))
      (yason:encode-object-element "kind" (view-kind view)))))




;;; drawing
(defgeneric object-width (drawing-object))

(defmethod object-width ((drawing-object display:void-object))
  0)

(defmethod object-width ((drawing-object display:text-object))
  (lem-core:string-width (display:text-object-string drawing-object)))

(defmethod object-width ((drawing-object display:eol-cursor-object))
  0)

(defmethod object-width ((drawing-object display:extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object display:line-end-object))
  0)

(defmethod object-width ((drawing-object display:image-object))
  0)

(defgeneric draw-object (jsonrpc object x y view))

(defmethod draw-object (jsonrpc (object display:void-object) x y view)
  (values))

(defvar *put-target* :window)

(defun put (jsonrpc view x y string attribute)
  (with-error-handler ()
    (notify jsonrpc
            (ecase *put-target*
              (:window "put")
              (:modeline "modeline-put"))
            (hash "viewInfo" view
                  "x" x
                  "y" y
                  "text" string
                  "textWidth" (lem:string-width string)
                  "attribute" (lem:ensure-attribute attribute nil)))))

(defmethod draw-object (jsonrpc (object display:text-object) x y view)
  (let ((string (display:text-object-string object))
        (attribute (display:text-object-attribute object)))
    (when (and attribute (lem-core:cursor-attribute-p attribute))
      (lem-core::set-last-print-cursor (view-window view) x y))
    (put jsonrpc view x y string attribute)))

(defmethod draw-object (jsonrpc (object display:eol-cursor-object) x y view)
  (lem-core::set-last-print-cursor (view-window view) x y)
  (put jsonrpc view x y " "
       (lem:make-attribute
        :background
        (lem:color-to-hex-string (display:eol-cursor-object-color object)))))

(defmethod draw-object (jsonrpc (object display:extend-to-eol-object) x y view)
  (let ((width (lem-if:view-width (lem-core:implementation) view)))
    (when (< x width)
      (put jsonrpc view x y
           (make-string (- width x) :initial-element #\space)
           (lem:make-attribute
            :background
            (lem:color-to-hex-string (display:extend-to-eol-object-color object)))))))

(defmethod draw-object (jsonrpc (object display:line-end-object) x y view)
  (let ((string (display:text-object-string object))
        (attribute (display:text-object-attribute object)))
    (put jsonrpc
         view
         (+ x (display:line-end-object-offset object))
         y
         string
         attribute)))

(defmethod draw-object (jsonrpc (object display:image-object) x y view)
  (values))

(defun render-line (jsonrpc view x y objects)
  (loop :for object :in objects
        :do (draw-object jsonrpc object x y view)
            (incf x (object-width object))))

(defun render-line-from-behind (jsonrpc view y objects)
  (loop :with current-x := (view-width view)
        :for object :in objects
        :do (decf current-x (object-width object))
            (draw-object jsonrpc object current-x y view)))

(defmethod lem-if:render-line ((jsonrpc jsonrpc) view x y objects height)
  (with-error-handler ()
    (notify jsonrpc
            "clear-eol"
            (hash "viewInfo" view
                  "x" x
                  "y" y))
    (render-line jsonrpc view x y objects)))

(defmethod lem-if:render-line-on-modeline ((jsonrpc jsonrpc) view left-objects right-objects
                                           default-attribute height)
  (let ((*put-target* :modeline))
    (with-error-handler ()
      (notify jsonrpc
              "modeline-put"
              (hash "viewInfo" view
                    "x" 0
                    "y" 0
                    "text" (make-string (view-width view) :initial-element #\space)
                    "textWidth" (view-width view)
                    "attribute" default-attribute))
      (render-line jsonrpc view 0 0 left-objects)
      (render-line-from-behind jsonrpc view 0 right-objects))))

(defmethod lem-if:object-width ((jsonrpc jsonrpc) drawing-object)
  (object-width drawing-object))

(defmethod lem-if:object-height ((jsonrpc jsonrpc) drawing-object)
  1)

(defmethod lem-if:clear-to-end-of-window ((jsonrpc jsonrpc) view y)
  (notify jsonrpc "clear-eob"
          (hash "viewInfo" view
                "x" 0
                "y" y)))


;;;
(defconstant +abort+ 0)
(defconstant +keyevent+ 1)
(defconstant +resize+ 2)
(defconstant +command+ 3)
(defconstant +method+ 4)

(defvar *method-table* (make-hash-table :test 'equal))

(defmacro define-notification-method (name params &body body)
  `(setf (gethash ,name *method-table*)
         (lambda (&key ,@params) ,@body)))

(defun convert-keyevent (e)
  (let ((key (gethash "key" e))
        (ctrl (gethash "ctrl" e))
        (meta (gethash "meta" e))
        (super (gethash "super" e))
        (shift (gethash "shift" e)))
    (cond ((string= key " ") (setf key "Space")))
    (lem:make-key :ctrl ctrl
                  :meta meta
                  :super super
                  :shift (if (lem:insertion-key-sym-p key) nil shift)
                  :sym key)))

(defun input-callback (jsonrpc args)
  (handler-case
      (let ((kind (gethash "kind" args))
            (value (gethash "value" args)))
        (cond ((= kind +abort+)
               (lem:send-abort-event *editor-thread* nil))
              ((= kind +keyevent+)
               (when value
                 (let ((key (convert-keyevent value)))
                   (lem:send-event key))))
              ((= kind +resize+)
               (resize-display jsonrpc
                               (gethash "width" value)
                               (gethash "height" value))
               (lem:send-event :resize))
              ((= kind +command+)
               (error "unimplemented"))
              ((= kind +method+)
               (let* ((method (gethash (gethash "method" value) *method-table*))
                      (params (gethash "params" value))
                      (args
                        (loop :for k :being :the :hash-keys :in params :using (hash-value v)
                              :collect (intern (string-upcase k) :keyword)
                              :collect v)))
                 (lem:send-event (lambda () (apply method args)))))
              (t
               (error "unexpected kind: ~D" kind))))
    (error (e)
      (pdebug "input-callback: ~A ~A" e
              (with-output-to-string (stream)
                (let ((stream (yason:make-json-output-stream stream)))
                  (yason:encode args stream)))))))

;;;
(defparameter +command-line-spec+
  ;; TODO: more helpful documentation
  '((("mode" #\m) :type string :optional t :documentation "\"tcp\" or \"stdio\"")
    (("port" #\p) :type integer :optional nil :documentation "port of \"tcp\"")))

(defun program (&optional (args (uiop:command-line-arguments)))
  (command-line-arguments:handle-command-line
   +command-line-spec+
   (lambda (&key (mode "stdio") port)
     (cond ((string= mode "tcp")
            (unless port
              (command-line-arguments:show-option-help +command-line-spec+)
              (uiop:quit 1))
            (let ((*mode* :tcp)
                  (*port* port))
              (lem:lem)))
           ((string= mode "stdio")
            (let ((*mode* :stdio))
              (lem:lem)))
           (t
            (command-line-arguments:show-option-help +command-line-spec+)
            (uiop:quit 1))))
   :name "lem-rpc"
   :positional-arity 0
   :command-line args))

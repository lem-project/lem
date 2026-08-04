(defpackage :lem-server
  (:use :cl
        :lem-server/utils
        :lem-server/view)
  (:local-nicknames (:display :lem-core/display)
                    (:queue :lem/common/queue)
                    (:mouse :lem-server/mouse))
  (:export :jsonrpc
           :register-method
           :run-tcp-server
           :run-stdio-server
           :run-websocket-server
           :*exit-function*
           :main))
(in-package :lem-server)

(defvar *server-runner*)

(defvar *exit-function* (lambda () (uiop:quit 0))
  "Function called from *exit-editor-hook* after notifying clients of exit.
The default quits the process. The webview frontend replaces it because
uiop:quit cannot unwind the main thread while it is blocked in the native
webview event loop; it terminates that loop instead and the main thread
quits by itself afterwards.")

(defclass server-runner ()
  ())

;;;
(defclass websocket-server-runner (server-runner)
  ((port :initarg :port
         :reader websocket-server-runner-port)
   (host :initarg :host
         :reader websocket-server-runner-host)))

(defmethod server-listen ((runner websocket-server-runner) server)
  (let* ((null-stream (make-broadcast-stream))
         (*trace-output* null-stream)
         (*error-output* null-stream))
    (jsonrpc:server-listen server
                           :mode :websocket
                           :port (websocket-server-runner-port runner)
                           :host (websocket-server-runner-host runner)
                           :debug nil
                           :silent t
                           :clack-handler 'clack-handler)))

(defun collect-files (directory)
  (append (uiop:directory-files directory)
          (mapcan #'collect-files (uiop:subdirectories directory))))

(defun dump-files ()
  (loop :for file :in (collect-files (asdf:system-relative-pathname :lem-server #p"frontend/dist/"))
        :collect (cons (enough-namestring file (asdf:system-source-directory :lem-server))
                       (alexandria:read-file-into-byte-vector file))))

(defvar *dist* (dump-files))

(defun find-dist-by-path (path)
  (cdr (assoc path *dist* :test #'equal)))

(defun clack-handler (env)
  (unless (wsd:websocket-p env)
    (let ((path (getf env :path-info)))
      (cond ((string= "/" path)
             `(200 (:content-type "text/html")
                   ,(find-dist-by-path "frontend/dist/index.html")))
            ((alexandria:starts-with-subseq "/assets/" path)
             `(200 (:content-type ,(hunchentoot:mime-type path))
                   ,(find-dist-by-path (format nil
                                               "frontend/dist/~A"
                                               (string-left-trim "/" path)))))
            ((alexandria:starts-with-subseq "/local/" path)
             (let* ((file (uiop:parse-native-namestring (subseq path (length "/local"))))
                    (mime-type (hunchentoot:mime-type file)))
               `(200 (:content-type ,mime-type)
                     ,file)))
            (t
             '(200 () ("ok")))))))

;;;
(defclass stdio-server-runner (server-runner)
  ())

(defmethod server-listen ((runner stdio-server-runner) server)
  (jsonrpc:server-listen server
                         :mode :stdio))

;;;
(defclass local-domain-socket-server-runner (server-runner)
  ((address :initarg :address :reader local-domain-socket-server-runner-address)))

(defmethod server-listen ((runner local-domain-socket-server-runner) server)
  (jsonrpc:server-listen server
                         :mode :local-domain-socket
                         :address (local-domain-socket-server-runner-address runner)))

(defclass server (jsonrpc:server) ())

(defclass jsonrpc (lem:implementation)
  ((server :initform (make-instance 'server)
           :reader jsonrpc-server)
   (display-width :initform 80
                  :accessor jsonrpc-display-width)
   (display-height :initform 24
                   :accessor jsonrpc-display-height)
   (background-color :accessor jsonrpc-background-color)
   (foreground-color :accessor jsonrpc-foreground-color)
   (message-queue :initform (queue:make-queue)
                  :reader jsonrpc-message-queue)
   (editor-thread :initform nil
                  :accessor jsonrpc-editor-thread)
   ;; pixel size of one character cell. multiplied into every coordinate we send, so never NIL:
   ;; we guess, and the client corrects at login.
   (cell-width :initform 8
               :accessor jsonrpc-cell-width)
   (cell-height :initform 16
                :accessor jsonrpc-cell-height)
   ;; how far below a cell's top the client puts the text baseline.
   (cell-ascent :initform nil
                :accessor jsonrpc-cell-ascent)
   ;; the font's own size. the cell height is measured from the glyph bounding box, so it is larger.
   (font-em :initform nil
            :accessor jsonrpc-font-em))
  (:default-initargs
   :name :jsonrpc
   :redraw-after-modifying-floating-window t
   :window-left-margin 0
   :window-bottom-margin 0
   :html-support t
   :underline-color-support t
   :no-force-needed t
   :support-pixel-positioning t))

(defun view-id-hash (view)
  "Return a minimal hash table containing only the view ID.
Used for hot-path messages (put, clear-eol, etc.) where the JS frontend
only needs viewInfo.id. Avoids serializing the full VIEW object (14+ fields).

The hash is memoized on the view: it is called once per drawing object
per frame, but its content is constant, so we allocate it once and reuse
the same immutable instance for every subsequent message."
  (or (view-cached-id-hash view)
      (setf (view-cached-id-hash view)
            (hash "id" (view-id view)))))

(defun get-all-views ()
  (if (null (lem:current-frame))
      (vector)
      (coerce
       (loop :for frame :in (lem:all-frames)
             :append (loop :for window :in (append (lem:frame-header-windows frame)
                                                   (lem:window-list frame)
                                                   (lem:frame-floating-windows frame))
                           :collect (lem:window-view window)))
       'vector)))

(defmethod resize-display ((jsonrpc jsonrpc) width height)
  (setf (jsonrpc-display-width jsonrpc) width
        (jsonrpc-display-height jsonrpc) height))

(defmethod notify ((jsonrpc jsonrpc) method argument)
  (jsonrpc:broadcast (jsonrpc-server jsonrpc) method argument))

(defmethod notify* ((jsonrpc jsonrpc) method argument)
  (queue:enqueue (jsonrpc-message-queue jsonrpc)
                 (hash "method" method "argument" argument)))

(defmethod notify-all ((jsonrpc jsonrpc))
  (let ((argument (coerce (loop :until (queue:empty-p (jsonrpc-message-queue jsonrpc))
                                :collect (queue:dequeue (jsonrpc-message-queue jsonrpc)))
                          'vector)))
    (notify jsonrpc "bulk" argument)))

(defun update-cell-metrics (jsonrpc params)
  "take the client's font metrics out of PARAMS, if it sent any.
returns true when one of them changed, since nothing already measured survives a new cell size."
  (let ((changed))
    (flet ((update (key accessor)
             (alexandria:when-let ((value (gethash key params)))
               (when (and (realp value) (plusp value)
                          (not (eql value (funcall accessor jsonrpc))))
                 (funcall (fdefinition `(setf ,accessor)) value jsonrpc)
                 (setf changed t)))))
      (update "fontWidth" 'jsonrpc-cell-width)
      (update "fontHeight" 'jsonrpc-cell-height)
      (update "fontAscent" 'jsonrpc-cell-ascent)
      (update "fontSize" 'jsonrpc-font-em))
    changed))

(defun handle-login (jsonrpc logged-in-callback params)
  (with-error-handler ()
    (let* ((size (gethash "size" params))
           (foreground (gethash "foreground" params))
           (background (gethash "background" params)))

      (when size
        (let ((width (gethash "width" size))
              (height (gethash "height" size)))
          (resize-display jsonrpc width height)))
      (update-cell-metrics jsonrpc params)
      (when background
        (alexandria:when-let (color (lem:parse-color background))
          (setf (jsonrpc-background-color jsonrpc) color)))
      (when foreground
        (alexandria:when-let (color (lem:parse-color foreground))
          (setf (jsonrpc-foreground-color jsonrpc) color)))
      (funcall logged-in-callback)

      (let ((response (hash "views" (with-error-handler () (get-all-views))
                            "foreground" (lem-core::foreground-color)
                            "background" (lem-core::background-color)
                            "size" (hash "width" (lem:display-width)
                                         "height" (lem:display-height)))))
        response))))

(defun login (jsonrpc logged-in-callback)
  (lambda (params)
    (handle-login jsonrpc logged-in-callback params)))

(defun redraw (args)
  (with-error-handler ()
    (let ((size (and args (gethash "size" args)))
          ;; the client re-sends its font metrics here, so a font change reaches us by the same
          ;; path as a resize instead of needing one of its own.
          (metrics-changed (and args (update-cell-metrics (lem:implementation) args))))
      (when size
        (let ((width (gethash "width" size))
              (height (gethash "height" size)))
          (resize-display (lem:implementation) width height)
          (notify (lem:implementation) "resize-display" size)))
      (lem:send-event (lambda ()
                        (when metrics-changed
                          ;; the scroll position was recorded in the old cell size
                          (dolist (window (lem:window-list))
                            (setf (lem-core::horizontal-scroll-start window) 0)))
                        (lem-core::adjust-all-window-size)
                        ;; :force clears the caches, whose widths are stale after a cell-size change
                        (lem:redraw-display :force t))))))

(defvar *invoke-method-table* (make-hash-table :test 'equal))

(defun register-method (method function)
  (setf (gethash method *invoke-method-table*) function))

(defun invoke (params)
  (alexandria:when-let ((method (gethash "method" params))
                        (args (gethash "args" params)))
    (funcall (gethash method *invoke-method-table*)
             args)))

(defmethod lem-if:invoke ((jsonrpc jsonrpc) function)
  (let ((ready (bt2:make-semaphore :name "lem-server ready")))
    (setf (jsonrpc-editor-thread jsonrpc)
          (funcall function
                   (lambda ()
                     (bt2:wait-on-semaphore ready))))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "login"
                    (login jsonrpc
                           (lambda ()
                             (bt2:signal-semaphore ready))))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "input"
                    (lambda (args)
                      (input-callback jsonrpc args)))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "redraw"
                    'redraw)
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "got-clipboard-text"
                    'got-clipboard-text)
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "invoke"
                    'invoke)

    (lem:add-hook lem:*exit-editor-hook*
                  (lambda ()
                    (notify jsonrpc "exit" nil)
                    (funcall *exit-function*)))

    (server-listen *server-runner* (jsonrpc-server jsonrpc))))

(defmethod lem-if:get-background-color ((jsonrpc jsonrpc))
  (jsonrpc-background-color jsonrpc))

(defmethod lem-if:get-foreground-color ((jsonrpc jsonrpc))
  (jsonrpc-foreground-color jsonrpc))

(defmethod lem-if:update-foreground ((jsonrpc jsonrpc) color-name)
  (with-error-handler ()
    (alexandria:when-let (color (lem:parse-color color-name))
      (setf (jsonrpc-foreground-color jsonrpc) color))
    (notify jsonrpc "update-foreground" color-name)))

(defmethod lem-if:update-background ((jsonrpc jsonrpc) color-name)
  (with-error-handler ()
    (alexandria:when-let (color (lem:parse-color color-name))
      (setf (jsonrpc-background-color jsonrpc) color))
    (notify jsonrpc "update-background" color-name)))

(defmethod lem-if:update-cursor-shape ((jsonrpc jsonrpc) cursor-type)
  (with-error-handler ()
    (notify jsonrpc "update-cursor-shape"
            (hash "cursorType" (string-downcase (string cursor-type))))))

(defmethod lem-if:display-width ((jsonrpc jsonrpc))
  (with-error-handler ()
    (jsonrpc-display-width jsonrpc)))

(defmethod lem-if:display-height ((jsonrpc jsonrpc))
  (with-error-handler ()
    (jsonrpc-display-height jsonrpc)))

(defmethod lem-if:display-title ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:set-display-title ((jsonrpc jsonrpc) title)
  ;; TODO
  )

(defmethod lem-if:display-fullscreen-p ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:set-display-fullscreen-p ((jsonrpc jsonrpc) fullscreen-p)
  ;; TODO
  )

(defmethod lem-if:update-screen-size ((jsonrpc jsonrpc))
  (let* ((response (call "get-display-size" nil))
         (width (gethash "width" response))
         (height (gethash "height" response)))
    (resize-display jsonrpc width height)
    (lem:send-event :resize)))

(defmethod lem-if:make-view ((jsonrpc jsonrpc) window x y width height use-modeline)
  (let ((view (make-view :window window
                         :x x
                         :y y
                         :width width
                         :height height
                         :use-modeline use-modeline
                         :kind (cond ((or (lem:floating-window-p window)
                                          (lem:attached-window-p window))
                                      "floating")
                                     ((lem:header-window-p window)
                                      "header")
                                     (t
                                      "tile"))
                         :border (lem:window-border window)
                         :border-shape (and (lem:floating-window-p window)
                                            (lem:floating-window-border-shape window)))))
    (notify* jsonrpc "make-view" view)
    view))

(defmethod lem-if:view-width ((jsonrpc jsonrpc) view)
  (view-px-width view))

(defmethod lem-if:view-height ((jsonrpc jsonrpc) view)
  (view-px-height view))

(defmethod lem-if:delete-view ((jsonrpc jsonrpc) view)
  (with-error-handler ()
    (notify* jsonrpc "delete-view" (hash "viewInfo" (view-id-hash view)))))

(defmethod lem-if:clear ((jsonrpc jsonrpc) view)
  (with-error-handler ()
    (notify* jsonrpc "clear" (hash "viewInfo" (view-id-hash view)))))

(defmethod lem-if:set-view-size ((jsonrpc jsonrpc) view width height)
  (with-error-handler ()
    (resize-view view width height)
    (notify* jsonrpc
             "resize-view"
             (hash "viewInfo" (view-id-hash view)
                   "width" width
                   "height" height
                   "pixelWidth" (view-px-width view)
                   "pixelHeight" (view-px-height view)))))

(defmethod lem-if:set-view-pos ((jsonrpc jsonrpc) view x y)
  (with-error-handler ()
    (move-view view x y)
    (notify* jsonrpc
             "move-view"
             (hash "viewInfo" (view-id-hash view)
                   "x" x
                   "y" y
                   "pixelX" (view-px-x view)
                   "pixelY" (view-px-y view)))))

(defmethod lem-if:make-view-with-pixels ((jsonrpc jsonrpc) window x y width height
                                         pixel-x pixel-y pixel-width pixel-height
                                         use-modeline)
  (let ((view (make-view :window window
                         :x x
                         :y y
                         :width width
                         :height height
                         :pixel-x pixel-x
                         :pixel-y pixel-y
                         :pixel-width pixel-width
                         :pixel-height pixel-height
                         :use-modeline use-modeline
                         :kind (cond ((or (lem:floating-window-p window)
                                          (lem:attached-window-p window))
                                      "floating")
                                     ((lem:header-window-p window)
                                      "header")
                                     (t
                                      "tile"))
                         :border (lem:window-border window)
                         :border-shape (and (lem:floating-window-p window)
                                            (lem:floating-window-border-shape window)))))
    (notify* jsonrpc "make-view" view)
    view))

(defmethod lem-if:set-view-pos-pixels ((jsonrpc jsonrpc) view x y pixel-x pixel-y)
  (with-error-handler ()
    (move-view view x y pixel-x pixel-y)
    (notify* jsonrpc
             "move-view"
             (hash "viewInfo" (view-id-hash view)
                   "x" x
                   "y" y
                   "pixelX" (view-px-x view)
                   "pixelY" (view-px-y view)))))

(defmethod lem-if:set-view-size-pixels ((jsonrpc jsonrpc) view width height pixel-width pixel-height)
  (with-error-handler ()
    (resize-view view width height pixel-width pixel-height)
    (notify* jsonrpc
             "resize-view"
             (hash "viewInfo" (view-id-hash view)
                   "width" width
                   "height" height
                   "pixelWidth" (view-px-width view)
                   "pixelHeight" (view-px-height view)))))

(defmethod lem-if:redraw-view-before ((jsonrpc jsonrpc) view)
  )

(defmethod lem-if:redraw-view-after ((jsonrpc jsonrpc) view)
  (notify* jsonrpc
           "redraw-view-after"
           (hash "viewInfo" (view-id-hash view))))

(defmethod lem:redraw-buffer ((jsonrpc jsonrpc) (buffer lem:html-buffer) window force)
  )

(defmethod lem-if:will-update-display ((jsonrpc jsonrpc))
  (clear-attribute-hash-cache))

(defmethod lem-if:update-display ((jsonrpc jsonrpc))
  (with-error-handler ()
    (let* ((view (lem:window-view (lem:current-window)))
           (x (lem:last-print-cursor-x (lem:current-window)))
           (y (lem:last-print-cursor-y (lem:current-window)))
           (cursor-attr (lem:ensure-attribute 'lem:cursor nil))
           (impl (lem:implementation))
           (cursor-color (ensure-rgb
                          (or (when cursor-attr
                                (lem:attribute-background cursor-attr))
                              (lem-if:get-foreground-color impl))))
           (cursor-char (lem:character-at
                         (lem:buffer-point
                          (lem:window-buffer (lem:current-window)))))
           (cursor-text (if (and cursor-char (char/= cursor-char #\Newline))
                            (string cursor-char)
                            " "))
           (cursor-fg (ensure-rgb
                       (or (when cursor-attr
                             (lem:attribute-foreground cursor-attr))
                           (lem-if:get-background-color impl)))))
      (notify* jsonrpc
               "move-cursor"
               (hash "viewInfo" (view-id-hash view) "x" x "y" y
                     "color" cursor-color
                     "cursorText" cursor-text
                     "cursorForeground" cursor-fg)))
    (notify* jsonrpc "update-display" nil)
    (notify-all jsonrpc)))

(defvar *clipboard-wait-queue* (lem/common/queue:make-concurrent-queue))

(defmethod lem-if:clipboard-paste ((jsonrpc jsonrpc))
  (notify jsonrpc "get-clipboard-text" (hash))
  (lem/common/queue:dequeue *clipboard-wait-queue* :timeout 0.1))

(defun got-clipboard-text (params)
  (let ((text (gethash "text" params)))
    (lem/common/queue:enqueue *clipboard-wait-queue* text)))

(defmethod lem-if:clipboard-copy ((jsonrpc jsonrpc) text)
  (notify jsonrpc "set-clipboard-text" (hash "text" text)))

(defmethod lem-if:increase-font-size ((jsonrpc jsonrpc))
  (let ((size (nth-value 1 (lem-if:get-font jsonrpc))))
    (lem:set-font-size (1+ size))))

(defmethod lem-if:decrease-font-size ((jsonrpc jsonrpc))
  (let ((size (nth-value 1 (lem-if:get-font jsonrpc))))
    (when (< 0 size)
      (lem:set-font-size (1- size)))))

(defmethod lem-if:resize-display-before ((jsonrpc jsonrpc))
  )

(defmethod lem-if:get-font-list ((jsonrpc jsonrpc))
  )

(defmethod lem-if:get-mouse-position ((jsonrpc jsonrpc))
  (mouse:get-position))

(defmethod lem-if:cell-width ((jsonrpc jsonrpc))
  (jsonrpc-cell-width jsonrpc))

(defmethod lem-if:cell-height ((jsonrpc jsonrpc))
  (jsonrpc-cell-height jsonrpc))

(defmethod lem-if:cell-pixel-size ((jsonrpc jsonrpc))
  (values (jsonrpc-cell-width jsonrpc)
          (jsonrpc-cell-height jsonrpc)
          (jsonrpc-cell-ascent jsonrpc)))

(defmethod lem-if:font-em-pixels ((jsonrpc jsonrpc))
  (jsonrpc-font-em jsonrpc))

(defun call (method params)
  (let ((mailbox (sb-concurrency:make-mailbox :name "lem-server-call-async")))
    (loop :for connection
          :in (jsonrpc/server::server-client-connections
               (jsonrpc-server (lem:implementation)))
          :do (jsonrpc:call-async-to
               (jsonrpc-server (lem:implementation))
               connection
               method
               params
               (lambda (res)
                 (sb-concurrency:send-message mailbox (list t res)))
               (lambda (message code)
                 (sb-concurrency:send-message
                  mailbox
                  (list nil
                        (make-condition 'jsonrpc/errors:jsonrpc-callback-error
                                        :message message
                                        :code code))))))
    (destructuring-bind (ok value)
        (sb-concurrency:receive-message mailbox)
      (if ok
          value
          (error value)))))

(defmethod lem-if:js-eval ((jsonrpc jsonrpc) view code &key wait)
  (let ((params (hash "viewInfo" (view-id-hash view) "code" code)))
    (if wait
        (call "js-eval" params)
        (notify (lem:implementation) "js-eval" params))))

(defmethod lem-if:set-font-name ((jsonrpc jsonrpc) font-name)
  (notify (lem:implementation) "set-font" (hash "fontName" font-name)))

(defmethod lem-if:set-font-size ((jsonrpc jsonrpc) font-size)
  (notify (lem:implementation) "set-font" (hash "fontSize" font-size)))

(defmethod lem-if:get-font ((jsonrpc jsonrpc))
  (let ((response (call "get-font" nil)))
    (values (gethash "name" response)
            (gethash "size" response))))

(defun load-css (css-content)
  (notify (lem:implementation) "load-css" (hash "content" css-content)))

(lem:add-hook lem:*switch-to-buffer-hook* 'on-switch-to-buffer)

(defun on-switch-to-buffer (buffer)
  (cond ((and (typep buffer 'lem:html-buffer)
              (lem:html-buffer-updated-p buffer))
         (lem:invalidate-html-buffer-updated buffer)
         (change-view-to-html (lem:current-window) (lem:html-buffer-html buffer)))
        ((and (typep (lem:current-buffer) 'lem:html-buffer)
              (not (typep buffer 'lem:html-buffer)))
         (change-view-to-editor (lem:current-window)))
        ((and (not (typep (lem:current-buffer) 'lem:html-buffer))
              (typep buffer 'lem:html-buffer))
         (change-view-to-html (lem:current-window) (lem:html-buffer-html buffer)))))

(defun change-view-to-editor (window)
  (notify* (lem:implementation)
           "change-view"
           (hash "viewInfo" (view-id-hash (lem:window-view window))
                 "type" "editor")))

(defun change-view-to-html (window content)
  (notify* (lem:implementation)
           "change-view"
           (hash "viewInfo" (view-id-hash (lem:window-view window))
                 "type" "html"
                 "content" content)))

;;;;
(defun bool (x) (if x 'yason:true 'yason:false))

(defun ensure-rgb (color)
  (typecase color
    (lem:color
     (lem:color-to-hex-string color))
    (string
     (lem:color-to-hex-string (lem:parse-color color)))
    (otherwise
     color)))

(defmethod yason:encode ((attribute lem:attribute) &optional (stream *standard-output*))
  (with-error-handler ()
    (yason:with-output (stream)
      (yason:with-object ()
        (yason:encode-object-element "foreground" (ensure-rgb (lem:attribute-foreground attribute)))
        (yason:encode-object-element "background" (ensure-rgb (lem:attribute-background attribute)))
        (yason:encode-object-element "reverse" (bool (lem:attribute-reverse attribute)))
        (yason:encode-object-element "bold" (bool (lem:attribute-bold attribute)))
        (yason:encode-object-element "underline" (lem:attribute-underline attribute))
        (yason:encode-object-element "cursor" (bool (lem-core:cursor-attribute-p attribute)))))))



;;; drawing

(defgeneric draw-object (jsonrpc object x y view row)
  (:documentation "draw OBJECT into VIEW with its top-left corner at pixel position X, Y.
`lem-core/display:layout-row' already positioned it, so ROW is only for what an object shares with
the rest of its row: the full height a background fills and where the row's text sits, which is
where the caret goes."))

(defmethod draw-object (jsonrpc (object display:void-object) x y view row)
  (values))

(defvar *put-target* :edit-area)

;;; Attribute JSON cache: avoids re-serializing the same attribute objects
;;; through YASON's CLOS dispatch (ensure-rgb, encode-object-element, etc.)
;;; every frame.  The table is held in a lexical LET around its two
;;; accessors so the state isn't a global defvar; it is eq-keyed because
;;; attribute objects have identity, and cleared once per display update.
(let ((attribute-hash-cache (make-hash-table :test 'eq)))
  (defun clear-attribute-hash-cache ()
    "Drop all memoized attribute->hash entries.  Called once at the start
of each display update so the cache only reflects the current frame's
live attributes."
    (clrhash attribute-hash-cache))

  (defun attribute-to-hash (attribute)
    "Return a cached hash-table representation of ATTRIBUTE for fast JSON
encoding.  Uses EQ identity caching — same attribute object returns the
same hash."
    (when attribute
      (or (gethash attribute attribute-hash-cache)
          (setf (gethash attribute attribute-hash-cache)
                (hash "foreground" (ensure-rgb (lem:attribute-foreground attribute))
                      "background" (ensure-rgb (lem:attribute-background attribute))
                      "reverse" (bool (lem:attribute-reverse attribute))
                      "bold" (bool (lem:attribute-bold attribute))
                      "underline" (lem:attribute-underline attribute)
                      "cursor" (bool (lem-core:cursor-attribute-p attribute))))))))

(defun ensure-attribute (attribute)
  (let ((attribute (lem:ensure-attribute attribute nil)))
    (when (and lem-if:*background-color-of-drawing-window*
               (null attribute))
      (setf attribute (lem:make-attribute :background lem-if:*background-color-of-drawing-window*)))
    (attribute-to-hash attribute)))

(defun taller-than-text-p (jsonrpc row)
  "whether ROW is taller than a line of text, which an image on it can make it."
  (and row (> (display:row-height row) (jsonrpc-cell-height jsonrpc))))

(defun put (jsonrpc view x y string attribute &key font text-width row)
  "draw STRING at pixel position X, Y in VIEW, over a background TEXT-WIDTH wide and as tall as ROW."
  (with-error-handler ()
    (let ((tall (taller-than-text-p jsonrpc row)))
      (notify* jsonrpc
               (ecase *put-target*
                 (:edit-area "put")
                 (:modeline "modeline-put"))
               (hash "viewInfo" (view-id-hash view)
                     "x" x
                     "y" y
                     "text" string
                     "textWidth" (or text-width
                                     (* (lem:string-width string) (jsonrpc-cell-width jsonrpc)))
                     "backgroundY" (and tall (display:row-top row))
                     "backgroundHeight" (and tall (display:row-height row))
                     "attribute" (ensure-attribute attribute)
                     "font" font)))))

(defun draw-block (jsonrpc view x y width height color)
  "fill the WIDTH by HEIGHT rectangle at pixel position X, Y in VIEW with COLOR.
unlike `put', which is one line of text tall, this covers a row an image made taller. a NIL COLOR
leaves the client to use its default background."
  (with-error-handler ()
    (notify* jsonrpc
             (ecase *put-target*
               (:edit-area "draw-block")
               (:modeline "modeline-draw-block"))
             (hash "viewInfo" (view-id-hash view)
                   "x" x
                   "y" y
                   "width" width
                   "height" height
                   "color" (and color (lem:color-to-hex-string color))))))

(defmethod draw-object (jsonrpc (object display:text-object) x y view row)
  (let* ((string (display:text-object-string object))
         (attribute (display:text-object-attribute object))
         (type (display:text-object-type object))
         (width (lem-if:object-width jsonrpc object)))
    (when (and attribute (lem-core:cursor-attribute-p attribute))
      (lem-core:set-last-print-cursor (view-window view) x y))
    (put jsonrpc
         view
         x
         y
         string
         attribute
         :text-width width
         :row row)))

(defmethod draw-object (jsonrpc (object display:icon-object) x y view row)
  (let* ((string (display:text-object-string object))
         (attribute (display:text-object-attribute object))
         (type (display:text-object-type object))
         (width (lem-if:object-width jsonrpc object)))
    (when (and attribute (lem-core:cursor-attribute-p attribute))
      (lem-core:set-last-print-cursor (view-window view) x y))
    (put jsonrpc
         view
         x
         y
         string
         attribute
         :text-width width
         :row row
         :font (lem:icon-value (char-code (char string 0))
                               :font))))

(defmethod draw-object (jsonrpc (object display:eol-cursor-object) x y view row)
  (lem-core:set-last-print-cursor (view-window view) x y)
  (let ((attr (lem:make-attribute
               :background
               (lem:color-to-hex-string (display:eol-cursor-object-color object)))))
    (lem-core:set-cursor-attribute attr)
    (put jsonrpc view x y " " attr :text-width (jsonrpc-cell-width jsonrpc))))

(defmethod draw-object (jsonrpc (object display:line-end-object) x y view row)
  (let ((string (display:text-object-string object))
        (attribute (display:text-object-attribute object))
        (width (lem-if:object-width jsonrpc object)))
    (put jsonrpc
         view
         ;; the offset is a column count, unlike the x it is added to.
         (+ x (* (display:line-end-object-offset object) (jsonrpc-cell-width jsonrpc)))
         y
         string
         attribute
         :text-width width
         :row row)))

(defun image-object-url (object)
  "return a URL the JS client can load for OBJECT's image, or NIL.
a pathname or plain-string path is served through the existing /local static route.
a string already carrying a data:/https: URL is passed through unchanged."
  (let ((image (display:image-object-image object)))
    (typecase image
      (pathname (format nil "/local~A" (namestring image)))
      (string (if (or (alexandria:starts-with-subseq "data:" image)
                      (alexandria:starts-with-subseq "http:" image)
                      (alexandria:starts-with-subseq "https:" image))
                  image
                  (format nil "/local~A" image)))
      (t nil))))

(defun attribute-own-background (attribute)
  "the background ATTRIBUTE asks for as a color, or NIL when it asks for none.
not `lem:attribute-background-with-reverse', which answers with the default background rather than NIL."
  (alexandria:when-let ((background (if (lem:attribute-reverse attribute)
                                        (lem:attribute-foreground attribute)
                                        (lem:attribute-background attribute))))
    (typecase background
      (lem:color background)
      (string (lem:parse-color background)))))

(defun row-text-top (jsonrpc row)
  "the top of a line of text on ROW: its baseline less the font's ascent."
  (- (display:row-baseline row)
     (or (jsonrpc-cell-ascent jsonrpc) (jsonrpc-cell-height jsonrpc))))

(defmethod draw-object (jsonrpc (object display:image-object) x y view row)
  (alexandria:when-let ((attribute (lem:ensure-attribute (display:image-object-attribute object)
                                                         nil)))
    ;; the image carries the attribute of the text it replaced, so selecting the line reaches it too
    (alexandria:when-let ((color (attribute-own-background attribute)))
      (draw-block jsonrpc view x (display:row-top row) (lem-if:object-width jsonrpc object)
                  (display:row-height row) color))
    ;; the cursor can sit on an image. Y is the image's top, which can be far above the row's text,
    ;; so report the text's top instead and the caret aligns with the text.
    (when (lem-core:cursor-attribute-p attribute)
      (lem-core:set-last-print-cursor (view-window view) x (row-text-top jsonrpc row))))
  (let ((url (image-object-url object)))
    (when url
      (with-error-handler ()
        (let* ((pw (display:image-draw-width jsonrpc object))
               (ph (display:image-draw-height jsonrpc object))
               ;; how much may appear: the crop the layout applied, and the room left in the view.
               ;; an image is a DOM element over the view, not pixels in it, so nothing clips it
               ;; for us.
               (clip-width (min pw
                                (max 0 (- (view-px-width view) x))
                                (or (display:image-object-visible-width object) pw)))
               (clip-height (min ph (max 0 (- (view-px-height view) y)))))
          (notify* jsonrpc
                   "put-image"
                   (hash "viewInfo" (view-id-hash view)
                         "x" x
                         "y" y
                         "pixelWidth" pw
                         "pixelHeight" ph
                         ;; the visible part, from the image's top-left
                         "clipWidth" clip-width
                         "clipHeight" clip-height
                         "url" url)))))))

(defun draw-row (jsonrpc view row)
  "draw ROW's background fill, then everything placed on it.
the fill covers the row's full height, which a tall object (e.g. an image) can push past a single
text line's, so it goes as a `draw-block' rather than a put's background."
  (let ((width (view-px-width view)))
    (when (and (display:row-fill-color row)
               (< (display:row-fill-x row) width))
      (draw-block jsonrpc
                  view
                  (display:row-fill-x row)
                  (display:row-top row)
                  (- width (display:row-fill-x row))
                  (display:row-height row)
                  (display:row-fill-color row))))
  (loop :for placement :in (display:row-placements row)
        :do (draw-object jsonrpc
                         (display:placement-object placement)
                         (display:placement-x placement)
                         (display:placement-top placement)
                         view
                         row)))

(defmethod lem-if:render-row ((jsonrpc jsonrpc) view row)
  (with-error-handler ()
    (notify* jsonrpc
             "clear-eol"
             (hash "viewInfo" (view-id-hash view)
                   "x" 0
                   "y" (display:row-top row)
                   "height" (display:row-height row)))
    (draw-row jsonrpc view row)))

(defmethod lem-if:render-modeline-row ((jsonrpc jsonrpc) view row default-attribute)
  ;; the modeline has a surface of its own here, so the row is drawn where it was laid out.
  (let ((*put-target* :modeline))
    ;; the modeline's own background, under everything the row places on it
    (draw-block jsonrpc
                view
                0
                (display:row-top row)
                (view-px-width view)
                (display:row-height row)
                (lem:attribute-background-with-reverse default-attribute))
    (draw-row jsonrpc view row)))

(defmethod lem-if:clear-to-end-of-window ((jsonrpc jsonrpc) view y)
  (notify* jsonrpc
           "clear-eob"
           (hash "viewInfo" (view-id-hash view)
                 "x" 0
                 "y" y)))


;;;
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
                  :shift (if (lem:insertion-key-sym-p key)
                             nil
                             shift)
                  :sym (if (and (lem:insertion-key-sym-p key)
                                shift
                                meta)
                           (string-upcase key)
                           key))))

(defun convert-button (button)
  (case button
    (0 :button-1)
    (2 :button-3)
    (1 :button-2)))

(defun input-callback (jsonrpc args)
  (handler-case
      (let ((kind (gethash "kind" args))
            (value (gethash "value" args)))
        (alexandria:switch (kind :test #'equal)
          ("abort"
           (lem:send-abort-event (jsonrpc-editor-thread jsonrpc) nil))
          ("key"
           (when value
             (let ((key (convert-keyevent value)))
               (lem:send-event key))))
          ("clipboard-paste"
           (lem:send-event
            (lambda ()
              (let ((text (lem:get-clipboard-data))
                    (mode (lem:ensure-mode-object
                           (lem:current-major-mode-at-point
                            (lem:current-point)))))
                (when text
                  (lem:paste-using-mode mode text))))))
          ("mousedown"
           (let ((x (gethash "x" value))
                 (y (gethash "y" value))
                 (pixel-x (gethash "pixelX" value))
                 (pixel-y (gethash "pixelY" value))
                 (button (convert-button (gethash "button" value)))
                 (clicks (gethash "clicks" value)))
             (lem:send-event
              (lambda ()
                (lem:receive-mouse-button-down x
                                               y
                                               pixel-x
                                               pixel-y
                                               button
                                               clicks)))))
          ("mouseup"
           (let ((x (gethash "x" value))
                 (y (gethash "y" value))
                 (pixel-x (gethash "pixelX" value))
                 (pixel-y (gethash "pixelY" value))
                 (button (convert-button (gethash "button" value))))
             (when button
               (lem:send-event
                (lambda ()
                  (lem:receive-mouse-button-up x
                                               y
                                               pixel-x
                                               pixel-y
                                               button))))))
          ("mousemove"
           (let ((x (gethash "x" value))
                 (y (gethash "y" value))
                 (pixel-x (gethash "pixelX" value))
                 (pixel-y (gethash "pixelY" value))
                 (button (convert-button (gethash "button" value))))
             (lem:send-event
              (lambda ()
                (mouse:update-position x y)
                (lem:receive-mouse-motion x
                                          y
                                          pixel-x
                                          pixel-y
                                          button)))))
          ("wheel"
           (let ((x (gethash "x" value))
                 (y (gethash "y" value))
                 (pixel-x (gethash "pixelX" value))
                 (pixel-y (gethash "pixelY" value))
                 (wheel-x (gethash "wheelX" value))
                 (wheel-y (gethash "wheelY" value)))
             (lem:send-event
              (lambda ()
                (lem:receive-mouse-wheel x y pixel-x pixel-y wheel-x wheel-y)
                (when (= 0 (lem:event-queue-length))
                  (lem:redraw-display))))))
          ("resize"
           (resize-display jsonrpc
                           (gethash "width" value)
                           (gethash "height" value))
           (lem:send-event :resize))
          ("input-string"
           (loop :for c :across value
                 :for key := (convert-keyevent
                              (alexandria:plist-hash-table (list "key" (string c))
                                                           :test 'equal))
                 :do (lem:send-event key)))
          (otherwise (error "unexpected input kind: ~D" kind))))
    (error (e)
      (log:info "input-callback: ~A ~A" e
              (with-output-to-string (stream)
                (let ((stream (yason:make-json-output-stream stream)))
                  (yason:encode args stream)))))))

(defun init ()
  ;; TODO: Fix this problem: frame-multiplexer cannot be used with lem-server, disable them for now.
  (lem:remove-hook lem:*after-init-hook* 'lem/frame-multiplexer::enable-frame-multiplexer))

(defun run-websocket-server (&key (port 50000) (hostname "127.0.0.1") args)
  (let ((*server-runner*
          (make-instance 'websocket-server-runner
                         :port port
                         :host hostname)))
    (init)
    (apply #'lem:lem (append args (list "--interface" "JSONRPC")))))

(defun run-stdio-server ()
  (let ((*server-runner* (make-instance 'stdio-server-runner)))
    (init)
    (lem:lem "--interface" "JSONRPC")))

(defun run-local-domain-socket-server (&key address)
  (let ((*server-runner* (make-instance 'local-domain-socket-server-runner
                                        :address address)))
    (init)
    (lem:lem "--interface" "JSONRPC")))

(defun check-port-specified (port)
  (unless port
    (command-line-arguments:show-option-help +command-line-spec+)
    (uiop:quit 1)))

(defparameter +command-line-spec+
  '(("mode" :type string :optional t :documentation "\"websocket\", \"stdio\", \"local-domain-socket\"")
    ("port" :type integer :optional nil :documentation "port of \"websocket\"")
    ("host" :type string :optional t)
    ("address" :type string :optional t :documentation "address of \"local-domain-socket\"")))

(defun main (&optional (args (uiop:command-line-arguments)))
  (command-line-arguments:handle-command-line
   +command-line-spec+
   (lambda (&key (mode "websocket")
                 port
                 (host "127.0.0.1")
                 address)
     (vom:config t :info)
     (cond ((string= mode "websocket")
            (check-port-specified port)
            (format t
                    "~%~4Topen http://localhost:~D/ in your browser~2%"
                    port)
            (run-websocket-server :port port
                                  :hostname host))
           ((string= mode "stdio")
            (run-stdio-server))
           ((string= mode "local-domain-socket")
            (run-local-domain-socket-server :address address))
           (t
            (command-line-arguments:show-option-help +command-line-spec+)
            (uiop:quit 1))))
   :name "lem-server"
   :positional-arity 0
   :command-line args))

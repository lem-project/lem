(defpackage :lem/co-editing/client
  (:use :cl :lem/co-editing/utils))
(in-package :lem/co-editing/client)

(defvar *inhibit-handle* nil)
(defvar *client*)

(defclass client (jsonrpc:client)
  ((id :initform (random-uuid)
       :reader client-id)))

(defun create-client (port)
  (let ((client (make-instance 'client)))
    (jsonrpc:client-connect client
                            :port port
                            :mode :websocket)
    client))

(defun start-sync (port)
  (setf *client* (create-client port))

  (jsonrpc:expose *client* "insert-string" 'api/insert-string)
  (jsonrpc:expose *client* "delete-string" 'api/delete-string)
  (jsonrpc:expose *client* "update-text" 'api/update-text)
  (jsonrpc:expose *client* "move-cursor" 'api/move-cursor)

  (lem:add-hook lem:*find-file-hook* 'find-file-hook)
  (lem:add-hook lem:*post-command-hook* 'move-cursor)
  (lem:add-hook (lem:variable-value 'lem:before-change-functions :global t) 'sync-buffer)

  (set-sync-buffer (lem:current-buffer)))

(defun sync-buffer-p (buffer)
  (lem:buffer-value buffer 'sync-buffer))

(defun set-sync-buffer (buffer)
  (setf (lem:buffer-value buffer 'sync-buffer) t))

(defun find-file-hook (buffer)
  (unless (lem:buffer-temporary-p buffer)
    (set-sync-buffer buffer)
    (let ((text (did-open (lem:buffer-name buffer)
                          (lem:buffer-text buffer))))
      (when (stringp text)
        (replace-text buffer text)))))

(defun sync-buffer (point arg)
  (when (sync-buffer-p (lem:point-buffer point))
    (unless *inhibit-handle*
      (let ((buffer (lem:point-buffer point)))
        (cond ((characterp arg)
               (send-write-string (lem:buffer-name buffer)
                                  (lem:line-number-at-point point)
                                  (lem:point-charpos point)
                                  (string arg)))
              ((stringp arg)
               (send-write-string (lem:buffer-name buffer)
                                  (lem:line-number-at-point point)
                                  (lem:point-charpos point)
                                  arg))
              ((integerp arg)
               (send-delete-string (lem:buffer-name buffer)
                                   (lem:line-number-at-point point)
                                   (lem:point-charpos point)
                                   arg)))))))

(defun move-cursor ()
  (let* ((buffer (lem:current-buffer))
         (point (lem:buffer-point buffer)))
    (send-cursor (lem:buffer-name buffer)
                 (lem:line-number-at-point point)
                 (lem:point-charpos point))))

;;;
(defun buffer-user-cursors (buffer)
  (lem:buffer-user-cursors buffer))

(defun get-user-cursor (buffer client-id)
  (gethash client-id (buffer-user-cursors buffer)))

(defun set-user-cursor (buffer client-id client-color line character)
  (let ((cursor (get-user-cursor buffer client-id)))
    (unless cursor
      ;; TODO: delete-point
      (setf cursor (lem-core::make-other-user-cursor
                    (lem:copy-point (lem:buffer-point buffer) :temporary)
                    client-id
                    client-color))
      (setf (gethash client-id (buffer-user-cursors buffer)) cursor))
    (move-point cursor line character)
    cursor))

(defun move-other-user-cursor (client-id client-color buffer-name line character)
  (alexandria:when-let (buffer (lem:get-buffer buffer-name))
    (set-user-cursor buffer client-id client-color line character)))

(defun api/insert-string (params)
  (log:info "insert-string: ~A" (pretty-json params))
  (with-hash-bindings (client
                       buffer-name
                       line
                       character
                       string)
      params
    (let ((client-id (gethash "id" client))
          (client-color (gethash "color" client)))
      (unless (equal client-id (client-id *client*))
        (lem:send-event
         (lambda ()
           (let ((*inhibit-handle* t))
             (move-other-user-cursor client-id client-color buffer-name line character)
             (insert-string buffer-name line character string))
           (lem:redraw-display)))))))

(defun api/delete-string (params)
  (log:info "delete-string: ~A" (pretty-json params))
  (with-hash-bindings (client
                       buffer-name
                       line
                       character
                       n)
      params
    (let ((client-id (gethash "id" client))
          (client-color (gethash "color" client)))
      (unless (equal client-id (client-id *client*))
        (lem:send-event
         (lambda ()
           (let ((*inhibit-handle* t))
             (move-other-user-cursor client-id client-color buffer-name line character)
             (delete-string buffer-name line character n))
           (lem:redraw-display)))))))

(defun api/update-text (params)
  (log:info "update-text: ~A" (pretty-json params))
  (with-hash-bindings (client
                       buffer-name
                       text)
      params
    (declare (ignore client))
    (lem:send-event
     (lambda ()
       (when (lem:get-buffer buffer-name)
         (let ((*inhibit-handle* t))
           (replace-text buffer-name text)
           (lem:redraw-display)))))))

(defun api/move-cursor (params)
  (log:info "move-cursor: ~A" (pretty-json params))
  (with-hash-bindings (client
                       buffer-name
                       line
                       character)
      params
    (let ((client-id (gethash "id" client))
          (client-color (gethash "color" client)))
      (unless (equal client-id (client-id *client*))
        (lem:send-event
         (lambda ()
           (let ((*inhibit-handle* t))
             (move-other-user-cursor client-id client-color buffer-name line character))
           (lem:redraw-display)))))))
;;;
(defun did-open (buffer-name text)
  (jsonrpc:call *client*
                "co/did-open"
                (hash "client-id" (client-id *client*)
                      "buffer-name" buffer-name
                      "text" text)))

(defun send-write-string (buffer-name line character string)
  (jsonrpc:notify *client*
                  "co/insert-string"
                  (hash "client-id" (client-id *client*)
                        "buffer-name" buffer-name
                        "line" line
                        "character" character
                        "string" string)))

(defun send-delete-string (buffer-name line character n)
  (jsonrpc:notify *client*
                  "co/delete-string"
                  (hash "client-id" (client-id *client*)
                        "buffer-name" buffer-name
                        "line" line
                        "character" character
                        "n" n)))

(defun send-cursor (buffer-name line character)
  (jsonrpc:notify *client*
                  "co/move-cursor"
                  (hash "client-id" (client-id *client*)
                        "buffer-name" buffer-name
                        "line" line
                        "character" character)))

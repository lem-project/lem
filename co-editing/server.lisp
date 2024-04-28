(defpackage :lem/co-editing/server
  (:use :cl :lem/co-editing/utils)
  (:export :start
           :expose))
(in-package :lem/co-editing/server)

;; TODO:
;; - clientの数が色の数を越えたとき
;; - clientが切断したとき

(defparameter *cursor-colors*
  '("royalblue"
    "sandybrown"
    "seagreen3"
    "violetred2"
    "aliceblue"
    "chocolate1"
    "chartreuse"
    "firebrick"
    "mediumorchid1"))

(defvar *server* (jsonrpc:make-server))
(defvar *clients* (make-hash-table :test 'equal))

(defstruct client
  id
  color)

(defun start (&key (port 50879) (host "localhost"))
  (register-methods)
  (jsonrpc:server-listen *server*
                         :port port
                         :host host
                         :mode :websocket))

(defun expose (method-name function)
  (jsonrpc:expose *server* method-name function))

(defun register-methods ()
  (jsonrpc:expose *server* "co/insert-string" 'api/insert-string)
  (jsonrpc:expose *server* "co/delete-string" 'api/delete-string)
  (jsonrpc:expose *server* "co/move-cursor" 'api/move-cursor)
  (jsonrpc:expose *server* "co/did-open" 'api/did-open))

(defun register-client (client-id)
  (or (gethash client-id *clients*)
      (setf (gethash client-id *clients*)
            (make-client :id client-id
                         :color (elt *cursor-colors*
                                     (mod (hash-table-count *clients*)
                                          (length *cursor-colors*)))))))

(defun update-text (client buffer)
  (jsonrpc:broadcast *server*
                     "update-text"
                     (hash "client" (hash "id" (client-id client)
                                          "color" (client-color client))
                           "buffer-name" (lem:buffer-name buffer)
                           "text" (lem:buffer-text buffer))))

(defun api/did-open (params)
  (with-hash-bindings (client-id
                       buffer-name
                       text)
      params
    (declare (ignore client-id))
    (let ((buffer (lem:get-buffer buffer-name)))
      (cond (buffer
             (lem:buffer-text buffer))
            (t
             (replace-text buffer-name text)
             nil)))))

(defun api/insert-string (params)
  (with-hash-bindings (client-id
                       buffer-name
                       line
                       character
                       string)
      params
    (let ((client (register-client client-id))
          (buffer (insert-string buffer-name line character string)))
      (update-text client buffer))))

(defun api/delete-string (params)
  (with-hash-bindings (client-id
                       buffer-name
                       line
                       character
                       n)
      params
    (let ((client (register-client client-id))
          (buffer (delete-string buffer-name line character n)))
      (update-text client buffer))))

(defun api/move-cursor (params)
  (with-hash-bindings (client-id
                       buffer-name
                       line
                       character)
      params
    (let ((client (register-client client-id)))
      (jsonrpc:broadcast *server*
                         "move-cursor"
                         (hash "client" (hash "id" client-id
                                              "color" (client-color client))
                               "buffer-name" buffer-name
                               "line" line
                               "character" character)))))

(in-package :lem-lisp-mode)

(define-key *lisp-mode-keymap* "C-c C-b" 'lisp-connection-list)

(defclass connection-item (lem/multi-column-list:multi-column-list-item)
  ((connection :initarg :connection
               :reader connection-item-connection)))

(defclass connection-menu (lem/multi-column-list:multi-column-list) ()
  (:default-initargs :columns '("" "hostname" "port" "pid" "name" "version" "command")))

(defmethod lem/multi-column-list:map-columns ((component connection-menu) (item connection-item))
  (let ((connection (connection-item-connection item)))
    (list (if (eq connection *connection*) "*" "")
          (connection-hostname connection)
          (connection-port connection)
          (or (self-connection-p connection) (connection-pid connection))
          (connection-implementation-name connection)
          (connection-implementation-version connection)
          (connection-command connection))))

(defmethod lem/multi-column-list:select-item ((component connection-menu) item)
  (switch-connection (connection-item-connection item))
  (lem/multi-column-list:update component)
  (lem/multi-column-list:quit component))

(define-command lisp-connection-list () ()
  (lem/multi-column-list:display
   (make-instance 'connection-menu
                  :items (mapcar (lambda (c)
                                   (make-instance 'connection-item :connection c))
                                 (connection-list)))))

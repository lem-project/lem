(in-package :lem)

(defparameter *default-popup-message-timeout* 5)

(defun display-popup-message (buffer-or-string
                              &key (timeout *default-popup-message-timeout*)
                                   destination-window
                                   style)
  (lem-if:display-popup-message (implementation)
                                buffer-or-string
                                :timeout timeout
                                :destination-window destination-window
                                :style style))

(defun delete-popup-message (popup-message)
  (lem-if:delete-popup-message (implementation) popup-message))

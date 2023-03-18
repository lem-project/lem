(in-package :lem)

(defparameter *default-popup-message-timeout* 5)

(defun display-popup-message (buffer-or-string
                              &key (timeout *default-popup-message-timeout*)
                                   destination-window
                                   source-window
                                   style)
  (lem-if:display-popup-message (implementation)
                                buffer-or-string
                                :timeout timeout
                                :destination-window destination-window
                                :source-window source-window
                                :style style))

(defun delete-popup-message (popup-message)
  (lem-if:delete-popup-message (implementation) popup-message))

(defun display-popup-menu (items
                           &rest args
                           &key action-callback
                                print-spec)
  (declare (ignore action-callback print-spec))
  (apply #'lem-if:display-popup-menu (implementation)
         items
         args))

(defun popup-menu-update (items &key print-spec)
  (lem-if:popup-menu-update (implementation) items :print-spec print-spec))

(defun popup-menu-quit ()
  (lem-if:popup-menu-quit (implementation)))

(defun popup-menu-down ()
  (lem-if:popup-menu-down (implementation)))

(defun popup-menu-up ()
  (lem-if:popup-menu-up (implementation)))

(defun popup-menu-first ()
  (lem-if:popup-menu-first (implementation)))

(defun popup-menu-last ()
  (lem-if:popup-menu-last (implementation)))

(defun popup-menu-select ()
  (lem-if:popup-menu-select (implementation)))

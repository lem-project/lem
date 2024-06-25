(defpackage :lem-terminal/terminal
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi))
  (:export :create
           :destroy
           :clear
           :render
           :update
           :input-character
           :input-key))
(in-package :lem-terminal/terminal)

(defclass terminal ()
  ((viscus :initarg :viscus
           :reader terminal-viscus)
   (buffer :initarg :buffer
           :reader terminal-buffer)
   (rows :initarg :rows
         :accessor terminal-rows)
   (cols :initarg :cols
         :accessor terminal-cols)))

(defun create (&key (rows (alexandria:required-argument :rows))
                    (cols (alexandria:required-argument :cols))
                    (buffer (alexandria:required-argument :buffer)))
  (make-instance 'terminal
                 :viscus (ffi::terminal-new rows cols)
                 :buffer buffer
                 :rows rows
                 :cols cols))

(defmethod destroy ((terminal terminal))
  (ffi::terminal-delete (terminal-viscus terminal)))

(defun get-foreground-color (viscus)
  (let ((r (ffi::terminal-last-cell-fg-red viscus))
        (g (ffi::terminal-last-cell-fg-green viscus))
        (b (ffi::terminal-last-cell-fg-blue viscus)))
    (make-color r g b)))

(defun get-background-color (viscus)
  (let ((r (ffi::terminal-last-cell-bg-red viscus))
        (g (ffi::terminal-last-cell-bg-green viscus))
        (b (ffi::terminal-last-cell-bg-blue viscus)))
    (make-color r g b)))

(defun get-cell-attribute (viscus)
  (let ((foreground (get-foreground-color viscus))
        (background (get-background-color viscus)))
    (make-attribute :foreground foreground
                    :background background)))

(defun get-cell-character (viscus)
  (let* ((chars (ffi::terminal-last-cell-chars viscus))
         (char (code-char (cffi:mem-ref chars :int 0))))
    char))

(defstruct cell
  character
  attribute)

(defmethod retrieve-display ((terminal terminal))
  (let* ((viscus (terminal-viscus terminal))
         (rows (terminal-rows terminal))
         (cols (terminal-cols terminal))
         (display (make-array (list rows cols))))
    (loop :for row :from 0 :below rows
          :do (loop :for col :from 0 :below cols
                    :do (ffi::terminal-query-cell viscus col row)
                        (let ((char (get-cell-character viscus))
                              (attribute (get-cell-attribute viscus)))
                          (setf (aref display row col)
                                (if (char= char #\Nul)
                                    (make-cell :character #\space :attribute attribute)
                                    (make-cell :character char :attribute attribute))))))
    display))

(defun %render (buffer display rows cols cursor-row cursor-col)
  (let ((point (buffer-point buffer)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (loop :for row :from 0 :below rows
            :do (loop :for col :from 0 :below cols
                      :do (let ((cell (aref display row col)))
                            (insert-string point
                                           (string (cell-character cell))
                                           :attribute (cell-attribute cell))))
                (insert-character point #\newline))
      (move-to-line point (1+ cursor-row))
      (line-offset point 0 cursor-col))))

(defmethod render ((terminal terminal))
  (%render (terminal-buffer terminal)
           (retrieve-display terminal)
           (terminal-rows terminal)
           (terminal-cols terminal)
           (ffi::terminal-cursor-row (terminal-viscus terminal))
           (ffi::terminal-cursor-col (terminal-viscus terminal))))

(defmethod update ((terminal terminal) &key with-block)
  (if with-block
      (ffi::terminal-process-input (terminal-viscus terminal))
      (ffi::terminal-process-input-nonblock (terminal-viscus terminal))))

(defmethod input-character ((terminal terminal) character)
  (ffi::terminal-input-char (terminal-viscus terminal)
                            (char-code character)
                            0))

(defmethod input-key ((terminal terminal) key)
  (ffi::terminal-input-key (terminal-viscus terminal)
                           key
                           0))

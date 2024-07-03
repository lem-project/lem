(defpackage :lem-terminal/terminal
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi))
  (:export :terminals
           :create
           :destroy
           :clear
           :render
           :update
           :input-character
           :input-key
           :resize))
(in-package :lem-terminal/terminal)

(defvar *terminal-id-counter* 0)

(defun generate-terminal-id () (incf *terminal-id-counter*))

(defvar *terminals* '())

(defun terminals ()
  *terminals*)

(defclass terminal ()
  ((timer :initarg :timer
          :accessor terminal-timer)
   (id :initarg :id
       :reader terminal-id)
   (viscus :initarg :viscus
           :reader terminal-viscus)
   (buffer :initarg :buffer
           :reader terminal-buffer)
   (rows :initarg :rows
         :accessor terminal-rows)
   (cols :initarg :cols
         :accessor terminal-cols)))

(defun find-terminal-by-id (id)
  (find id *terminals* :key #'terminal-id))

(defun remove-terminal (terminal)
  (alexandria:deletef *terminals* terminal))

(defun create (&key (rows (alexandria:required-argument :rows))
                    (cols (alexandria:required-argument :cols))
                    (buffer (alexandria:required-argument :buffer)))
  (let* ((id (generate-terminal-id))
         (terminal
           (make-instance 'terminal
                          :id id
                          :viscus (ffi::terminal-new id rows cols)
                          :buffer buffer
                          :rows rows
                          :cols cols))
         (timer
           (make-idle-timer (lambda ()
                              (ignore-errors
                                (with-error-handler ()
                                  (update terminal)
                                  (render terminal))))
                            :name (format nil "Terminal ~D" id))))
    (setf (terminal-timer terminal) timer)
    (start-timer timer 100 :repeat t)
    (push terminal *terminals*)
    terminal))

(defmethod destroy ((terminal terminal))
  (remove-terminal terminal)
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
  (let ((chars (ffi::terminal-last-cell-chars viscus)))
    (when chars
      (let ((char (ignore-errors (code-char (cffi:mem-ref chars :uint32 0)))))
        char))))

(defmethod render ((terminal terminal))
  (let* ((viscus (terminal-viscus terminal))
         (rows (terminal-rows terminal))
         (cols (terminal-cols terminal))
         (buffer (terminal-buffer terminal))
         (point (buffer-point buffer)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (loop :for row :from 0 :below rows
            :do (loop :with previous-attribute := nil
                      :and string := ""
                      :for col :from 0 :below cols
                      :do (ffi::terminal-query-cell viscus col row)
                          (let ((char (get-cell-character viscus))
                                (attribute (get-cell-attribute viscus)))
                            (when char
                              (unless (attribute-equal attribute previous-attribute)
                                (insert-string point string :attribute previous-attribute)
                                (setf previous-attribute attribute)
                                (setf string ""))
                              (setf string
                                    (concatenate 'string
                                                 string
                                                 (string (if (eql char #\Nul) #\Space char))))))
                      :finally (insert-string point string :attribute previous-attribute))
                (insert-character point #\newline)))
    (move-to-line point (1+ (ffi::terminal-cursor-row viscus)))
    (move-to-column point (ffi::terminal-cursor-col viscus))))

(defmethod update ((terminal terminal) &key with-block)
  (if with-block
      (ffi::terminal-process-input (terminal-viscus terminal))
      (ffi::terminal-process-input-nonblock (terminal-viscus terminal))))

(defmethod input-character ((terminal terminal) character &key (mod 0))
  (ffi::terminal-input-char (terminal-viscus terminal)
                            (char-code character)
                            mod)
  (run-update-timer terminal))

(defmethod input-key ((terminal terminal) key &key (mod 0))
  (ffi::terminal-input-key (terminal-viscus terminal)
                           key
                           mod)
  (run-update-timer terminal))

(defmethod resize ((terminal terminal)
                   &key (rows (alexandria:required-argument :rows))
                        (cols (alexandria:required-argument :cols)))
  (ffi::terminal-resize (terminal-viscus terminal) rows cols))

;;; callbacks
(defun cb-damage (rect id)
  (declare (ignore rect id))
  )

(defun cb-moverect (dest src id)
  (declare (ignore dest src id)))

(defun cb-movecursor (pos oldpos visible id)
  (declare (ignore pos oldpos visible id)))

(defun cb-settermprop (prop val id)
  (declare (ignore prop val id)))

(defun cb-bell (id)
  (declare (ignore id)))

(defun cb-resize (rows cols id)
  (let ((terminal (find-terminal-by-id id)))
    (setf (terminal-rows terminal) rows
          (terminal-cols terminal) cols)))

(defun cb-sb-pushline (cols cells id)
  (declare (ignore cols cells id)))

(defun cb-sb-popline (cols cells id)
  (declare (ignore cols cells id)))

(ffi::set-callbacks :damage 'cb-damage
                    :moverect 'cb-moverect
                    :movecursor 'cb-movecursor
                    :settermprop 'cb-settermprop
                    :bell 'cb-bell
                    :resize 'cb-resize
                    :sb-pushline 'cb-sb-pushline
                    :sb-popline 'cb-sb-popline)

;;; timer
(defun call-with-error-handler (function)
  (handler-bind ((error (lambda (c)
                          (message "~A"
                                   (with-output-to-string (out)
                                     (uiop:println c out)
                                     (uiop:print-backtrace :condition c :stream out))))))
    (funcall function)))

(defmacro with-error-handler (() &body body)
  `(call-with-error-handler (lambda () ,@body)))

(defun run-update-timer (terminal)
  (start-timer (terminal-timer terminal) 0))

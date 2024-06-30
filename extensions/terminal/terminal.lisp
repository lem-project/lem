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
           :input-key))
(in-package :lem-terminal/terminal)

(defvar *terminal-id-counter* 0)

(defun generate-terminal-id () (incf *terminal-id-counter*))

(defvar *terminals* '())

(defun terminals ()
  *terminals*)

(defclass terminal ()
  ((id :initarg :id
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
                          :cols cols)))
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

(defstruct cell
  width
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
                              (width (ffi::terminal-last-cell-width viscus))
                              (attribute (get-cell-attribute viscus)))
                          (setf (aref display row col)
                                (if (eql char #\Nul)
                                    (make-cell :width width :character #\space :attribute attribute)
                                    (make-cell :width width :character char :attribute attribute))))))
    display))

(defun %render (buffer display rows cols cursor-row cursor-col)
  (let ((point (buffer-point buffer)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (loop :for row :from 0 :below rows
            :do (loop :for col :from 0 :below cols
                      :do (let ((cell (aref display row col)))
                            (when (cell-character cell)
                              (insert-string point
                                             (string (cell-character cell))
                                             :attribute (cell-attribute cell)))))
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

;;; callbacks
(defun damage (rect id)
  (declare (ignore rect id))
  )

(defun moverect (dest src id)
  (declare (ignore dest src id)))

(defun movecursor (pos oldpos visible id)
  (declare (ignore pos oldpos visible id)))

(defun settermprop (prop val id)
  (declare (ignore prop val id)))

(defun bell (id)
  (declare (ignore id)))

(defun resize (rows cols id)
  (declare (ignore rows cols id)))

(defun sb-pushline (cols cells id)
  (declare (ignore cols cells id)))

(defun sb-popline (cols cells id)
  (declare (ignore cols cells id)))

(ffi::set-callbacks :damage 'damage
                    :moverect 'moverect
                    :movecursor 'movecursor
                    :settermprop 'settermprop
                    :bell 'bell
                    :resize 'resize
                    :sb-pushline 'sb-pushline
                    :sb-popline 'sb-popline)

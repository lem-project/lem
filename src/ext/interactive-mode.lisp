(uiop:define-package :lem/interactive-mode
  (:use :cl :lem)
  (:export :context
           :session-context
           :prompt
           :execute-input
           :insert
           :start-loading
           :stop-loading
           :run
           :with-output-stream))
(in-package :lem/interactive-mode)

(defclass context () ())

;;; session
(defclass session ()
  ((output-buffer
    :initarg :output-buffer
    :reader session-output-buffer)
   (input-buffer
    :initarg :input-buffer
    :reader session-input-buffer)
   (output-loading-spinner
    :initform nil
    :accessor session-output-loading-spinner)
   (contex
    :initarg :context
    :reader session-context)))

(defun buffer-session (buffer)
  (buffer-value buffer 'session))

(defun (setf buffer-session) (session buffer)
  (setf (buffer-value buffer 'session) session))

(defmethod insert ((session session) text &key attribute)
  (with-point ((point (buffer-point (session-output-buffer session)) :left-inserting))
    (buffer-end point)
    (insert-string point text :attribute attribute)))

(defmethod start-loading ((session session) loading-message)
  (let* ((buffer (session-output-buffer session))
         (point (buffer-point buffer)))
    (buffer-end point)
    (skip-whitespace-backward point)
    (insert-character point #\newline)
    (delete-between-points point (buffer-end-point buffer))
    (setf (session-output-loading-spinner session)
          (lem/loading-spinner:start-loading-spinner :line
                                                     :loading-message loading-message
                                                     :point point))))

(defmethod stop-loading ((session session))
  (lem/loading-spinner:stop-loading-spinner (session-output-loading-spinner session)))

;;; stream
(defun call-with-output-stream (session function)
  (with-open-stream (stream (make-buffer-output-stream (buffer-end-point
                                                        (session-output-buffer session))
                                                       :interactive t))
    (funcall function stream)
    (when (get-buffer-windows (session-output-buffer session))
      (redraw-display))))

(defmacro with-output-stream ((stream session) &body body)
  `(call-with-output-stream ,session (lambda (,stream) ,@body)))

(defun call-with-attribute (session attribute function)
  (with-point ((start (buffer-point (session-output-buffer session)))
               (end (buffer-point (session-output-buffer session)) :left-inserting))
    (funcall function)
    (put-text-property start end :attribute attribute)))

(defmacro with-attribute ((session attribute) &body body)
  `(call-with-attribute ,session ,attribute (lambda () ,@body)))

(defun current-session ()
  (buffer-session (current-buffer)))

;;; interactive-mode
(defgeneric prompt (session mode)
  (:method (session mode)
    (values "" nil)))

(defgeneric execute-input (session mode input)
  (:method (session mode input)))

(define-minor-mode interactive-mode
    (:name ""
     :keymap *interactive-mode-keymap*)
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil))

(define-key *interactive-mode-keymap* "Return" 'interactive/execute)

(defun make-space-string (string)
  (make-string (length string) :initial-element #\space))

(defmethod lem:compute-left-display-area-content ((mode interactive-mode) buffer point)
  (multiple-value-bind (string attribute)
      (prompt (buffer-session buffer)
              (ensure-mode-object (buffer-major-mode buffer)))
    (unless (first-line-p point)
      (setf string (make-space-string string)
            attribute nil))
    (lem/buffer/line:make-content :string string
                                  :attributes `((0 ,(length string) ,attribute)))))

(defun copy-input-buffer-to-output-buffer (session)
  (with-point ((output-point (buffer-point (session-output-buffer session)) :left-inserting))
    (buffer-end output-point)
    (insert-character output-point #\newline)
    (with-point ((first-point output-point))
      (insert-buffer output-point (session-input-buffer session))
      (multiple-value-bind (prompt-string prompt-attribute)
          (prompt session (ensure-mode-object (buffer-major-mode (session-input-buffer session))))
        (move-point output-point first-point)
        (insert-string output-point prompt-string :attribute prompt-attribute)
        (loop
          :while (line-offset output-point 1)
          :do (insert-string output-point (make-space-string prompt-string)))))))

(define-command interactive/execute () ()
  (assert (current-session))
  (let ((input (buffer-text (current-buffer)))
        (session (current-session)))
    (copy-input-buffer-to-output-buffer session)
    (erase-buffer (session-input-buffer session))
    (execute-input (buffer-session (current-buffer))
                   (ensure-mode-object (buffer-major-mode (current-buffer)))
                   input)))

;;;
(defun run (&key buffer-name mode context)
  (let ((buffer (make-buffer buffer-name :enable-undo-p nil))
        (attached-buffer (make-buffer (format nil "~A (attached)" buffer-name) :temporary t)))
    (change-buffer-mode attached-buffer 'interactive-mode)
    (change-buffer-mode attached-buffer mode)
    (attach-buffer buffer attached-buffer)
    (let ((window (pop-to-buffer buffer)))
      (switch-to-window window)
      (let ((session (make-instance 'session
                                    :output-buffer buffer
                                    :input-buffer attached-buffer
                                    :context context)))
        (setf (buffer-session buffer)
              session
              (buffer-session attached-buffer)
              session)
        session))))

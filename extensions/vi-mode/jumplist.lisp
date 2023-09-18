(defpackage :lem-vi-mode/jumplist
  (:use :cl
        :lem)
  (:import-from :lem-base
                :alive-point-p)
  (:import-from :alexandria
                :when-let
                :ignore-some-conditions)
  (:export :with-jumplist
           :without-jumplist
           :jump-back
           :jump-next
           :window-jumplist
           :current-jumplist
           :copy-jumplist))
(in-package :lem-vi-mode/jumplist)

(defvar *max-jumplist-size* 100)

(defstruct jumplist
  (history nil :type list)
  (index 0 :type (integer 0))
  (current nil :type (or null point)))

(defmethod print-object ((object jumplist) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (history index current)
        object
      (format stream ":HISTORY (~A items) :INDEX ~A"
              (+ (length history)
                 (if current 1 0))
              index))))

(defun delete-jumplist (jumplist)
  (check-type jumplist jumplist)
  (mapc #'delete-point (jumplist-history jumplist))
  (when (jumplist-current jumplist)
    (delete-point (jumplist-current jumplist)))
  (values))

(define-condition jumplist-invalid-index (error)
  ((jumplist :initarg :jumplist)
   (index :initarg :index))
  (:report (lambda (condition stream)
             (with-slots (jumplist index) condition
               (format stream "Invalid index ~A for ~A"
                       index
                       jumplist)))))

(defun jumplist-ref (jumplist index)
  (check-type index integer)
  (cond
    ((= index 0)
     (jumplist-current jumplist))
    ((< 0 index)
     (or (nth (1- index)
              (jumplist-history jumplist))
         (error 'jumplist-invalid-index
                :jumplist jumplist
                :index index)))
    (t
     (error 'jumplist-invalid-index
            :jumplist jumplist
            :index index))))

(defun jumplist-find-backward (jumplist from-index)
  (loop for i from from-index below *max-jumplist-size*
        for point = (jumplist-ref jumplist i)
        until (or (null point)
                  (alive-point-p point))
        finally (return (values point i))))

(defun jumplist-find-forward (jumplist from-index)
  (loop for i downfrom from-index
        for point = (jumplist-ref jumplist i)
        until (or (null point)
                  (alive-point-p point))
        finally (return (values point i))))

(defun delete-exceeded-elements (list count)
  (when-let ((last-cdr (nthcdr (- count 1) list)))
    (mapc #'delete-point (cdr last-cdr))
    (setf (cdr last-cdr) nil))
  list)

(defun jumplist-history-push (jumplist point)
  (with-slots (history index current) jumplist
    ;; Delete the newer history
    (when (< 0 index)
      (loop repeat (1- index)
            for point in history
            do (delete-point point))
      (setf history (nthcdr (1- index) history))
      (when current
        (delete-point current))
      (setf index 0
            current nil))
    (setf history
          (cons (copy-point point :left-inserting)
                ;; Remove points at the same line in the history
                (remove point
                        history
                        :test (lambda (new-point point)
                                (when (and (eq (point-buffer new-point)
                                               (point-buffer point))
                                           (= (line-number-at-point new-point)
                                              (line-number-at-point point)))
                                  (delete-point point)
                                  t))
                        :count 1)))
    ;; Keep only *max-jumplist-size* points (including `current`)
    (delete-exceeded-elements history (1- *max-jumplist-size*))
    point))

(defun jumplist-history-back (jumplist)
  (with-slots (index current) jumplist
    (when (zerop index)
      (setf current (copy-point (current-point) :left-inserting)))
    (ignore-some-conditions (jumplist-invalid-index)
      (multiple-value-bind (point new-index)
          (jumplist-find-backward jumplist (1+ index))
        (setf index new-index)
        point))))

(defun jumplist-history-next (jumplist)
  (with-slots (index current) jumplist
    (when (<= 0 (1- index))
      (ignore-some-conditions (jumplist-invalid-index)
        (multiple-value-bind (point new-index)
            (jumplist-find-forward jumplist (1- index))
          (setf index new-index)
          (when (zerop index)
            (delete-point current)
            (setf current nil))
          point)))))

(defun window-jumplist (window)
  (or (window-parameter window :vi-mode-jumplist)
      (let ((jumplist (make-jumplist)))
        (setf (window-parameter window :vi-mode-jumplist)
              jumplist)
        (push (lambda () (delete-jumplist jumplist))
              (window-delete-hook window))
        jumplist)))

(defun (setf window-jumplist) (jumplist window)
  (check-type jumplist jumplist)
  (when-let ((current-jumplist (window-parameter window :vi-mode-jumplist)))
    (delete-jumplist current-jumplist))
  (setf (window-parameter window :vi-mode-jumplist)
        jumplist))

(defun current-jumplist ()
  (window-jumplist (current-window)))

(defun jumplist-table (jumplist)
  (flet ((buffer-identifier (buffer)
           (if (buffer-filename buffer)
               (enough-namestring (buffer-filename buffer))
               (buffer-name buffer))))
    (with-slots (index current) jumplist
      (loop with results = (list (if (and current
                                          (alive-point-p current))
                                     (list (= 0 index)
                                           (abs (- 0 index))
                                           (line-number-at-point current)
                                           (point-column current)
                                           (buffer-identifier (point-buffer current)))
                                     (list (= 0 index)
                                           (abs (- 0 index))
                                           nil nil nil)))
            with dead-count = 0
            for point in (jumplist-history jumplist)
            for i from 1
            if (alive-point-p point)
            ;; current-p index line column file/buffer-name
            do (push (list
                      (= (- i dead-count) index)
                      (abs (- (- i dead-count) index))
                      (line-number-at-point point)
                      (point-column point)
                      (buffer-identifier (point-buffer point)))
                     results)
            else do (incf dead-count)
            finally (return results)))))

(defun print-jumplist (jumplist &optional (stream *standard-output*))
  (let ((table (jumplist-table jumplist)))
    (dolist (row table)
      (destructuring-bind (current-p index line column file)
          row
        (format stream "~:[  ~;> ~]~A: ~A~%"
                current-p
                index
                (and line column
                     (format nil "(~A, ~A)  ~A" line column file)))))))

(defvar *disable-jumplist* nil)

(defun call-with-jumplist (fn)
  (with-point ((p (current-point)))
    (let ((*disable-jumplist* t))
      (prog1 (funcall fn)
        (unless (and (eq (point-buffer p)
                         (current-buffer))
                     (= (line-number-at-point p)
                        (line-number-at-point (current-point))))
          (jumplist-history-push (current-jumplist) p))))))

(defmacro with-jumplist (&body body)
  `(if *disable-jumplist*
       (progn ,@body)
       (call-with-jumplist
        (lambda () ,@body))))

(defmacro without-jumplist (&body body)
  `(let ((*disable-jumplist* t))
     ,@body))

(defun jump-back ()
  (when-let ((point (jumplist-history-back (current-jumplist))))
    (switch-to-buffer (point-buffer point))
    (move-point (current-point) point)
    point))

(defun jump-next ()
  (when-let ((point (jumplist-history-next (current-jumplist))))
    (switch-to-buffer (point-buffer point))
    (move-point (current-point) point)))

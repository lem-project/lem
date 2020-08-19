(defpackage :lem-frame-multiplexer
  (:use :cl :lem :lem.button))
(in-package :lem-frame-multiplexer)

(defconstant +frame-multiplexer-max-number-of-frames+ 256)
(defconstant +frame-multiplexer-max-width-of-each-frame-name+ 20)

(defvar *virtual-frame-map* (make-hash-table))

(define-attribute frame-multiplexer-active-frame-name-attribute
  (t :foreground "white" :background "blue"))

(define-attribute frame-multiplexer-frame-name-attribute
  (t :foreground "white" :background "blue"))

(define-attribute frame-multiplexer-background-attribute
  (t :underline-p t))

(define-editor-variable frame-multiplexer nil ""
  (lambda (value)
    (if value
        (frame-multiplexer-on)
        (frame-multiplexer-off))))

(defclass virtual-frame (header-window)
  ((implementation
    :initarg :impl
    :initform nil
    :accessor virtual-frame-impl
    :type lem:implementation)
   (id/frame-table
    :initarg :id/frame-table
    :accessor virtual-frame-id/frame-table
    :type array)
   (current
    :initarg :current
    :accessor virtual-frame-current
    :type frame)
   (display-width
    :initarg :width
    :accessor virtual-frame-width)
   (display-height
    :initarg :height
    :accessor virtual-frame-height)
   (changed
    :initform t
    :accessor virtual-frame-changed)
   (buffer
    :initarg :buffer
    :accessor virtual-frame-header-buffer)))

(defun make-virtual-frame (impl frame)
  (declare (type frame frame))
  (let* ((buffer (make-buffer "*frame-multiplexer*" :enable-undo-p nil :temporary t))
         (id/frame-table (make-array +frame-multiplexer-max-number-of-frames+ :initial-element nil)))
    (setf (aref id/frame-table 0) frame)
    (setf (lem:variable-value 'truncate-lines :buffer buffer) nil)
    (let ((vf (make-instance 'virtual-frame
                             :impl impl
                             :buffer buffer
                             :width (display-width)
                             :height (display-height)
                             :id/frame-table id/frame-table
                             :current frame)))
      vf)))

(defun find-unused-frame-id (virtual-frame)
  (position-if #'null (virtual-frame-id/frame-table virtual-frame)))

(defun find-frame-id (virtual-frame frame)
  (position frame (virtual-frame-id/frame-table virtual-frame)))

(defun num-frames (virtual-frame)
  (count-if-not #'null (virtual-frame-id/frame-table virtual-frame)))

(defun allocate-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((id (find-unused-frame-id virtual-frame)))
    (setf (aref (virtual-frame-id/frame-table virtual-frame) id)
          frame)))

(defun free-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((id (find-frame-id virtual-frame frame)))
    (setf (aref (virtual-frame-id/frame-table virtual-frame) id)
          nil)))

(defun get-frame-from-id (virtual-frame id)
  (aref (virtual-frame-id/frame-table virtual-frame) id))

(defun linear-search-frame (virtual-frame frame dir wrap)
  (let ((id (find-frame-id virtual-frame frame)))
    (loop :for n := (funcall wrap (+ id dir)) :then (funcall wrap (+ n dir))
          :until (= n id)
          :do (unless (null (get-frame-from-id virtual-frame n))
                (return-from linear-search-frame (get-frame-from-id virtual-frame n))))))

(defun search-previous-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((len (length (virtual-frame-id/frame-table virtual-frame))))
    (linear-search-frame virtual-frame
                         frame
                         -1
                         (lambda (n)
                           (if (minusp n)
                               (+ (1- len) n)
                               n)))))

(defun search-next-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((len (length (virtual-frame-id/frame-table virtual-frame))))
    (linear-search-frame virtual-frame
                         frame
                         1
                         (lambda (n)
                           (if (>= n len)
                               (- len n)
                               n)))))

(defun virtual-frame-frames (virtual-frame)
  (coerce (remove-if #'null (virtual-frame-id/frame-table virtual-frame))
          'list))

(defun require-update-p (virtual-frame)
  (cond ((virtual-frame-changed virtual-frame) t)
        ((not (= (display-width)
                 (virtual-frame-width virtual-frame)))
         t)
        ((not (= (display-height)
                 (virtual-frame-height virtual-frame)))
         t)))

(defmethod window-redraw ((window virtual-frame) force)
  (when (or force
            (loop :for k :being :each :hash-key :of *virtual-frame-map*
                  :using (hash-value vf)
                  :thereis (require-update-p vf)))
    ;; draw button for frames
    (let* ((buffer (virtual-frame-header-buffer window))
           (p (buffer-point buffer))
           (charpos (point-charpos p)))
      (erase-buffer buffer)
      (dolist (frame (virtual-frame-frames window))
        (let ((focusp (eq frame (virtual-frame-current window)))
              (start-pos (point-charpos p)))
          (insert-button p
                         ;; virtual frame name on header
                         (let* ((buffer (window-buffer (lem:frame-current-window frame)))
                                (name (buffer-name buffer)))
                           (format nil "~a~a:~a "
                                   (if focusp #\# #\space)
                                   (find-frame-id window frame)
                                   (if (>= (length name) +frame-multiplexer-max-width-of-each-frame-name+)
                                       (format nil "~a..."
                                               (subseq name 0 +frame-multiplexer-max-width-of-each-frame-name+))
                                       name)))
                         ;; set action when click
                         (let ((frame frame))
                           (lambda ()
                             (setf (virtual-frame-current window) frame)
                             (setf (virtual-frame-changed window) t)))
                         :attribute (if focusp
                                        'frame-multiplexer-active-frame-name-attribute
                                        'frame-multiplexer-frame-name-attribute))
          ;; increment charpos
          (when focusp
            (let ((end-pos (point-charpos p)))
              (unless (<= start-pos charpos (1- end-pos))
                (setf charpos start-pos))))))
      ;; fill right margin
      (let ((margin-right (- (display-width) (point-column p))))
        (when (> margin-right 0)
          (insert-string p (make-string margin-right :initial-element #\space)
                         :attribute 'frame-multiplexer-background-attribute)))
      (line-offset p 0 charpos))
    ;; redraw windows in current frame
    (let* ((frame (virtual-frame-current (gethash (lem:implementation) *virtual-frame-map*))))
      (dolist (w (lem::window-tree-flatten (lem:frame-window-tree frame)))
        (lem:window-redraw w t))
      (dolist (w (lem:frame-floating-windows frame))
        (lem:window-redraw w t))
      (dolist (w (lem:frame-header-windows frame))
        (unless (eq w window)
          (lem:window-redraw w t))))
    (lem-if:update-display (lem:implementation)))
  ;; clear all virtual-frame-changed to nil because of applying redraw
  (maphash (lambda (k vf)
             (declare (ignore k))
             (setf (virtual-frame-changed vf) nil))
           *virtual-frame-map*)
  (call-next-method))

(defun frame-multiplexer-init ()
  (clrhash *virtual-frame-map*)
  (loop
    :for impl :in (list (implementation))  ; for multi-frame support in the future...
    :do (let ((vf (make-virtual-frame impl (lem:get-frame impl))))
          (setf (gethash impl *virtual-frame-map*) vf)
          (lem:map-frame (implementation) (virtual-frame-current vf)))))

(defun enabled-frame-multiplexer-p ()
  (variable-value 'frame-multiplexer :global))

(defun check-frame-multiplexer-enabled ()
  (unless (enabled-frame-multiplexer-p)
    (editor-error "frame-multiplexer-mode is not enabled")))

(defun frame-multiplexer-on ()
  (unless (enabled-frame-multiplexer-p)
    (frame-multiplexer-init)))

(defun frame-multiplexer-off ()
  (when (enabled-frame-multiplexer-p)
    (maphash (lambda (k v)
               (declare (ignore k))
               (delete-window v))
             *virtual-frame-map*)
    (clrhash *virtual-frame-map*)))

(define-command toggle-frame-multiplexer () ()
  (setf (variable-value 'frame-multiplexer :global)
        (not (variable-value 'frame-multiplexer :global))))

(define-key *global-keymap* "C-z c" 'frame-multiplexer-create-with-new-buffer-list)
(define-command frame-multiplexer-create-with-new-buffer-list () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (id (find-unused-frame-id vf)))
    (when (null id)
      (editor-error "it's full of frames in virtual frame"))
    (let* ((frame (lem:make-frame))
           (tmp-buffer (primordial-buffer)))
      (lem:setup-frame frame tmp-buffer)
      (push vf (lem:frame-header-windows frame))
      (let ((new-window (lem::make-window tmp-buffer
                                          (lem::window-topleft-x) (lem::window-topleft-y)
                                          (lem::window-max-width) (lem::window-max-height)
                                          t)))
        (setf (lem:frame-window-tree frame) new-window)
        (setf (lem:frame-current-window frame) new-window)
        (setf (virtual-frame-current vf) frame)
        (allocate-frame vf frame)
        (lem:map-frame (implementation) frame))
      (setf (virtual-frame-changed vf) t))))

(define-key *global-keymap* "C-z d" 'frame-multiplexer-delete)
(define-command frame-multiplexer-delete () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (num (num-frames vf)))
    (when (= num 1)
      (editor-error "cannot delete this virtual frame"))
    (let ((frame (search-previous-frame vf (virtual-frame-current vf))))
      (free-frame vf (virtual-frame-current vf))
      (setf (virtual-frame-current vf) frame)
      (lem:map-frame (implementation) frame))
    (setf (virtual-frame-changed vf) t)))

(define-key *global-keymap* "C-z p" 'frame-multiplexer-prev)
(define-command frame-multiplexer-prev () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (frame (search-previous-frame vf (virtual-frame-current vf))))
    (when frame
      (setf (virtual-frame-current vf) frame)
      (lem:map-frame (implementation) frame))
    (lem::change-display-size-hook)
    (setf (virtual-frame-changed vf) t)))

(define-key *global-keymap* "C-z n" 'frame-multiplexer-next)
(define-command frame-multiplexer-next () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (frame (search-next-frame vf (virtual-frame-current vf))))
    (when frame
      (setf (virtual-frame-current vf) frame)
      (lem:map-frame (implementation) frame))
    (lem::change-display-size-hook)
    (setf (virtual-frame-changed vf) t)))

(define-command frame-multiplexer-test () ()
  (labels ((vf ()
             (maphash (lambda (k v)
                        (declare (ignore k))
                        (return-from vf v))
                      *virtual-frame-map*))
           (check-nth-frame (expected-id)
             (let* ((virtual-frame (vf))
                    (actual-id (find-frame-id virtual-frame (virtual-frame-current virtual-frame))))
               (assert (= expected-id actual-id))))
           (check-tabs (&rest nums)
             (let ((virtual-frame (vf)))
               (assert (equal nums (mapcar (lambda (frame)
                                             (find-frame-id virtual-frame frame))
                                           (virtual-frame-frames virtual-frame)))))))
    (when (enabled-frame-multiplexer-p)
      (editor-error "frame-multiplexer-mode is already enabled"))
    ;; frame-multiplexer-create-with-new-buffer-list
    (toggle-frame-multiplexer)
    (frame-multiplexer-create-with-new-buffer-list)
    (check-nth-frame 1)
    (frame-multiplexer-create-with-new-buffer-list)
    (check-nth-frame 2)
    (frame-multiplexer-create-with-new-buffer-list)
    (check-nth-frame 3)
    (frame-multiplexer-next)
    ;; frame-multiplexer-next, frame-multiplexer-prev
    (frame-multiplexer-next)
    (check-nth-frame 1)
    (frame-multiplexer-prev)
    (check-nth-frame 0)
    (frame-multiplexer-prev)
    (check-nth-frame 3)
    (frame-multiplexer-next)
    (check-nth-frame 0)
    (check-tabs 0 1 2 3)
    ;; frame-multiplexer-delete
    (frame-multiplexer-delete)
    (check-tabs 1 2 3)
    (check-nth-frame 3)
    (frame-multiplexer-prev)
    (check-nth-frame 2)
    (frame-multiplexer-delete)
    (check-tabs 1 3)
    (check-nth-frame 1)
    ))

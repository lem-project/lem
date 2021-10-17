(defpackage :lem-frame-multiplexer
  (:use :cl :lem :lem.button))
(in-package :lem-frame-multiplexer)

(defconstant +max-number-of-frames+ 256)
(defconstant +max-width-of-each-frame-name+ 20)

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
    :type (or null implementation))
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
         (id/frame-table (make-array +max-number-of-frames+ :initial-element nil)))
    (setf (aref id/frame-table 0) frame)
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (make-instance 'virtual-frame
                   :impl impl
                   :buffer buffer
                   :width (display-width)
                   :height (display-height)
                   :id/frame-table id/frame-table
                   :current frame)))

(defun switch-current-frame (virtual-frame frame)

  ;; save buffer-point to window-point
  (move-point (lem::%window-point (current-window))
              (lem::window-buffer-point (current-window)))

  (setf (virtual-frame-current virtual-frame) frame)
  (setf (virtual-frame-changed virtual-frame) t)
  (notify-frame-redisplay-required frame)
  (map-frame (implementation) frame)

  ;; set current-buffer
  (setf (current-buffer) (window-buffer (current-window)))

  ;; restore buffer-point from window-point
  (move-point (lem::window-buffer-point (current-window))
              (lem::%window-point (current-window)))
  )

(defun find-unused-frame-id (virtual-frame)
  (position-if #'null (virtual-frame-id/frame-table virtual-frame)))

(defun find-frame-id (virtual-frame frame)
  (position frame (virtual-frame-id/frame-table virtual-frame)))

(defun num-frames (virtual-frame)
  (count-if-not #'null (virtual-frame-id/frame-table virtual-frame)))

(defun allocate-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((id (find-unused-frame-id virtual-frame)))
    (assert id)
    ;; NOTE:
    ;; primordial-bufferは現在のバッファリストから*tmp*バッファを返すが
    ;; バッファリストをフレームごとに管理する場合にここで*tmp*バッファを返すと
    ;; 元のフレームの*tmp*バッファを新しい方のフレームから参照することになってしまう
    (setup-frame frame (primordial-buffer))
    (setf (aref (virtual-frame-id/frame-table virtual-frame) id)
          frame)))

(defun free-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((id (find-frame-id virtual-frame frame)))
    (assert id)
    (teardown-frame frame)
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

(defun changed-current-buffer-p (current-frame)
  (not (eq (current-buffer)
           (window-buffer (frame-current-window current-frame)))))

(defun require-update-p (virtual-frame)
  (or (virtual-frame-changed virtual-frame)
      (not (= (display-width)
              (virtual-frame-width virtual-frame)))
      (not (= (display-height)
              (virtual-frame-height virtual-frame)))
      (virtual-frame-current virtual-frame)
      (changed-current-buffer-p (virtual-frame-current virtual-frame))))

(defmethod window-redraw ((window virtual-frame) force)
  (when (or force (require-update-p window))
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
                         (let* ((buffer (window-buffer (frame-current-window frame)))
                                (name (buffer-name buffer)))
                           (format nil "~a~a:~a "
                                   (if focusp #\# #\space)
                                   (find-frame-id window frame)
                                   (if (>= (length name) +max-width-of-each-frame-name+)
                                       (format nil "~a..."
                                               (subseq name 0 +max-width-of-each-frame-name+))
                                       name)))
                         ;; set action when click
                         (let ((frame frame))
                           (lambda ()
                             (switch-current-frame window frame)))
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
      (line-offset p 0 charpos)))
  ;; clear virtual-frame-changed to nil because of applying redraw
  (setf (virtual-frame-changed window) nil)
  (call-next-method))

(defun frame-multiplexer-init ()
  (clrhash *virtual-frame-map*)
  (loop
    :for impl :in (list (implementation))  ; for multi-frame support in the future...
    :do (let ((vf (make-virtual-frame impl (get-frame impl))))
          (setf (gethash impl *virtual-frame-map*) vf)
          (switch-current-frame vf (virtual-frame-current vf)))))

(defun enabled-frame-multiplexer-p ()
  (variable-value 'frame-multiplexer :global))

(defun check-frame-multiplexer-enabled ()
  (unless (enabled-frame-multiplexer-p)
    (editor-error "frame-multiplexer-mode is not enabled")))

(defun frame-multiplexer-on ()
  (unless (enabled-frame-multiplexer-p)
    (frame-multiplexer-init)
    (change-class (lem-base::buffer-list-manager) 'buffer-list-manager)))

(defun frame-multiplexer-off ()
  (when (enabled-frame-multiplexer-p)
    (maphash (lambda (k v)
               (declare (ignore k))
               (delete-window v))
             *virtual-frame-map*)
    (clrhash *virtual-frame-map*)
    ;; TODO: buffer-list-managerを元に戻す
    ))

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
    (let ((frame (make-frame (current-frame))))
      (allocate-frame vf frame)
      (switch-current-frame vf frame))))

(define-key *global-keymap* "C-z d" 'frame-multiplexer-delete)
(define-command frame-multiplexer-delete () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (num (num-frames vf)))
    (when (= num 1)
      (editor-error "cannot delete this virtual frame"))
    (let* ((frame-now  (virtual-frame-current vf))
           (frame-prev (search-previous-frame vf frame-now)))
      (switch-current-frame vf frame-prev)
      (free-frame vf frame-now))))

(define-key *global-keymap* "C-z p" 'frame-multiplexer-prev)
(define-command frame-multiplexer-prev () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (frame (search-previous-frame vf (virtual-frame-current vf))))
    (when frame
      (switch-current-frame vf frame))
    (lem::change-display-size-hook)))

(define-key *global-keymap* "C-z n" 'frame-multiplexer-next)
(define-command frame-multiplexer-next () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (frame (search-next-frame vf (virtual-frame-current vf))))
    (when frame
      (switch-current-frame vf frame))
    (lem::change-display-size-hook)))

(defun enable-frame-multiplexer ()
  (setf (variable-value 'frame-multiplexer :global) t))

(add-hook *after-init-hook* 'enable-frame-multiplexer)

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


(defclass buffer-list-manager (lem::buffer-list-manager) ())

(defmethod lem-base::delete-buffer-using-manager :before
    ((manager buffer-list-manager)
     buffer)
  (maphash (lambda (k virtual-frame)
             (declare (ignore k))
             (dolist (frame (virtual-frame-frames virtual-frame))
               (lem::strip-buffer-from-frame-windows buffer frame)))
           *virtual-frame-map*))

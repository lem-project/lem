(defpackage :lem-fm-mode
  (:use :cl :lem :lem.button))
(in-package :lem-fm-mode)

(defconstant +fm-max-number-of-frames+ 256)
(defconstant +fm-max-width-of-each-frame-name+ 20)

(defvar *virtual-frame-map* (make-hash-table))

(define-attribute fm-active-frame-name-attribute
  (t :foreground "white" :background "blue"))

(define-attribute fm-frame-name-attribute
  (t :foreground "white" :background "blue"))

(define-attribute fm-background-attribute
  (t :underline-p t))

(define-editor-variable frame-multiplexer nil ""
  (lambda (value)
    (if value
        (frame-multiplexer-on)
        (frame-multiplexer-off))))

(defstruct (%frame (:constructor %make-frame (id frame)))
  (id 0 :type integer)
  (frame nil :type lem:frame))

(defclass virtual-frame (header-window)
  ((implementation
    :initarg :impl
    :initform nil
    :accessor virtual-frame-impl
    :type lem:implementation)
   (frames
    :initarg :frames
    :accessor virtual-frame-frames
    :type array)
   (current
    :initarg :current
    :accessor virtual-frame-current
    :type %frame)
   (buffer-list-map
    :initarg :buffer-list-map
    :accessor virtual-frame-buffer-list-map)
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
  (let* ((buffer (make-buffer "*fm*" :enable-undo-p nil :temporary t))
         (%frame (%make-frame 0 frame))
         (frames (make-array +fm-max-number-of-frames+ :initial-element nil)))
    (setf (aref frames 0) %frame)
    (setf (lem:variable-value 'truncate-lines :buffer buffer) nil)
    (let ((vf (make-instance 'virtual-frame
                             :impl impl
                             :buffer buffer
                             :width (display-width)
                             :height (display-height)
                             :frames frames
                             :current %frame
                             :buffer-list-map (make-hash-table))))
      vf)))

(defun all-buffer-list ()
  (remove-duplicates
   (loop
     :for k :being :each :hash-key :of *virtual-frame-map*
     :using (hash-value vf)
     :append (loop
               :for k :being :each :hash-key :of (virtual-frame-buffer-list-map vf)
               :using (hash-value buffer-list)
               :append buffer-list))))

(defun find-unused-frame-id (virtual-frame)
  (position-if #'null (virtual-frame-frames virtual-frame)))

(defun num-frames (virtual-frame)
  (count-if-not #'null (virtual-frame-frames virtual-frame)))

(defun allocate-frame (virtual-frame frame)
  (setf (aref (virtual-frame-frames virtual-frame) (%frame-id frame))
        frame))

(defun free-frame (virtual-frame frame)
  (setf (aref (virtual-frame-frames virtual-frame) (%frame-id frame))
        nil))

(defun search-previous-frame (vf frame)
  (let* ((frames (virtual-frame-frames vf))
         (id (%frame-id frame))
         (len (length frames)))
    (flet ((wrap (n)
             (if (minusp n)
                 (+ (1- len) n)
                 n)))
      (loop
        :for n := (wrap (1- id)) :then (wrap (1- n))
        :until (= n id)
        :do (unless (null (aref frames n))
              (return-from search-previous-frame (aref frames n)))))))

(defun search-next-frame (vf frame)
  (let* ((frames (virtual-frame-frames vf))
         (id (%frame-id frame))
         (len (length frames)))
    (flet ((wrap (n)
             (if (>= n len)
                 (- len n)
                 n)))
      (loop
        :for n := (wrap (1+ id)) :then (wrap (1+ n))
        :until (= n id)
        :do (unless (null (aref frames n))
              (return-from search-next-frame (aref frames n)))))))

(defun require-update-p (vf)
  (cond ((virtual-frame-changed vf) t)
        ((not (= (display-width)
                 (virtual-frame-width vf)))
         t)
        ((not (= (display-height)
                 (virtual-frame-height vf)))
         t)))

(defun set-to-frame-buffer-list (vf frame)
  (let ((buffer-list-map (virtual-frame-buffer-list-map vf))
        (buffer-list (copy-list (buffer-list))))
    (setf (gethash frame buffer-list-map) buffer-list)))

(defun set-to-current-buffer-list (vf frame)
  (let* ((buffer-list-map (virtual-frame-buffer-list-map vf))
         (buffer-list (gethash frame buffer-list-map)))
    (lem-base::set-buffer-list buffer-list)))

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
      (loop
        :for %frame :across (virtual-frame-frames window)
        :unless (null %frame)
        :do (let* ((focusp (eq %frame (virtual-frame-current window)))
                   (start-pos (point-charpos p)))
              (insert-button p
                             ;; virtual frame name on header
                             (let* ((frame (%frame-frame %frame))
                                    (buffer (window-buffer (lem:frame-current-window frame)))
                                    (name (buffer-name buffer)))
                               (format nil "~a~a:~a "
                                       (if focusp #\# #\space)
                                       (%frame-id %frame)
                                       (if (>= (length name) +fm-max-width-of-each-frame-name+)
                                           (format nil "~a..."
                                                   (subseq name 0 +fm-max-width-of-each-frame-name+))
                                           name)))
                             ;; set action when click
                             (let ((%frame %frame))
                               (lambda ()
                                 (setf (virtual-frame-current window) %frame)
                                 (setf (virtual-frame-changed window) t)))
                             :attribute (if focusp
                                            'fm-active-frame-name-attribute
                                            'fm-frame-name-attribute))
              ;; increment charpos
              (when focusp
                (let ((end-pos (point-charpos p)))
                  (unless (<= start-pos charpos (1- end-pos))
                    (setf charpos start-pos))))))
      ;; fill right margin
      (let ((margin-right (- (display-width) (point-column p))))
        (when (> margin-right 0)
          (insert-string p (make-string margin-right :initial-element #\space)
                         :attribute 'fm-background-attribute)))
      (line-offset p 0 charpos))
    ;; redraw windows in current frame
    (let* ((%frame (virtual-frame-current (gethash (lem:implementation) *virtual-frame-map*)))
           (frame (%frame-frame %frame)))
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
          (setf (gethash (virtual-frame-current vf) (virtual-frame-buffer-list-map vf))
                (copy-list (buffer-list)))
          (lem:map-frame (implementation) (%frame-frame (virtual-frame-current vf))))))

(defun kill-buffer-from-all-frames (buffer)
  ;; update buffer-list-map
  (let ((vf (gethash (implementation) *virtual-frame-map*)))
    (setf (gethash (virtual-frame-current vf) (virtual-frame-buffer-list-map vf))
          (copy-list (buffer-list))))
  ;; kill buffer from all frames in all virtual frames
  (loop
    :for display :being :each :hash-key :of *virtual-frame-map*
    :using (hash-value vf)
    :do (let ((current-frame (virtual-frame-current vf)))
          (dolist (frame (alexandria:hash-table-keys (virtual-frame-buffer-list-map vf)))
            (unwind-protect
                 (progn
                   ;; temporary switched current frame and buffer-list
                   (map-frame display (%frame-frame frame))
                   (setf (virtual-frame-current vf) frame)
                   (let ((frame-buffer-list (gethash frame (virtual-frame-buffer-list-map vf))))
                     (set-to-current-buffer-list vf frame))
                   ;; switch buffers that will be deleted
                   (dolist (window (get-buffer-windows buffer))
                     (with-current-window window
                       (switch-to-buffer (or (get-previous-buffer buffer)
                                             (car (last (buffer-list)))))))
                   ;; delete buffer from the frame
                   (lem-base::set-buffer-list (delete buffer (buffer-list))))
              ;; restore current frame and buffer-list
              (progn
                (setf (virtual-frame-current vf) current-frame
                      (virtual-frame-changed vf) t)
                (set-to-frame-buffer-list vf frame)
                (map-frame display (%frame-frame current-frame))))))))

(defun enabled-frame-multiplexer-p ()
  (variable-value 'frame-multiplexer :global))

(defun check-frame-multiplexer-enabled ()
  (unless (enabled-frame-multiplexer-p)
    (editor-error "fm-mode is not enabled")))

(defun frame-multiplexer-on ()
  (unless (enabled-frame-multiplexer-p)
    (add-hook (variable-value 'kill-buffer-hook :global) 'kill-buffer-from-all-frames)
    (frame-multiplexer-init)))

(defun frame-multiplexer-off ()
  (when (enabled-frame-multiplexer-p)
    (remove-hook (variable-value 'kill-buffer-hook :global) 'kill-buffer-from-all-frames)
    (maphash (lambda (k v)
               (declare (ignore k))
               (delete-window v))
             *virtual-frame-map*)
    (clrhash *virtual-frame-map*)))

(define-command fm-mode () ()
  (setf (variable-value 'frame-multiplexer :global)
        (not (variable-value 'frame-multiplexer :global))))

(defun create-frame (new-buffer-list-p)
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (id (find-unused-frame-id vf)))
    (when (null id)
      (editor-error "it's full of frames in virtual frame"))
    (set-to-frame-buffer-list vf (virtual-frame-current vf))
    (let* ((frame (lem:make-frame))
           (%frame (%make-frame id frame)))
      (lem:setup-frame frame)
      ;;create new-window with *tmp* buffer by default
      (let* ((tmp-buffer (find "*tmp*" (append (buffer-list) (all-buffer-list))
                               :key (lambda (b) (buffer-name b))
                               :test #'string=))
             (buffer-list (if new-buffer-list-p
                              (list tmp-buffer)
                              (copy-list (buffer-list)))))
        ;; set buffer-list to virtual frame and global buffer-list
        (setf (gethash %frame (virtual-frame-buffer-list-map vf)) buffer-list)
        (lem-base::set-buffer-list buffer-list)
        ;; create window and set to frame
        (when tmp-buffer
          (push vf (lem:frame-header-windows frame))
          (let ((new-window (lem::make-window tmp-buffer
                                              (lem::window-topleft-x) (lem::window-topleft-y)
                                              (lem::window-max-width) (lem::window-max-height)
                                              t)))
            (setf (lem:frame-window-tree frame) new-window
                  (lem:frame-current-window frame) new-window
                  (virtual-frame-current vf) %frame)
            ;; expose frame
            (allocate-frame vf %frame)
            (lem:map-frame (implementation) frame))))
      ;; new window should be redraw
      (setf (virtual-frame-changed vf) t))))

(define-key *global-keymap* "C-z c" 'fm-create-with-new-buffer-list)
(define-command fm-create-with-new-buffer-list () ()
  (create-frame t))

(define-key *global-keymap* "C-z C" 'fm-create)
(define-command fm-create () ()
  (create-frame nil))

(define-key *global-keymap* "C-z d" 'fm-delete)
(define-command fm-delete () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (num (num-frames vf)))
    (when (= num 1)
      (editor-error "cannot delete this virtual frame"))
    (remhash (virtual-frame-current vf) (virtual-frame-buffer-list-map vf))
    (free-frame vf (virtual-frame-current vf))
    (let ((%frame (search-previous-frame vf (virtual-frame-current vf))))
      (setf (virtual-frame-current vf) %frame)
      (lem:map-frame (implementation) (%frame-frame %frame)))
    (setf (virtual-frame-changed vf) t)))

(define-key *global-keymap* "C-z p" 'fm-prev)
(define-command fm-prev () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*)))
    (let ((%frame (search-previous-frame vf (virtual-frame-current vf))))
      (when %frame
        (let ((prev-current (virtual-frame-current vf)))
          (set-to-frame-buffer-list vf prev-current))
        (setf (virtual-frame-current vf) %frame)
        (set-to-current-buffer-list vf %frame)
        (lem:map-frame (implementation) (%frame-frame %frame))))
    (lem::change-display-size-hook)
    (setf (virtual-frame-changed vf) t)))

(define-key *global-keymap* "C-z n" 'fm-next)
(define-command fm-next () ()
  (check-frame-multiplexer-enabled)
  (let* ((vf (gethash (implementation) *virtual-frame-map*)))
    (let ((%frame (search-next-frame vf (virtual-frame-current vf))))
      (when %frame
        (let ((prev-current (virtual-frame-current vf)))
          (set-to-frame-buffer-list vf prev-current))
        (setf (virtual-frame-current vf) %frame)
        (set-to-current-buffer-list vf %frame)
        (lem:map-frame (implementation) (%frame-frame %frame))))
    (lem::change-display-size-hook)
    (setf (virtual-frame-changed vf) t)))

(defun completion-buffer-name-from-all-frames (str)
  (completion-strings str (mapcar #'buffer-name (all-buffer-list))))

(define-key *global-keymap* "C-z b" 'fm-select-buffer-from-all-frames)
(define-command fm-select-buffer-from-all-frames (name)
    ((list (let ((lem:*minibuffer-buffer-complete-function*
                   #'completion-buffer-name-from-all-frames))
             (prompt-for-buffer "Use buffer: " (buffer-name (current-buffer))
                                t (all-buffer-list)))))
  (check-frame-multiplexer-enabled)
  (let ((buffer (find name (all-buffer-list) :test #'string= :key #'buffer-name)))
    (when (null (find buffer (buffer-list)))
      (lem-base::set-buffer-list (cons buffer (buffer-list))))
    (select-buffer buffer)))

(defpackage :lem/filer
  (:use :cl :lem)
  (:export :subdirectory-p
           :expand-to-directory
           :expand-directory-item
           :find-child-directory-item
           :create-directory-item
           :directory-item
           :directory-item-open-p
           :directory-item-children
           :file-item
           :item-pathname))
(in-package :lem/filer)

(define-attribute triangle-attribute
  (t :bold t :foreground :base0D))

(define-attribute current-file-attribute
  (t :bold t :background :base01))

(define-major-mode filer-mode ()
    (:name "Filer"
     :keymap *filer-mode-keymap*)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) nil)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *global-keymap* "C-x d" 'filer)
(define-key *filer-mode-keymap* "Return" 'filer-select)
(define-key *filer-mode-keymap* "D" 'filer-delete-file)

(defclass item ()
  ((pathname :initarg :pathname
             :reader item-pathname)))

(defclass file-item (item) ())

(defclass directory-item (item)
  ((open :initarg :open
         :initform nil
         :accessor directory-item-open-p)
   (children :initarg :children
             :initform nil
             :accessor directory-item-children)))

(defgeneric item-content (item)
  (:method ((item file-item))
    (enough-namestring (item-pathname item)
                       (uiop:pathname-directory-pathname (item-pathname item))))
  (:method ((item directory-item))
    (enough-namestring (item-pathname item)
                       (uiop:pathname-parent-directory-pathname (item-pathname item)))))

(defun create-item (pathname &key open)
  (if (uiop:directory-pathname-p pathname)
      (create-directory-item pathname :open open)
      (create-file-item pathname)))

(defun create-file-item (pathname)
  (make-instance 'file-item :pathname pathname))

(defun create-directory-children (pathname)
  (loop :for pathname :in (list-directory pathname)
        :collect (create-item pathname :open nil)))

(defun create-directory-item (pathname &key open)
  (if open
      (make-instance 'directory-item
                     :pathname pathname
                     :open t
                     :children (create-directory-children pathname))
      (make-instance 'directory-item
                     :pathname pathname
                     :open nil
                     :children '())))

(defmethod open-item ((item file-item) buffer)
  (when (typep (current-window) 'lem-core:side-window)
    (next-window))
  (find-file (item-pathname item)))

(defmethod open-item ((item directory-item) buffer)
  (cond ((directory-item-open-p item)
         (setf (directory-item-open-p item) nil)
         (setf (directory-item-children item) '()))
        (t
         (setf (directory-item-open-p item) t)
         (setf (directory-item-children item)
               (create-directory-children (item-pathname item)))))
  (render buffer (root-item buffer)))

(defun root-item (buffer)
  (buffer-value buffer 'root-item))

(defun (setf root-item) (item buffer)
  (setf (buffer-value buffer 'root-item) item))

(defun select (point)
  (let ((buffer (point-buffer point))
        (item (text-property-at point :item)))
    (when item
      (open-item item buffer))))

(defun insert-item (point item)
  (with-point ((start point))
    (back-to-indentation start)
    (lem/directory-mode/internal::insert-icon point (item-pathname item))
    (insert-string point
                   (item-content item)
                   :attribute (lem/directory-mode/internal::get-file-attribute (item-pathname item)))
    (put-text-property start point :item item)
    (lem-core::set-clickable start
                             point
                             (lambda (window point)
                               (declare (ignore window))
                               (select point)))))

(defmethod render-item :before (point item depth)
  (insert-string point (make-string (* depth 2) :initial-element #\space)))

(defmethod render-item (point (item file-item) depth)
  (insert-string point "  ")
  (insert-item point item))

(defmethod render-item (point (item directory-item) depth)
  (insert-string point
                 (uiop:strcat 
                  (if (directory-item-open-p item)
                      (icon-string "down-pointing-triangle")
                      (icon-string "right-pointing-triangle"))
                  " ")
                 :attribute 'triangle-attribute)
  (insert-item point item)
  (dolist (item (directory-item-children item))
    (insert-character point #\newline)
    (render-item point item (1+ depth))))

(defun call-with-fix-scroll (function)
  (let ((n (if (lem-core::frame-leftside-window (current-frame))
               (1- (line-number-at-point 
                    (window-view-point
                     (lem-core::frame-leftside-window (current-frame)))))
               0)))
    (funcall function)
    (when (lem-core::frame-leftside-window (current-frame))
      (scroll-down n (lem-core::frame-leftside-window (current-frame))))))

(defmacro with-fix-scroll (() &body body)
  `(call-with-fix-scroll (lambda () ,@body)))

(defun render (buffer item)
  (with-buffer-read-only buffer nil
    (with-fix-scroll ()
      (let ((line (line-number-at-point (buffer-point buffer))))
        (setf (root-item buffer) item)
        (erase-buffer buffer)
        (render-item (buffer-point buffer) item 0)
        (move-to-line (buffer-point buffer) line)
        (back-to-indentation (buffer-point buffer))))))

(defun make-filer-buffer (directory)
  (let* ((directory (probe-file directory))
         (buffer (make-buffer "*Filer*" :temporary t)))
    (change-buffer-mode buffer 'filer-mode)
    (render buffer (create-directory-item directory :open t))
    (setf (not-switchable-buffer-p buffer) t)
    buffer))

(defun filer-active-p ()
  (and (lem-core::frame-leftside-window (current-frame))
       (eq 'filer-mode
           (buffer-major-mode
            (window-buffer
             (lem-core::frame-leftside-window
              (current-frame)))))))

(defun filer-buffer ()
  "Return the Filer buffer if it exists, or nil."
  (alexandria:when-let ((window (frame-leftside-window (current-frame))))
    (let ((buffer (window-buffer window)))
      (when (eq 'filer-mode (buffer-major-mode buffer))
        buffer))))

(defun filer-current-directory ()
  "Return the root directory of the current Filer, or nil if Filer is not active."
  (when (filer-active-p)
    (alexandria:when-let ((buffer (filer-buffer)))
      (alexandria:when-let ((root (root-item buffer)))
        (item-pathname root)))))

(defun subdirectory-p (child parent)
  "Return T if CHILD is a subdirectory of PARENT."
  (let ((child-str (namestring child))
        (parent-str (namestring parent)))
    (and (> (length child-str) (length parent-str))
         (string= parent-str child-str :end2 (length parent-str)))))

(defun find-child-directory-item (parent-item target-dir)
  "Find a child directory-item in PARENT-ITEM that is on the path to TARGET-DIR."
  (loop :for child :in (directory-item-children parent-item)
        :when (and (typep child 'directory-item)
                   (let ((child-path (item-pathname child)))
                     (or (uiop:pathname-equal child-path target-dir)
                         (subdirectory-p target-dir child-path))))
          :return child))

(defun expand-directory-item (item)
  "Expand a directory-item if not already expanded."
  (unless (directory-item-open-p item)
    (setf (directory-item-open-p item) t)
    (setf (directory-item-children item)
          (create-directory-children (item-pathname item)))))

(defun expand-to-directory (root-item target-dir)
  "Expand the tree from ROOT-ITEM to TARGET-DIR.
Returns T if expansion was performed, NIL otherwise."
  (let ((root-path (item-pathname root-item)))
    (when (or (uiop:pathname-equal root-path target-dir)
              (subdirectory-p target-dir root-path))
      (expand-directory-item root-item)
      (loop :for current-item := root-item
              :then next-item
            :for next-item := (find-child-directory-item current-item target-dir)
            :while next-item
            :do (expand-directory-item next-item)
            :finally (return t)))))

(defun current-file-overlay (buffer)
  "Get the current file highlight overlay for BUFFER."
  (buffer-value buffer 'current-file-overlay))

(defun (setf current-file-overlay) (overlay buffer)
  "Set the current file highlight overlay for BUFFER."
  (setf (buffer-value buffer 'current-file-overlay) overlay))

(defun clear-current-file-highlight (filer-buf)
  "Clear the current file highlight in FILER-BUF."
  (alexandria:when-let ((overlay (current-file-overlay filer-buf)))
    (delete-overlay overlay)
    (setf (current-file-overlay filer-buf) nil)))

(defun highlight-file-in-filer (filer-buf file-path)
  "Highlight FILE-PATH in FILER-BUF and move point to it."
  (clear-current-file-highlight filer-buf)
  (with-point ((point (buffer-point filer-buf)))
    (buffer-start point)
    (loop :while (not (end-buffer-p point))
          :do (back-to-indentation point)
              (alexandria:when-let ((item (text-property-at point :item)))
                (when (and (typep item 'file-item)
                           (uiop:pathname-equal (item-pathname item) file-path))
                  (with-point ((start point)
                               (end point))
                    (line-start start)
                    (line-end end)
                    (let ((overlay (make-overlay start end 'current-file-attribute)))
                      (setf (current-file-overlay filer-buf) overlay)))
                  (return)))
              (unless (line-offset point 1)
                (return)))))

(defun sync-filer-to-buffer-directory (buffer)
  "Sync the Filer to BUFFER's directory if Filer is active.
Expands the tree to show the buffer's directory and highlights the current file."
  (when (and (filer-active-p)
             (not (eq 'filer-mode (buffer-major-mode buffer)))
             (not (not-switchable-buffer-p buffer)))
    (alexandria:when-let ((buffer-dir (ignore-errors (probe-file (buffer-directory buffer)))))
      (alexandria:when-let ((filer-buf (filer-buffer)))
        (alexandria:when-let ((root (root-item filer-buf)))
          (when (expand-to-directory root buffer-dir)
            (render filer-buf root))
          (alexandria:when-let ((file-path (buffer-filename buffer)))
            (highlight-file-in-filer filer-buf file-path)))))))

(add-hook *switch-to-buffer-hook* 'sync-filer-to-buffer-directory)

(defun deactive-filer ()
  (when (eq (current-window) (lem-core::frame-leftside-window (current-frame)))
    (next-window))
  (delete-leftside-window))

(define-command filer () ()
  "Open the filer tree view at the project root."
  (if (filer-active-p)
      (deactive-filer)
      (let ((directory (lem-core/commands/project:find-root (buffer-directory))))
        (make-leftside-window (make-filer-buffer directory)))))

(define-command filer-directory () ()
  "Open the filer tree view at this directory."
  (if (filer-active-p)
      (deactive-filer)
      (let ((directory (buffer-directory)))
        (make-leftside-window (make-filer-buffer directory)))))

(define-command filer-at-directory () ()
  "Prompt for a directory and open the filer tree view at this directory."
  (let ((directory (prompt-for-directory "Directory: "
                                         :directory (buffer-directory)
                                         :gravity :cursor
                                         :use-border t)))
    (when (filer-active-p)
      (deactive-filer))
    (make-leftside-window (make-filer-buffer directory))))

(define-command filer-select () ()
  (select (back-to-indentation (current-point))))

(define-command filer-delete-file () ()
  "Delete the selected file in the filer."
  (let ((item (text-property-at (back-to-indentation (current-point)) :item)))
    (unless item
      (editor-error "No file selected")
      (return-from filer-delete-file))
    (let ((file (item-pathname item)))
      (when (and file
                 (prompt-for-y-or-n-p (format nil "Do you really want to delete \"~A\"?" file)))
        (handler-case
            (lem/directory-mode/file:delete-file* file)
          #+sbcl
          (sb-ext:delete-file-error (e)
            (editor-error (format nil "~A" e)))
          #-sbcl
          (error (e)
            (editor-error (format nil "~A" e))))
        (when (not (uiop:file-exists-p file))
          (with-buffer-read-only (current-buffer) nil
            (line-start (current-point))
            (kill-line 1)))))))

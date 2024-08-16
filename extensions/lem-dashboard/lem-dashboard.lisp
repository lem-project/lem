(defpackage :lem-dashboard
  (:use :cl :lem)
  (:export :open-dashboard
           :*dashboard-enable*
           :set-dashboard
           :set-default-dashboard
           :create-centered-string))

(in-package :lem-dashboard)

(defvar *dashboard-buffer-name* "*dashboard*")
(defvar *dashboard-enable* t)

(defun create-centered-string (str width)
  "Creates a 'centered' string by padding with space to fill WIDTH halfway."
  (let* ((padding (max 0 (floor (- width (length str)) 2)))
         (spaces (make-string padding :initial-element #\Space)))
    (concatenate 'string spaces str)))

(defclass dashboard-item ()
  ((item-attribute 
    :initarg :item-attribute 
    :accessor item-attribute 
    :initform 'document-text-attribute
    :documentation "Attribute to use when drawing this item.")
   (top-margin
    :initarg :top-margin 
    :accessor top-margin
    :initform 0
    :documentation "The amount of vertical space (lines) to apply before the item.")
   (bottom-margin
    :initarg :bottom-margin 
    :accessor bottom-margin
    :initform 1
    :documentation "The amount of vertical space (lines) to apply after the item.")
   (keybind 
    :initarg :keybind 
    :accessor keybind 
    :initform nil
    :documentation "The key binding associated with this item, if any.")
   (keybind-command 
    :initarg :keybind-command 
    :accessor keybind-command 
    :initform nil
    :documentation "The command to be executed when the keybind is activated.")
   (action 
    :initarg :action 
    :accessor action 
    :initform nil
    :documentation "Function to execute when <return> is pressed over this item."))
  (:documentation "Base class for all dashboard items."))

(defgeneric draw-dashboard-item (item point)
  (:documentation "Called to draw the dashboard item.")
  (:method :before ((item dashboard-item) point)
    (dotimes (i (top-margin item))
      (insert-character point #\Newline)))
  (:method :after ((item dashboard-item) point)
    (dotimes (i (bottom-margin item))
      (insert-character point #\Newline))))

(defclass dashboard-splash (dashboard-item)
  ((splash-texts :initarg :splash-texts :accessor splash-texts 
                 :initform '("Welcome!"))
   (selected-splash :initarg :selected-splash :accessor selected-splash
                    :initform nil))
  (:documentation "Randomly displays one of SPLASH-TEXTS"))

(defmethod draw-dashboard-item ((item dashboard-splash) point)
  (let ((width (window-width (current-window))))
    (unless (selected-splash item)
      (setf (selected-splash item) 
            (nth (random (length (splash-texts item))) (splash-texts item))))
    (dolist (line (str:lines (selected-splash item)))
      (insert-string point (create-centered-string line width) :attribute (item-attribute item))
      (insert-character point #\Newline))))

(defclass dashboard-url (dashboard-item)
  ((url :initarg :url :accessor url)
   (display-text :initarg :display-text :accessor display-text))
  (:documentation "Creates link/button with DISPLAY-TEXT that opens URL externally."))

(defmethod initialize-instance :after ((item dashboard-url) &key)
  (unless (action item)
    (setf (action item) (lambda () (open-external-file (url item))))))

(defmethod draw-dashboard-item ((item dashboard-url) point)
  (let ((width (window-width (current-window))))
    (lem/button:insert-button point 
                              (create-centered-string (display-text item) width)
                              (lambda () (open-external-file (url item)))
                              :attribute (item-attribute item))))

(defclass dashboard-working-dir (dashboard-item)
  ()
  (:documentation "Prints current working directory"))

(defmethod draw-dashboard-item ((item dashboard-working-dir) point)
  (let ((width (window-width (current-window)))
        (working-dir (format nil "> ~A" (buffer-directory))))
    (insert-string point (create-centered-string working-dir width) :attribute 'document-header4-attribute)
    (insert-character point #\Newline)))

(defclass dashboard-footer-message (dashboard-item)
  ((messages :initarg :messages :accessor messages :initform '("Happy Coding!"))
   (selected-message :initarg :selected-message :accessor selected-message
                     :initform nil))
  (:documentation "Randomly displays one of the passed-in MESSAGES"))

(defmethod draw-dashboard-item ((item dashboard-footer-message) point)
  (let* ((width (window-width (current-window))))
    (unless (selected-message item)
      (setf (selected-message item) 
            (nth (random (length (messages item))) (messages item))))
    (insert-string point 
                   (create-centered-string (format nil "> ~A" (selected-message item)) width) 
                   :attribute (item-attribute item))))

(defclass dashboard-command (dashboard-item)
  ((command :initarg :command :accessor command)
   (display-text :initarg :display-text :accessor display-text))
  (:documentation "Creates a link/button with DISPLAY-TEXT that executes COMMAND."))

(defmethod initialize-instance :after ((item dashboard-command) &key)
  (unless (keybind-command item)
    (setf (keybind-command item) (command item)))
  (unless (action item)
    (setf (action item) (command item))))

(defmethod draw-dashboard-item ((item dashboard-command) point)
  (let ((width (window-width (current-window))))
    (lem/button:insert-button point 
                              (create-centered-string (display-text item) width)
                              (command item)
                              :attribute (item-attribute item))))

(defclass dashboard-recent-projects (dashboard-item)
  ((project-count :initarg :project-count :accessor project-count :initform *dashboard-project-count*))
  (:documentation "Displays a list of recent projects, limited to the last PROJECT-COUNT."))

(defmethod initialize-instance :after ((item dashboard-recent-projects) &key)
  (unless (action item)
  (setf (action item)
        (lambda ()
          (let ((project (string-trim '(#\Space #\Tab) (line-string (current-point)))))
            (when project
              (lem-core/commands/project:project-find-file project)))))))

(define-command move-to-recent-projects () ()
  (let ((point (buffer-point (current-buffer))))
    (buffer-start point)
    (search-forward-regexp point "Recent Projects")
    (line-offset point 2)
    (move-to-beginning-of-line)))

(defmethod draw-dashboard-item ((item dashboard-recent-projects) point)
  (let* ((width (window-width (current-window)))
         (title (format nil "~A Recent Projects (r)" (icon-string "package")))
         (title-line (create-centered-string title width)))
    (insert-string point title-line :attribute 'document-header1-attribute)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (let* ((projects (reverse (lem-core/commands/project:saved-projects)))
           (longest-project (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) projects))
           (max-length (length longest-project))
           (left-padding (floor (- width max-length) 2)))
      (loop for project in (subseq projects 0 (min (project-count item) (length projects)))
            do (insert-string point (format nil "~v@{~A~:*~}" left-padding " "))
               (insert-string point (format nil "~A~%" project))))))

(defclass dashboard-recent-files (dashboard-item)
  ((file-count :initarg :file-count :accessor file-count :initform *dashboard-file-count*))
  (:documentation "Displays a list of recent files, limited to the last FILE-COUNT."))

(defmethod initialize-instance :after ((item dashboard-recent-files) &key)
  (unless (action item)
  (setf (action item)
        (lambda ()
          (let ((file (string-trim '(#\Space #\Tab) (line-string (current-point)))))
            (when file
              (find-file file)))))))

(define-command move-to-recent-files () ()
  (let ((point (buffer-point (current-buffer))))
    (buffer-start point)
    (search-forward-regexp point "Recent Files")
    (line-offset point 2)
    (move-to-beginning-of-line)))

(defmethod draw-dashboard-item ((item dashboard-recent-files) point)
  (let* ((width (window-width (current-window)))
         (title (format nil "~A Recent Files (f)" (icon-string "file-text")))
         (title-line (create-centered-string title width))
         (recent-files (reverse (lem/common/history:history-data-list (lem-core/commands/file:file-history)))))
    (insert-string point title-line :attribute 'document-header1-attribute)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (let* ((longest-file (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) recent-files))
           (max-length (length longest-file))
           (left-padding (floor (- width max-length) 2)))
      (loop for file in (subseq recent-files 0 (min (file-count item) (length recent-files)))
            do (insert-string point (format nil "~v@{~A~:*~}" left-padding " "))
               (insert-string point (format nil "~A~%" file))))))

(define-command dashboard-open-selected-item () ()
  "Execute action on selected dashboard item."
  (let* ((point (current-point))
         (item (text-property-at point :dashboard-item)))
    (when (and item (action item))
      (funcall (action item)))))

(defmethod draw-dashboard-item :around ((item dashboard-item) point)
  "Inserts a :dashboard-item text property in between the starting and ending POINT, useful for tracking."
  (let ((start (copy-point point :temporary)))
    (call-next-method)
    (let ((end (copy-point point :temporary)))
      (put-text-property start end :dashboard-item item)
      (delete-point start)
      (delete-point end))))

(defvar *dashboard-layout* nil
  "List of dashboard-item instances; will be drawn in order.")

(defvar *dashboard-mode-keymap* (make-keymap :name '*dashboard-mode-keymap* :parent *global-keymap*))

(define-major-mode dashboard-mode ()
    (:name "Dashboard"
     :keymap *dashboard-mode-keymap*))

(defun create-dashboard-buffer ()
  "Creates the dashboard buffer."
  (or (get-buffer *dashboard-buffer-name*)
      (make-buffer *dashboard-buffer-name*
                   :enable-undo-p nil
                   :read-only-p t)))

(defun redraw-dashboard ()
  "Redraws the dashboard, clearing and redrawing all content while attempting to maintain point position."
  (let* ((buffer (create-dashboard-buffer))
         (old-line (line-number-at-point (buffer-point buffer)))
         (old-column (point-column (buffer-point buffer))))
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (let ((point (buffer-point buffer)))
      (dolist (item *dashboard-layout*)
        (draw-dashboard-item item point)))
    (setf (buffer-read-only-p buffer) t)
    (change-buffer-mode buffer 'dashboard-mode)
    (move-to-line (buffer-point buffer) old-line)
    (move-to-column (buffer-point buffer) old-column)))

(define-command open-dashboard () ()
  "Opens the dashboard if it doesn't exist, or switches to it if it does."
  (when *dashboard-enable*
    (if (get-buffer *dashboard-buffer-name*)
        (switch-to-buffer (get-buffer *dashboard-buffer-name*))
        (progn
          (redraw-dashboard)
          (switch-to-buffer (get-buffer *dashboard-buffer-name*))))))

(defun setup-dashboard-keymap ()
  "Set dashboard keymap"
  (define-key *dashboard-mode-keymap* "n" 'next-line)
  (define-key *dashboard-mode-keymap* "p" 'previous-line)
  (define-key *dashboard-mode-keymap* "j" 'next-line)
  (define-key *dashboard-mode-keymap* "k" 'previous-line)
  (define-key *dashboard-mode-keymap* "Return" 'dashboard-open-selected-item)
  (dolist (item *dashboard-layout*)
    (when (and (keybind item) (keybind-command item))
      (define-key *dashboard-mode-keymap* (keybind item) (keybind-command item)))))

(defun set-dashboard (dashboard-items)
  "Sets the new dashboard layout to DASHBOARD-ITEMS list and applies new keymap."
  (when dashboard-items
    (setf *dashboard-layout* dashboard-items)
    (setup-dashboard-keymap)
    (when (get-buffer *dashboard-buffer-name*)
      (redraw-dashboard))))

(defun handle-resize (window)
  "Handle resizing; in this case, redraw the dashboard to keep it centered."
  (when (string= (buffer-name (window-buffer window)) *dashboard-buffer-name*)
    (redraw-dashboard)))

(add-hook *after-init-hook* 'open-dashboard)
(add-hook *window-size-change-functions* 'handle-resize)

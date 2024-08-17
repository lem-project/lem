(in-package :lem-dashboard)

;; Splash
(defclass dashboard-splash (dashboard-item)
  ((splash-texts :initarg :splash-texts :accessor splash-texts 
                 :initform '("Welcome!"))
   (selected-splash :initarg :selected-splash :accessor selected-splash
                    :initform nil))
  (:documentation "Randomly displays one of SPLASH-TEXTS")
  (:default-initargs
   :item-attribute 'document-text-attribute))

(defmethod draw-dashboard-item ((item dashboard-splash) point)
  (unless (selected-splash item)
    (setf (selected-splash item) 
          (nth (random (length (splash-texts item))) (splash-texts item))))
  (dolist (line (str:lines (selected-splash item)))
    (insert-string point (create-centered-string line) :attribute (item-attribute item))
    (insert-character point #\Newline)))

;; Url
(defclass dashboard-url (dashboard-item)
  ((url :initarg :url :accessor url)
   (display-text :initarg :display-text :accessor display-text))
  (:documentation "Creates link/button with DISPLAY-TEXT that opens URL externally.")
  (:default-initargs
   :item-attribute 'document-text-attribute))

(defmethod initialize-instance :after ((item dashboard-url) &key)
  (with-slots (keybind-command action) item
    (setf action (lambda () (funcall keybind-command)))))

(defmethod draw-dashboard-item ((item dashboard-url) point)
  (lem/button:insert-button point 
                            (create-centered-string (display-text item))
                            (lambda () (open-external-file (url item)))
                            :attribute (item-attribute item)))

;; Working dir
(defclass dashboard-working-dir (dashboard-item)
  ()
  (:documentation "Prints current working directory")
  (:default-initargs
   :item-attribute 'document-header4-attribute))

(defmethod draw-dashboard-item ((item dashboard-working-dir) point)
  (let ((working-dir (format nil "> ~A" (buffer-directory))))
    (insert-string point (create-centered-string working-dir) :attribute 'document-header4-attribute)
    (insert-character point #\Newline)))

;; Footer
(defclass dashboard-footer-message (dashboard-item)
  ((messages :initarg :messages :accessor messages :initform '("Happy Coding!"))
   (selected-message :initarg :selected-message :accessor selected-message
                     :initform nil))
  (:documentation "Randomly displays one of the passed-in MESSAGES")
  (:default-initargs
   :item-attribute 'document-text-attribute))

(defmethod draw-dashboard-item ((item dashboard-footer-message) point)
  (unless (selected-message item)
    (setf (selected-message item) 
          (nth (random (length (messages item))) (messages item))))
  (insert-string point 
                 (create-centered-string (format nil "> ~A" (selected-message item))) 
                 :attribute (item-attribute item)))

;; Command
(defclass dashboard-command (dashboard-item)
  ((display-text :initarg :display-text :accessor display-text))
  (:documentation "Creates a link/button with DISPLAY-TEXT that executes the keybind-command.")
  (:default-initargs
   :item-attribute 'document-text-attribute))

(defmethod initialize-instance :after ((item dashboard-command) &key)
  (with-slots (keybind-command action) item
    (setf action (lambda () (funcall keybind-command)))))

(defmethod draw-dashboard-item ((item dashboard-command) point)
  (lem/button:insert-button point 
                            (create-centered-string (display-text item))
                            (lambda () (funcall (keybind-command item) item))
                            :attribute (item-attribute item)))

;; Recent projects
(defclass dashboard-recent-projects (dashboard-item)
  ((project-count :initarg :project-count :accessor project-count :initform *dashboard-project-count*))
  (:documentation "Displays a list of recent projects, limited to the last PROJECT-COUNT.")
  (:default-initargs
   :item-attribute 'document-text-attribute
   :action (lambda ()
             (let ((project (string-trim '(#\Space #\Tab) (line-string (current-point)))))
               (when project
                 (lem-core/commands/project:project-find-file project))))))

(define-command move-to-recent-projects () ()
  (let ((point (buffer-point (current-buffer))))
    (buffer-start point)
    (search-forward-regexp point "Recent Projects")
    (line-offset point 2)
    (move-to-beginning-of-line)))

(defmethod draw-dashboard-item ((item dashboard-recent-projects) point)
  (let* ((title (format nil "~A Recent Projects (r)" (icon-string "package")))
         (title-line (create-centered-string title)))
    (insert-string point title-line :attribute 'document-header1-attribute)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (let* ((projects (reverse (lem-core/commands/project:saved-projects)))
           (longest-project (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) projects))
           (max-length (length longest-project))
           (left-padding (floor (- (window-width (current-window)) max-length) 2)))
      (loop for project in (subseq projects 0 (min (project-count item) (length projects)))
            do (insert-string point (format nil "~v@{~A~:*~}" left-padding " "))
               (insert-string point (format nil "~A~%" project))))))

;; Recent files
(defclass dashboard-recent-files (dashboard-item)
  ((file-count :initarg :file-count :accessor file-count :initform *dashboard-file-count*))
  (:documentation "Displays a list of recent files, limited to the last FILE-COUNT.")
  (:default-initargs
   :item-attribute 'document-text-attribute
   :action (lambda ()
             (let ((file (string-trim '(#\Space #\Tab) (line-string (current-point)))))
               (when file
                 (find-file file))))))

(define-command move-to-recent-files () ()
  (let ((point (buffer-point (current-buffer))))
    (buffer-start point)
    (search-forward-regexp point "Recent Files")
    (line-offset point 2)
    (move-to-beginning-of-line)))

(defmethod draw-dashboard-item ((item dashboard-recent-files) point)
  (let* ((title (format nil "~A Recent Files (f)" (icon-string "file-text")))
         (title-line (create-centered-string title))
         (recent-files (reverse (lem/common/history:history-data-list (lem-core/commands/file:file-history)))))
    (insert-string point title-line :attribute 'document-header1-attribute)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (let* ((longest-file (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) recent-files))
           (max-length (length longest-file))
           (left-padding (floor (- (window-width (current-window)) max-length) 2)))
      (loop for file in (subseq recent-files 0 (min (file-count item) (length recent-files)))
            do (insert-string point (format nil "~v@{~A~:*~}" left-padding " "))
               (insert-string point (format nil "~A~%" file))))))

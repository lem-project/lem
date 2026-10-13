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
  (with-slots (url action) item
    (setf action (lambda () (open-external-file url)))))

(defmethod draw-dashboard-item ((item dashboard-url) point)
  (let* ((text (display-text item))
         (padding (max 0 (floor (- (window-width (current-window)) (length text)) 2))))
    (insert-string point (make-string padding :initial-element #\Space) :attribute (item-attribute item))
    (multiple-value-bind (icon-prefix main-text suffix) (split-display-text text)
      (when (plusp (length icon-prefix))
        (insert-string point icon-prefix :attribute (item-attribute item)))
      (button:insert-button point
                            main-text
                            (lambda () (open-external-file (url item)))
                            :attribute (item-attribute item))
      (when (plusp (length suffix))
        (insert-string point suffix :attribute (item-attribute item))))))

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
  ((display-text :initarg :display-text :accessor display-text)
   (action-command :initarg :action-command :accessor action-command))
  (:documentation "Creates a link/button with DISPLAY-TEXT that executes ACTION-COMMAND when clicked.")
  (:default-initargs
   :item-attribute 'document-text-attribute))

(defmethod initialize-instance :after ((item dashboard-command) &key)
  (with-slots (action-command action) item
    (setf action (lambda () (funcall action-command)))))

(defmethod draw-dashboard-item ((item dashboard-command) point)
  (let* ((text (display-text item))
         (padding (max 0 (floor (- (window-width (current-window)) (length text)) 2))))
    (insert-string point (make-string padding :initial-element #\Space) :attribute (item-attribute item))
    (multiple-value-bind (icon-prefix main-text suffix) (split-display-text text)
      (when (plusp (length icon-prefix))
        (insert-string point icon-prefix :attribute (item-attribute item)))
      (button:insert-button point
                            main-text
                            (lambda () (funcall (action-command item)))
                            :attribute (item-attribute item))
      (when (plusp (length suffix))
        (insert-string point suffix :attribute (item-attribute item))))))

;; Recent projects
(defclass dashboard-recent-projects (dashboard-item)
  ((project-count :initarg :project-count :accessor project-count :initform 5))
  (:documentation "Displays a list of recent projects, limited to the last PROJECT-COUNT.")
  (:default-initargs
   :item-attribute 'document-text-attribute
   :action (lambda ()
             (let* ((point (current-point))
                    (project (text-property-at point :project-path)))
               (when project
                 (uiop:with-current-directory (project)
                   (project:project-find-file project)))))))

(define-command dashboard-move-to-recent-projects () ()
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
    (let* ((projects (reverse (project:saved-projects)))
           (display-projects (subseq projects 0 (min (project-count item) (length projects)))))
      (when display-projects
        (let* ((longest-project (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) display-projects))
               (max-length (length longest-project))
               (left-padding (floor (- (window-width (current-window)) max-length) 2)))
          (loop :for project :in display-projects
                :do (let ((line-start (copy-point point :temporary)))
                     (insert-string point (str:fit left-padding " ") :attribute (item-attribute item))
                     (let ((text-start (copy-point point :temporary)))
                       (insert-string point (format nil "~A" project) :attribute (item-attribute item))
                       (let ((text-end (copy-point point :temporary)))
                         (put-text-property line-start text-end :project-path project)
                         (button:apply-button-between-points
                          text-start text-end
                          (let ((p project))
                            (lambda ()
                              (uiop:with-current-directory (p)
                                (project:project-find-file p)))))))
                     (insert-character point #\Newline))))))))
(defun split-display-text (text)
  "Splits TEXT into three parts: a leading icon/glyph prefix, the main text,
and a trailing keymap hint suffix like \" (x)\".
Returns three values: prefix, main-text, suffix."
  (let* (;; Strip leading icons and spaces
         (start (or (position-if (lambda (c)
                                   (not (or (lem/common/character/icon:icon-code-p (char-code c))
                                            (char= c #\Space))))
                                 text)
                    (length text)))
         (prefix (subseq text 0 start))
         (after-prefix (subseq text start))
         ;; Strip trailing " (x)" keymap hint
         (len (length after-prefix))
         (suffix-start (if (and (>= len 4)
                                (char= (char after-prefix (- len 1)) #\))
                                (char= (char after-prefix (- len 3)) #\()
                                (char= (char after-prefix (- len 4)) #\Space))
                           (- len 4)
                           len))
         (main-text (subseq after-prefix 0 suffix-start))
         (suffix (subseq after-prefix suffix-start)))
    (values prefix main-text suffix)))
;; Recent files
(defclass dashboard-recent-files (dashboard-item)
  ((file-count :initarg :file-count :accessor file-count :initform 5))
  (:documentation "Displays a list of recent files, limited to the last FILE-COUNT.")
  (:default-initargs
   :item-attribute 'document-text-attribute
   :action (lambda ()
             (let* ((point (current-point))
                    (file (text-property-at point :file-path)))
               (when file
                 (find-file file))))))

(define-command dashboard-move-to-recent-files () ()
  (let ((point (buffer-point (current-buffer))))
    (buffer-start point)
    (search-forward-regexp point "Recent Files")
    (line-offset point 2)
    (move-to-beginning-of-line)))

(defmethod draw-dashboard-item ((item dashboard-recent-files) point)
  (let* ((title (format nil "~A Recent Files (f)" (icon-string "file-text")))
         (title-line (create-centered-string title))
         (recent-files (lem-core/commands/file:recent-files)))
    (insert-string point title-line :attribute 'document-header1-attribute)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (let ((display-files (subseq recent-files 0 (min (file-count item) (length recent-files)))))
      (when display-files
        (let* ((longest-file (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) display-files))
               (max-length (length longest-file))
               (left-padding (floor (- (window-width (current-window)) max-length) 2)))
          (loop :for file :in display-files
                :do (let ((line-start (copy-point point :temporary)))
                     (insert-string point (str:fit left-padding " ") :attribute (item-attribute item))
                     (let ((text-start (copy-point point :temporary)))
                       (insert-string point (format nil "~A" file) :attribute (item-attribute item))
                       (let ((text-end (copy-point point :temporary)))
                         (put-text-property line-start text-end :file-path file)
                         (button:apply-button-between-points
                          text-start text-end
                          (let ((f file))
                            (lambda ()
                              (find-file f))))))
                     (insert-character point #\Newline))))))))

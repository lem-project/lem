(defpackage :lem-dashboard
  (:use :cl :lem)
  (:export :open-dashboard
           :*dashboard-enable*
           :*dashboard-layout*
           :*dashboard-project-count*
           :*dashboard-file-count*
           :*dashboard-footer-messages*))

(in-package :lem-dashboard)

(defvar *dashboard-buffer-name* "*dashboard*")
(defvar *dashboard-enable* t)

(defvar *dashboard-project-count* 5
  "Number of recent projects to display.")

(defvar *dashboard-file-count* 5
  "Number of recent files to display.")

(defvar *dashboard-footer-messages* '("Happy Coding!"
                                      "ほげほげ"
                                      "<M-x> load-library <RET> tetris"
                                      "Lem Editor Modules? Lisp EMacs? Lem's Not Emacs?")
  "List of random messages to display in the footer.")

(defclass dashboard-item ()
  ((item-attribute :initarg :item-attribute :accessor item-attribute :initform 'document-text-attribute)
   (vertical-padding :initarg :vertical-padding :accessor vertical-padding :initform 1)
   (keybind :initarg :keybind :accessor keybind :initform nil)
   (keybind-command :initarg :keybind-command :accessor keybind-command :initform nil)
   (action :initarg :action :accessor action :initform nil))
  (:documentation "Base class for all dashboard items."))

(defgeneric draw-dashboard-item (item point)
  (:documentation "Called to draw the dashboard item.")
  (:method :after ((item dashboard-item) point)
    (dotimes (i (vertical-padding item))
      (insert-character point #\Newline))))

(defclass dashboard-splash (dashboard-item)
  ((splash-texts :initarg :splash-texts :accessor splash-texts 
                 :initform '("Welcome!")))
  (:documentation "Randomly displays one of SPLASH-TEXTS"))

(defmethod draw-dashboard-item ((item dashboard-splash) point)
  (let ((width (window-width (current-window)))
        (splash-text (nth (random (length (splash-texts item))) (splash-texts item))))
    (dolist (line (str:lines splash-text))
      (insert-string point (create-centered-string line width) :attribute (item-attribute item))
      (insert-character point #\Newline))))

(defclass dashboard-url (dashboard-item)
  ((url :initarg :url :accessor url)
   (display-text :initarg :display-text :accessor display-text))
  (:documentation "Creates link/button with DISPLAY-TEXT that opens URL externally"))

(defmethod initialize-instance :after ((item dashboard-url) &key)
  (setf (action item) (lambda () (open-external-file (url item)))))

(defmethod draw-dashboard-item ((item dashboard-url) point)
  (let ((width (window-width (current-window))))
    (lem/button:insert-button point 
                              (create-centered-string (display-text item) width)
                              (lambda () (open-external-file (url item)))
                              :attribute (item-attribute item))))

(defclass dashboard-footer-message (dashboard-item)
  ((messages :initarg :messages :accessor messages :initform *dashboard-footer-messages*))
  (:documentation "Randomly displays one of the passed-in MESSAGES"))

(defmethod draw-dashboard-item ((item dashboard-footer-message) point)
  (let* ((width (window-width (current-window)))
         (message (nth (random (length (messages item))) (messages item))))
    (insert-string point (create-centered-string (format nil "> ~A" message) width) :attribute (item-attribute item))))

(defclass dashboard-command (dashboard-item)
  ((command :initarg :command :accessor command)
   (display-text :initarg :display-text :accessor display-text))
  (:documentation "Creates a link/button with DISPLAY-TEXT that executes COMMAND"))

(defmethod initialize-instance :after ((item dashboard-command) &key)
  (setf (action item) (command item)))

(defmethod draw-dashboard-item ((item dashboard-command) point)
  (let ((width (window-width (current-window))))
    (lem/button:insert-button point 
                              (create-centered-string (display-text item) width)
                              (command item)
                              :attribute (item-attribute item))))

(defclass dashboard-recent-projects (dashboard-item)
  ((project-count :initarg :project-count :accessor project-count :initform *dashboard-project-count*))
  (:documentation "Displays a list of recent projects, limited to the last PROJECT-COUNT"))

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
  (:documentation "Displays a list of recent files, limited to the last FILE-COUNT"))

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

(defvar *dashboard-layout* nil
  "List of dashboard-item instances; will be drawn in order.")

(setf *dashboard-layout* (list (make-instance 'dashboard-splash 
                                              :item-attribute 'document-metadata-attribute
                                              :splash-texts '("
 -----------------------
 [   Welcome to Lem!   ]
 -----------------------
                
                ,:coodddddoc.             
           ',;cldddddddddddddolc.         
        .,';oddddddddddddddddddddo:       
      ''.,loollooodddddddddddddddddo:     
    .'.............';:lddddddddddddddo'   
   '.................   ,ddddddddddddddc  
  '..................    .Oxddddddddddddc 
 ....................''''oK0xdddddddddddd,
................,ldOKKKKKKKK0xdddxx:,,''',
..............ckKKKKKKKKKKKKK0kO0KKo.     
............'kKKKKKKKKKKKKKKKKKKKKKKKKo   
...........'xdl:;;:O000:                  
.................'k0000:                  
 ...............'k000000                  
 ...............xKKKKKKKk                 
  .............'KKKKKKKKKO'               
   ............,KKKKKKKKKKKko.     .      
    ............xKKKKKKKKKKKKK0OkO;       
      ...........dKKKKKKKKKKKKK;          
         .........,lkKKKKKKK0.            
           ...........;xKKKKK0            
                ...';ckKKKKKK0            
                    .lOKx'                "))
                               (make-instance 'dashboard-recent-projects 
                                              :project-count *dashboard-project-count* 
                                              :vertical-padding 2
                                              :keybind "r"
                                              :keybind-command 'move-to-recent-projects
                                              :action (lambda () 
                                                        (let ((project (line-string (current-point))))
                                                          (when project                                                           
                                                            (lem-core/commands/project:project-find-file project)))))
                               (make-instance 'dashboard-recent-files 
                                              :file-count *dashboard-file-count* 
                                              :vertical-padding 2
                                              :keybind "f"
                                              :keybind-command 'move-to-recent-files
                                              :action (lambda ()
                                                        (let ((file (string-trim '(#\Space #\Tab) 
                                                                                      (line-string (current-point)))))
                                                          (when file
                                                            (find-file file)))))
                               (make-instance 'dashboard-command 
                                              :display-text "New Lisp Scratch Buffer (l)" 
                                              :command #'lem-lisp-mode/internal:lisp-scratch 
                                              :item-attribute 'document-header2-attribute
                                              :keybind "l"
                                              :keybind-command 'lem-lisp-mode/internal:lisp-scratch
                                              :vertical-padding 2)
                               (make-instance 'dashboard-url 
                                              :display-text "Getting Started (s)" 
                                              :url "https://lem-project.github.io/usage/usage/"
                                              :item-attribute 'document-header3-attribute
                                              :keybind "s"
                                              :keybind-command 'open-lem-docs)
                               (make-instance 'dashboard-url 
                                              :display-text "GitHub (g)" 
                                              :url "https://github.com/lem-project/lem"
                                              :item-attribute 'document-header3-attribute
                                              :keybind "g"
                                              :keybind-command 'open-lem-github
                                              :vertical-padding 2)
                               (make-instance 'dashboard-footer-message 
                                              :item-attribute 'document-blockquote-attribute
                                              :messages *dashboard-footer-messages*)))

(define-command open-lem-docs () ()
  (open-external-file "https://lem-project.github.io/usage/usage/"))

(define-command open-lem-github () ()
  (open-external-file "https://github.com/lem-project/lem"))

(defvar *dashboard-mode-keymap* (make-keymap :name '*dashboard-mode-keymap*
                                             :parent *global-keymap*))

(define-major-mode dashboard-mode ()
    (:name "Dashboard"
     :keymap *dashboard-mode-keymap*))

(defun create-dashboard-buffer ()
  "Creates the dashboard buffer."
  (or (get-buffer *dashboard-buffer-name*)
      (make-buffer *dashboard-buffer-name*
                   :enable-undo-p nil
                   :read-only-p t)))

(define-command dashboard-open-selected-item () ()
  "Execute action on selected dashboard item."
  (let* ((point (current-point))
         (item (text-property-at point :dashboard-item)))
    (when (and item (action item))
      (funcall (action item)))))

(defmethod draw-dashboard-item :around ((item dashboard-item) point)
  "Inserts a :dashboard-item text property in between the starting and ending POINT."
  (let ((start (copy-point point :temporary)))
    (call-next-method)
    (let ((end (copy-point point :temporary)))
      (put-text-property start end :dashboard-item item)
      (delete-point start)
      (delete-point end))))

(define-command open-dashboard () ()
  "Opens the dashboard, overwriting any existing one"
  (when *dashboard-enable*
    (setup-dashboard-keymap)
    (let ((buffer (create-dashboard-buffer)))
      (switch-to-buffer buffer)
      (setf (buffer-read-only-p buffer) nil)
      (erase-buffer buffer)
      (let ((point (buffer-point buffer)))
        (dolist (item *dashboard-layout*)
          (draw-dashboard-item item point)))
      (buffer-start (buffer-point buffer))
      (setf (buffer-read-only-p buffer) t)
      (change-buffer-mode buffer 'dashboard-mode))))

(defun setup-dashboard-keymap ()
  ;; Add standard navigation keys
  (define-key *dashboard-mode-keymap* "n" 'next-line)
  (define-key *dashboard-mode-keymap* "p" 'previous-line)
  (define-key *dashboard-mode-keymap* "j" 'next-line)
  (define-key *dashboard-mode-keymap* "k" 'previous-line)
  (define-key *dashboard-mode-keymap* "Return" 'dashboard-open-selected-item)
  
  ;; Add custom keybinds from the layout
  (dolist (item *dashboard-layout*)
    (when (and (keybind item) (keybind-command item))
      (define-key *dashboard-mode-keymap* (keybind item) (keybind-command item)))))

(defun create-centered-string (str width)
  "Creates a 'centered' string by padding with space to fill WIDTH halfway."
  (let* ((padding (max 0 (floor (- width (length str)) 2)))
         (spaces (make-string padding :initial-element #\Space)))
    (concatenate 'string spaces str)))

(defun handle-resize (window) 
  (when (string= (buffer-name (window-buffer window)) *dashboard-buffer-name*)
    (open-dashboard)))

(add-hook *after-init-hook* 'open-dashboard)
(add-hook *window-size-change-functions* 'handle-resize)
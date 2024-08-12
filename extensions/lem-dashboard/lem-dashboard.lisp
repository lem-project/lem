(defpackage :lem-dashboard
  (:use :cl :lem)
  (:export :show-dashboard
           :*dashboard-project-count*
           :*dashboard-file-count*))

(in-package :lem-dashboard)

(defvar *dashboard-buffer-name* "*dashboard*")

(defun create-centered-string (str width)
  (let* ((padding (max 0 (floor (- width (length str)) 2)))
         (spaces (make-string padding :initial-element #\Space)))
    (concatenate 'string spaces str)))

(defun insert-splash-screen (point)
  (let ((width (window-width (current-window)))
        (splash-text '("
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
                    .lOKx'                ")))
    (dolist (line (str:lines (car splash-text)))
      (insert-string point (create-centered-string line width) :attribute 'document-metadata-attribute)
      (insert-character point #\Newline))))

(defun insert-working-dir (point)
  (let ((width (window-width (current-window)))
        (working-dir (format nil "> ~A" (buffer-directory))))
    (insert-string point (create-centered-string working-dir width) :attribute 'document-header4-attribute)
    (insert-character point #\Newline)))

(defvar *dashboard-project-count* 5)

(defun insert-recent-projects (point)
  (let* ((width (window-width (current-window)))
         (title (format nil "~A Recent Projects (r)" (icon-string "package")))
         (title-line (create-centered-string title width)))
    (insert-string point title-line :attribute 'document-header1-attribute)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (let* ((longest-project (reduce #'(lambda (a b) (if (> (length a) (length b)) a b))
                                    (lem-core/commands/project:saved-projects)))
           (max-length (length longest-project))
           (left-padding (floor (- width max-length) 2)))
      (loop for project in (subseq (lem-core/commands/project:saved-projects) 
                                   0 
                                   (min *dashboard-project-count* (length (lem-core/commands/project:saved-projects))))
            do (insert-string point (format nil "~v@{~A~:*~}" left-padding " "))
               (insert-string point (format nil "~A~%" project))))))

(defvar *dashboard-file-count* 5)

(defun insert-recent-files (point)
  (let* ((width (window-width (current-window)))
         (title (format nil "~A Recent Files (f)" (icon-string "file-text")))
         (title-line (create-centered-string title width))
         (recent-files (lem/common/history:history-data-list (lem-core/commands/file:file-history))))
    (insert-string point title-line :attribute 'document-header1-attribute)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (let* ((longest-file (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) recent-files))
           (max-length (length longest-file))
           (left-padding (floor (- width max-length) 2)))
      (loop for file in (subseq recent-files 0 (min *dashboard-file-count* (length recent-files)))
            do (insert-string point (format nil "~v@{~A~:*~}" left-padding " "))
               (insert-string point (format nil "~A~%" file))))))

(define-command open-lem-docs () ()
  (open-external-file "https://lem-project.github.io/usage/usage/"))

(define-command open-lem-github () ()
  (open-external-file "https://github.com/lem-project/lem"))

(defun insert-misc-shortcuts (point)
  (let* ((width (window-width (current-window)))
         (scratch-text (format nil "~A New Lisp Scratch Buffer (l)" (icon-string "lisp")))
         (getting-started-text (format nil "~A Getting Started (s)" (icon-string "markdown")))
         (github-text (format nil "~A GitHub (g)" (icon-string "file-text"))))
    (lem/button:insert-button point 
                              (create-centered-string scratch-text width)
                              #'lem-lisp-mode/internal::lisp-scratch
                              :attribute 'document-header2-attribute)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (lem/button:insert-button point 
                              (create-centered-string getting-started-text width)
                              #'open-lem-docs
                              :attribute 'document-header3-attribute)
    (insert-character point #\Newline)
    (lem/button:insert-button point 
                              (create-centered-string github-text width)
                              #'open-lem-github
                              :attribute 'document-header3-attribute)))

(defun create-dashboard-buffer ()
  (make-buffer *dashboard-buffer-name*
               :enable-undo-p nil
               :read-only-p t))

(define-major-mode dashboard-mode ()
    (:name "Dashboard"
     :keymap *dashboard-mode-keymap*))

(define-command show-dashboard () ()
  (let ((buffer (create-dashboard-buffer)))
    (switch-to-buffer buffer)
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (let ((point (buffer-point buffer)))
      (insert-splash-screen point)
      (insert-character point #\Newline)
      (insert-character point #\Newline)
      (insert-working-dir point)
      (insert-character point #\Newline)
      (insert-character point #\Newline)
      (insert-recent-projects point)
      (insert-character point #\Newline)
      (insert-character point #\Newline)
      (insert-recent-files point)
      (insert-character point #\Newline)
      (insert-character point #\Newline)
      (insert-misc-shortcuts point))
    (buffer-start (buffer-point buffer))
    (setf (buffer-read-only-p buffer) t)
    (change-buffer-mode buffer 'dashboard-mode)))

(define-command move-to-recent-projects () ()
  (let ((point (buffer-point (current-buffer))))
    (buffer-start point)
    (search-forward-regexp point "Recent Projects")
    (line-offset point 2)
    (move-to-beginning-of-line)))

(define-command move-to-recent-files () ()
  (let ((point (buffer-point (current-buffer))))
    (buffer-start point)
    (search-forward-regexp point "Recent Files")
    (line-offset point 2)
    (move-to-beginning-of-line)))

(define-command open-selected-project () () 
  (let* ((point (buffer-point (current-buffer)))
         (line (line-string point))
         (project-path (string-trim '(#\Space #\Tab) line)))
    (when (uiop:directory-exists-p project-path)
      (uiop:with-current-directory (project-path))
      (let ((filename (prompt-for-files-recursively)))
        (alexandria:when-let (buffer (execute-find-file *find-file-executor*
                                                        (lem-core/commands/file:get-file-mode filename)
                                                        filename))
          (when buffer
            (switch-to-buffer buffer t nil))))
      (delete-buffer (get-buffer *dashboard-buffer-name*)))))

(define-command open-selected-file () () 
  (let* ((point (buffer-point (current-buffer)))
         (line (line-string point))
         (file-path (string-trim '(#\Space #\Tab) line)))
    (when (uiop:file-exists-p file-path)
      (find-file file-path))))

(defun in-recent-files-section-p (point)
  (let* ((current-line (line-number-at-point point))
         (files-section-start nil)
         (temp-point (copy-point point :temporary)))
    (buffer-start temp-point)
    (when (search-forward-regexp temp-point "Recent Files")
      (setf files-section-start (+ (line-number-at-point temp-point) 2)))
    (delete-point temp-point)
    (when files-section-start
      (>= current-line files-section-start))))

(define-command open-selected-item () ()
  (let ((point (buffer-point (current-buffer))))
    (if (in-recent-files-section-p point)
        (open-selected-file)
        (open-selected-project))))

(define-key *dashboard-mode-keymap* "r" 'move-to-recent-projects)
(define-key *dashboard-mode-keymap* "f" 'move-to-recent-files)
(define-key *dashboard-mode-keymap* "l" 'lem-lisp-mode/internal::lisp-scratch)
(define-key *dashboard-mode-keymap* "q" 'lem-lisp-mode/internal::lisp-scratch)
(define-key *dashboard-mode-keymap* "s" 'open-lem-docs)
(define-key *dashboard-mode-keymap* "g" 'open-lem-github)
(define-key *dashboard-mode-keymap* "n" 'next-line)
(define-key *dashboard-mode-keymap* "p" 'previous-line)
(define-key *dashboard-mode-keymap* "j" 'next-line)
(define-key *dashboard-mode-keymap* "k" 'previous-line)
(define-key *dashboard-mode-keymap* "Return" 'open-selected-item)
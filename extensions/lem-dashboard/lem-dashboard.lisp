(defpackage #:lem-dashboard
  (:use :cl :lem)
  (:export #:show-dashboard))

(in-package :lem-dashboard)

(defvar *dashboard-buffer-name* "*dashboard*")

(defun create-centered-string (str width)
  (let* ((padding (max 0 (floor (- width (length str)) 2)))
         (spaces (make-string padding :initial-element #\Space)))
    (concatenate 'string spaces str)))

(defun insert-splash-screen (point)
  (let ((width (window-width (current-window)))
        (splash-text '("

 Welcome to Lem!
                
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
      (insert-string point (create-centered-string line width))
      (insert-character point #\Newline))))

(defun insert-recent-projects (point)
  (let* ((width (window-width (current-window)))
         (title "Recent Projects")
         (title-line (create-centered-string title width)))
    (insert-string point title-line)
    (insert-character point #\Newline)
    (insert-character point #\Newline)
    (let* ((longest-project (reduce #'(lambda (a b) (if (> (length a) (length b)) a b))
                                    (lem-core/commands/project:saved-projects)))
           (max-length (length longest-project))
           (left-padding (floor (- width max-length) 2)))
      (loop for project in (lem-core/commands/project:saved-projects)
            do (insert-string point (format nil "~v@{~A~:*~}" left-padding " "))
               (insert-string point (format nil "~A~%" project))))))

(defun create-dashboard-buffer ()
  (make-buffer *dashboard-buffer-name*
               :enable-undo-p nil
               :read-only-p t))

(define-major-mode dashboard-mode ()
    (:name "Dashboard"
     :keymap *dashboard-mode-keymap*))

(define-key *dashboard-mode-keymap* "p" 'move-to-recent-projects)

(define-command show-dashboard () ()
  (let ((buffer (create-dashboard-buffer)))
    (switch-to-buffer buffer)
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (let ((point (buffer-point buffer)))
      (insert-splash-screen point)
      (insert-character point #\Newline)
      (insert-character point #\Newline)
      (insert-recent-projects point))
    (buffer-start (buffer-point buffer))
    (setf (buffer-read-only-p buffer) t)
    (change-buffer-mode buffer 'dashboard-mode)))

(define-command move-to-recent-projects () ()
  (let ((point (buffer-point (current-buffer))))
    (buffer-start point)
    (search-forward-regexp point "Recent Projects")
    (line-offset point 2)))
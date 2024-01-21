(defpackage #:lem-welcome 
  (:use :cl :lem))
(in-package :lem-welcome)

(defparameter splash-width 45)
(defparameter splash-content 
"
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
                    .lOKx'                ")

(defun display-welcome ()
  (with-open-stream (stream (make-buffer-output-stream (buffer-start-point (current-buffer))))
    (loop :with prefix := (/ (- (window-width (current-window)) splash-width) 2)
          :for line :in (str:lines splash-content)
          :do (format stream "~v@{~a~:*~}" prefix " ")
          :do (format stream "~a~%" line)))
  (lem-vi-mode/commands:vi-goto-first-line))

(add-hook *after-init-hook* #'display-welcome)

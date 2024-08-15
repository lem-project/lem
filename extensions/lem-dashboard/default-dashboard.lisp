(in-package :lem-dashboard)

(defvar *dashboard-project-count* 5
  "Number of recent projects to display.")

(defvar *dashboard-file-count* 5
  "Number of recent files to display.")

(defvar *dashboard-footer-messages* '("Happy Coding!"
                                      "ほげほげ"
                                      "In Lisp we trust, for bugs we adjust"
                                      "Read, Evaluate, Print, Love"
                                      "May your parentheses always be balanced"
                                      "(setf productivity 'high)"
                                      "<M-x> load-library <RET> tetris"
                                      "Lem Editor Modules? Lisp EMacs? Lem's Not Emacs?"
                                      "(cons 'fun 'programming)")
  "List of random messages to display in the footer.")

(define-command open-lem-docs () ()
  (open-external-file "https://lem-project.github.io/usage/usage/"))

(define-command open-lem-github () ()
  (open-external-file "https://github.com/lem-project/lem"))

(set-dashboard (list (make-instance 'dashboard-splash
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
                     (make-instance 'dashboard-working-dir)
                     (make-instance 'dashboard-recent-projects 
                                    :project-count *dashboard-project-count* 
                                    :vertical-padding 1
                                    :keybind "r"
                                    :keybind-command 'move-to-recent-projects
                                    :action (lambda () 
                                              (let ((project (line-string (current-point))))
                                                (when project                                                           
                                                  (lem-core/commands/project:project-find-file project)))))
                     (make-instance 'dashboard-recent-files 
                                    :file-count *dashboard-file-count* 
                                    :vertical-padding 1
                                    :keybind "f"
                                    :keybind-command 'move-to-recent-files
                                    :action (lambda ()
                                              (let ((file (string-trim '(#\Space #\Tab) 
                                                                       (line-string (current-point)))))
                                                (when file
                                                  (find-file file)))))
                     (make-instance 'dashboard-command
                                    :display-text (format nil "~A New Lisp Scratch Buffer (l)" (icon-string "lisp"))
                                    :command #'lem-lisp-mode/internal:lisp-scratch 
                                    :item-attribute 'document-header2-attribute
                                    :keybind "l"
                                    :keybind-command 'lem-lisp-mode/internal:lisp-scratch
                                    :vertical-padding 2)
                     (make-instance 'dashboard-url 
                                    :display-text (format nil "~A Getting Started (s)" (icon-string "markdown"))
                                    :url "https://lem-project.github.io/usage/usage/"
                                    :item-attribute 'document-header3-attribute
                                    :keybind "s"
                                    :keybind-command 'open-lem-docs)
                     (make-instance 'dashboard-url
                                    :display-text (format nil "~A GitHub (g)" (icon-string "file-text"))
                                    :url "https://github.com/lem-project/lem"
                                    :item-attribute 'document-header3-attribute
                                    :keybind "g"
                                    :keybind-command 'open-lem-github
                                    :vertical-padding 2)
                     (make-instance 'dashboard-footer-message 
                                    :item-attribute 'document-blockquote-attribute
                                    :messages *dashboard-footer-messages*)))
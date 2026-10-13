(uiop:define-package :lem/legit-clone
  (:use :cl
        :lem)
  (:export
   :git-clone-and-open))

(in-package :lem/legit-clone)

(defun run-git-clone (url destination)
  "Clone the project given by URL to DESTINATION.

  Run the command synchronously with uiop:run-program, return 3 values: output (string), error-output (string), status code (integer). To handle errors and show them in a Lem popup message, use lem/legit:run-function."
  (let ((cmd (format nil "cd ~a && git clone ~a" destination url)))
    (uiop:run-program cmd
                      :output :string
                      :error-output :string
                      :ignore-error-status t)))

(defun cleanup-url (url)
  "Remove an optional .git suffix from the URL."
  (if (str:ends-with-p ".git" url)
      (subseq url 0 (- (length url) 4))
      url))

(defun cloned-directory (url destination)
  "Return the directory (string) where we cloned the new project."
  (let ((project-name (alexandria:last-elt (str:split "/" (cleanup-url url)))))
    (str:concat (str:ensure-suffix "/" destination) project-name)))

(define-command git-clone-and-open (url destination) ((:string "URL: ") (:file "Destination: "))
  "Clone a project at URL to DESTINATION and open the destination directory."
  (lem/legit:run-function
   (lambda ()
     (run-git-clone url destination))

   :on-success
   (lambda ()
     (let ((new-dir (cloned-directory url destination)))
       (find-file new-dir)))))

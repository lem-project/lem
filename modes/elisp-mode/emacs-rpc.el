(require 'cl-lib)
;; ;;https://github.com/jcaw/porthole

(require 'porthole)


(cl-defun lem-elisp-start-server (&key
				  (name "lem-elisp-server")
				  (port 55486)
				  (username "lem")
				  (password "lem"))
  "Start the porthole server."
  (porthole-start-server-advanced
   name
   :port port
   :username username
   :password password
   :publish-port nil
   :publish-username t
   :publish-password t))


(cl-defun lem-get-completion ()
  "Return a list of all the local-symbols."
  (cl-coerce (elisp--completion-local-symbols) 'list))

(cl-defun lem-symbol-location (symbol)
  "Return a cons of (file . absolute-position) of SYMBOL."
  (let ((symbol-intern (intern-soft symbol))
	symbol-info)
    (cond
     ((functionp symbol-intern)
      (setf symbol-info
	    (find-definition-noselect symbol-intern nil)))
     ((boundp symbol-intern)
      (setf symbol-info
	    (find-definition-noselect symbol-intern 'defvar)))
     (t
      (cl-return-from lem-symbol-location
	nil)))
    (list (buffer-file-name (car symbol-info))
	  (cdr symbol-info))))

(cl-defun lem-symbol-documentation (symbol)
  (let ((symbol-intern (intern-soft symbol)))
    (cond
     ((functionp symbol-intern)
      (documentation symbol-intern))
     ((boundp symbol-intern)
      (documentation-property symbol-intern 'variable-documentation))
     (t
      (cl-return-from lem-symbol-documentation
	nil)))))

(defvar *lem-emacs-export-functions*
  '(lem-get-completion
    lem-symbol-location
    lem-symbol-documentation))


(cl-defun lem-elisp-export-functions (&key
				      (server "lem-elisp-server")
				      (functions *lem-emacs-export-functions*))
  (mapcar (lambda (fname)
	    (porthole-expose-function server fname))
	  functions))

t
;; (lem-symbol-location "goto-char")
;; (lem-symbol-location "pi")
;; (lem-symbol-location "asdasd")



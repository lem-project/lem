(in-package :lem)

(export '(color-theme-names
          define-color-theme
          load-theme
          display-background-mode
          foreground
          background))

(defstruct color-theme
  specs
  parent)

(defvar *color-themes* (make-hash-table :test 'equal))

(defun find-color-theme (name)
  (gethash name *color-themes*))

(defun all-color-themes ()
  (alexandria:hash-table-keys *color-themes*))

(defmacro define-color-theme (name (&optional (parent nil parentp)) &body specs)
  (when parentp
    (check-type parent string))
  `(progn
     ,@(when parentp
         `((unless (find-color-theme ,parent)
             (error ,(format nil "undefined color theme: ~A" parent)))))
     (setf (gethash ,name *color-themes*)
           (make-color-theme
            :specs (list ,@(mapcar (lambda (spec)
                                     `(list ',(car spec)
                                            ,@(cdr spec)))
                                   specs))
            :parent ,parent))))

(defun inherit-load-theme (theme spec-table)
  (when (color-theme-parent theme)
    (inherit-load-theme (find-color-theme (color-theme-parent theme))
                        spec-table))
  (loop :for (name . args) :in (color-theme-specs theme)
        :do (setf (gethash name spec-table) args)))

(defun load-theme-1 (name)
  (let ((theme (find-color-theme name)))
    (unless theme
      (error "undefined color theme: ~A" name))
    (clear-all-attribute-cache)
    (let ((spec-table (make-hash-table)))
      (inherit-load-theme theme spec-table)
      (maphash (lambda (name args)
                 (case name
                   ((display-background-mode)
                    (set-display-background-mode
                     (case (first args)
                       ((:light :dark) (first args))
                       (otherwise nil))))
                   ((foreground)
                    (apply #'set-foreground args))
                   ((background)
                    (apply #'set-background args))
                   (otherwise
                    (apply #'set-attribute name args))))
               spec-table))))

(define-command load-theme (name)
    ((list (prompt-for-line "Color theme: "
                            nil
                            (lambda (string)
                              (completion string (all-color-themes)))
                            'find-color-theme
                            'mh-color-theme)))
  (when (find-color-theme name)
    (load-theme-1 name)
    (message nil)
    (redraw-display t)))

(define-color-theme "emacs-light" ()
  (display-background-mode :light)
  ;; (foreground "#000000")
  ;; (background "#FFFFFF")
  (minibuffer-prompt-attribute :foreground "blue" :bold-p t)
  (region :foreground nil :background "#eedc82")
  (modeline :background "#bbbbbb" :foreground "black")
  (modeline-inactive :background "#bbbbbb" :foreground "#777777")
  (completion-attribute :foreground "white" :background "blue")
  (non-focus-completion-attribute :foreground "black" :background "gray")
  (syntax-string-attribute :foreground "RosyBrown")
  (syntax-comment-attribute :foreground "firebrick")
  (syntax-keyword-attribute :foreground "purple")
  (syntax-constant-attribute :foreground "#ff00ff")
  (syntax-function-name-attribute :foreground "blue")
  (syntax-variable-attribute :foreground "darkgoldenrod")
  (syntax-type-attribute :foreground "forestgreen"))

(define-color-theme "emacs-dark" ("emacs-light")
  (display-background-mode :dark)
  ;; (foreground "#FFFFFF")
  ;; (background "#000000")
  (minibuffer-prompt-attribute :foreground "cyan" :bold-p t)
  (region :foreground nil :background "blue")
  (completion-attribute :foreground "white" :background "blue")
  (non-focus-completion-attribute :foreground "black" :background "gray")
  (syntax-string-attribute :foreground "light salmon")
  (syntax-comment-attribute :foreground "chocolate1")
  (syntax-keyword-attribute :foreground "cyan1")
  (syntax-constant-attribute :foreground "LightSteelBlue")
  (syntax-function-name-attribute :foreground "LightSkyBlue")
  (syntax-variable-attribute :foreground "LightGoldenrod")
  (syntax-type-attribute :foreground "PaleGreen"))

(in-package :lem)

(export '(color-theme-names
          define-color-theme
          load-theme))

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
    (let ((spec-table (make-hash-table)))
      (inherit-load-theme theme spec-table)
      (maphash (lambda (name args)
                 (case name
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
  (foreground "#000000")
  (background "#FFFFFF")
  (minibuffer-prompt-attribute :foreground "blue" :bold-p t)
  (region :background "#eedc82")
  (modeline :background "#bbbbbb" :foreground "black")
  (modeline-inactive :background "#bbbbbb" :foreground "#777777")
  (completion-attribute :foreground "#e5e5e5" :background "#0000FF")
  (non-focus-completion-attribute :foreground "black" :background "#aaaaaa")
  (syntax-string-attribute :foreground "#8B2252")
  (syntax-comment-attribute :foreground "#cd0000")
  (syntax-keyword-attribute :foreground "#C000A0")
  (syntax-constant-attribute :foreground "#ff00ff")
  (syntax-function-name-attribute :foreground "#0000ff")
  (syntax-variable-attribute :foreground "#8D5232")
  (syntax-type-attribute :foreground "#00875f"))

(define-color-theme "emacs-dark" ("emacs-light")
  (foreground "#FFFFFF")
  (background "#000000")
  (minibuffer-prompt-attribute :foreground "cyan" :bold-p t)
  (region :background "blue")
  (syntax-string-attribute :foreground "light salmon")
  (syntax-comment-attribute :foreground "chocolate1")
  (syntax-keyword-attribute :foreground "cyan1")
  (syntax-constant-attribute :foreground "LightSteelBlue")
  (syntax-function-name-attribute :foreground "LightSkyBlue")
  (syntax-variable-attribute :foreground "LightGoldenrod")
  (syntax-type-attribute :foreground "PaleGreen"))

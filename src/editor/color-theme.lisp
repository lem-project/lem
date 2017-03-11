(in-package :lem)

(export '(color-theme-names
          define-color-theme
          load-theme))

(defvar *color-themes* (make-hash-table :test 'equal))

(defun all-color-themes ()
  (alexandria:hash-table-keys *color-themes*))

(defmacro define-color-theme (name () &body specs)
  `(setf (gethash ,name *color-themes*) ',specs))

(defun load-theme (name)
  (let ((theme (gethash name *color-themes*)))
    (unless theme
      (error "undefined color theme: ~A" name))
    (loop :for (name . args) :in theme
          :do (case name
                ((foreground)
                 (apply #'set-foreground args))
                ((background)
                 (apply #'set-background args))
                (otherwise
                 (apply #'set-attribute name args))))))

(define-color-theme "default" ()
  ;;(foreground "#000000")
  ;;(background "#FFFFFF")
  (region :background "#eedc82")
  (modeline :background "#bbbbbb" :foreground "black")
  (modeline-inactive :background "#bbbbbb" :foreground "#777777")
  (completion-attribute :foreground "white" :background "#0000FF")
  (non-focus-completion-attribute :foreground "black" :background "#aaaaaa")
  (syntax-string-attribute :foreground "#8B2252")
  (syntax-comment-attribute :foreground "red")
  (syntax-keyword-attribute :foreground "#C000A0")
  (syntax-constant-attribute :foreground "color-201")
  (syntax-function-name-attribute :foreground "color-21")
  (syntax-variable-attribute :foreground "#8D5232")
  (syntax-type-attribute :foreground "color-29"))

(add-hook *after-init-hook* (lambda () (load-theme "default")))

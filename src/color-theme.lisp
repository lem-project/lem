(in-package :lem)

(defvar *current-theme*)

(defun current-theme ()
  *current-theme*)

(defun (setf current-theme) (theme)
  (setf *current-theme* theme))

(defvar *hot-reload-color-theme* nil)

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
            :parent ,parent))
     (when *hot-reload-color-theme*
       (load-theme ,name))))

(defun inherit-load-theme (theme spec-table)
  (when (color-theme-parent theme)
    (inherit-load-theme (find-color-theme (color-theme-parent theme))
                        spec-table))
  (loop :for (name . args) :in (color-theme-specs theme)
        :do (setf (gethash name spec-table) args)))

(defun apply-theme (theme)
  (setf *inactive-window-background-color* nil)
  (clear-all-attribute-cache)
  (let ((spec-table (make-hash-table)))
    (inherit-load-theme theme spec-table)
    (maphash (lambda (name args)
               (case name
                 ((:display-background-mode)
                  (set-display-background-mode
                   (case (first args)
                     ((:light :dark) (first args))
                     (otherwise nil))))
                 ((:foreground)
                  (apply #'set-foreground args))
                 ((:background)
                  (apply #'set-background args))
                 ((:inactive-window-background)
                  (setf *inactive-window-background-color* (first args)))
                 (otherwise
                  (apply #'set-attribute name args))))
             spec-table)))

(define-command load-theme (name)
    ((prompt-for-string "Color theme: "
                        :completion-function (lambda (string)
                                               (completion string (all-color-themes)))
                        :test-function 'find-color-theme
                        :history-symbol 'mh-color-theme))
  (let ((theme (find-color-theme name)))
    (unless theme
      (editor-error "undefined color theme: ~A" name))
    (apply-theme theme)
    (message nil)
    (redraw-display t)
    (setf (current-theme) name)
    (setf (config :color-theme) (current-theme))))

(defun background-color ()
  (second (assoc :background (color-theme-specs (find-color-theme *current-theme*)))))

(defun foreground-color ()
  (second (assoc :foreground (color-theme-specs (find-color-theme *current-theme*)))))

(define-major-mode color-theme-selector-mode ()
    (:name "Themes"
     :keymap *color-theme-selector-keymap*))

(define-key *color-theme-selector-keymap* "Return" 'color-theme-selector-select)

(define-command color-theme-selector-select () ()
  (with-point ((point (current-point)))
    (line-start point)
    (let ((theme (text-property-at point 'theme)))
      (load-theme theme))))

(define-command list-color-themes () ()
  (let* ((buffer (make-buffer "*Color Themes*"))
         (point (buffer-point buffer))
         (dark-themes '())
         (light-themes '()))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (dolist (name (all-color-themes))
        (let ((theme (find-color-theme name)))
          (if (eq :dark (second (assoc :display-background-mode (color-theme-specs theme))))
              (push (cons name theme) dark-themes)
              (push (cons name theme) light-themes))))
      (loop :for (name . theme) :in (append dark-themes light-themes)
            :do (insert-string
                 point name
                 :attribute (make-attribute
                             :foreground (second (assoc :foreground (color-theme-specs theme)))
                             :background (second (assoc :background (color-theme-specs theme))))
                 'theme name)
                (insert-character point #\newline)))
    (buffer-start point)
    (setf (buffer-read-only-p buffer) t)
    (switch-to-buffer buffer)
    (change-buffer-mode buffer 'color-theme-selector-mode)))


(defun initialize-color-theme ()
  (load-theme (config :color-theme "decaf")))

(add-hook *before-init-hook* 'initialize-color-theme)

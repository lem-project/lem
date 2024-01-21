(defpackage :lem-color-preview
  (:use :cl :lem))
(in-package :lem-color-preview)

(define-minor-mode color-preview
    (:name "Color Preview"
     :hide-from-modeline t
     :enable-hook 'enable
     :disable-hook 'disable))

(defun enable ()
  (add-hook (variable-value 'after-syntax-scan-hook :buffer (current-buffer))
            'scan-color-in-region))

(defun disable ()
  (remove-hook (variable-value 'after-syntax-scan-hook :buffer (current-buffer))
               'scan-color-in-region))

(defun scan-color-in-region (start end)
  (with-point ((point start))
    (loop :while (point< point end)
          :do (cond ((syntax-string-quote-char-p (character-at point))
                     (with-point ((string-start point))
                       (unless (form-offset point 1)
                         (return))
                       (with-point ((string-end point))
                         (character-offset string-end -1)
                         (character-offset string-start 1)
                         (let ((color (points-to-string string-start string-end)))
                           (when (parse-color color)
                             (character-offset string-end 1)
                             (character-offset string-start -1)
                             (put-text-property string-start
                                                string-end
                                                :attribute (make-attribute :background color
                                                                           :foreground (if (light-color-p color) "#000000" "#ffffff"))))))))
                    (t
                     (character-offset point 1))))))

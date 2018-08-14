(defpackage :lem-trailing-spaces
  (:use :cl :lem)
  (:export))
(in-package :lem-trailing-spaces)

(define-attribute space-attribute
  (t :background "cyan"))

(defvar *space-attribute* (make-attribute :background "cyan"))

(define-minor-mode trailing-spaces
    (:name "Trailing Spaces"
     :global t
     :enable-hook 'enable
     :disable-hook 'disable))

(defun scan-trailing-spaces (start end)
  (with-point ((p1 start)
               (p2 start)
               (end end))
    (line-start p1)
    (line-end end)
    (loop :while (point< p1 end)
          :do (line-end p1)
              (move-point p2 p1)
              (skip-whitespace-backward p1 t)
              (put-text-property p1 p2 :attribute 'space-attribute)
              (unless (line-offset p1 1)
                (return)))))

(defun enable ()
  (add-hook (variable-value 'after-syntax-scan-hook :buffer (current-buffer))
            'scan-trailing-spaces))

(defun disable ()
  (add-hook (variable-value 'after-syntax-scan-hook :buffer (current-buffer))
            'scan-trailing-spaces))

(defpackage :lem-lisp-mode.autodoc
  (:use :cl :lem :lem-lisp-mode))
(in-package :lem-lisp-mode.autodoc)

(define-key *lisp-mode-keymap* "Space" 'lisp-insert-space-and-autodoc)
(define-key *lisp-mode-keymap* "C-c C-d C-a" 'lisp-autodoc)

(let ((autodoc-symbol nil))
  (defun autodoc-symbol ()
    (or autodoc-symbol
        (setf autodoc-symbol (intern "AUTODOC" :swank)))))

(defun autodoc (function)
  (let ((context (lem-lisp-syntax:parse-for-swank-autodoc (current-point))))
    (lisp-eval-async
     `(,(autodoc-symbol) ',context)
     (lambda (doc)
       (trivia:match doc
         ((list doc _)
          (unless (eq doc :not-available)
            (let* ((buffer (make-buffer "*swank:autodoc-fontity*"
                                        :temporary t :enable-undo-p nil))
                   (point (buffer-point buffer)))
              (erase-buffer buffer)
              (change-buffer-mode buffer 'lisp-mode)
              (insert-string point doc)
              (buffer-start point)
              (multiple-value-bind (result string)
                  (search-forward-regexp point "(?====> (.*) <===)")
                (when result
                  (with-point ((start point))
                    (character-offset point 5)
                    (search-forward point "<===")
                    (delete-between-points start point)
                    (insert-string point string :attribute 'region))))
              (buffer-start (buffer-point buffer))
              (setf (variable-value 'truncate-lines :buffer buffer) nil)
              (funcall function buffer)))))))))

(define-command lisp-autodoc-with-typeout () ()
  (autodoc (lambda (temp-buffer)
             (let ((buffer (make-buffer (buffer-name temp-buffer))))
               (erase-buffer buffer)
               (insert-buffer (buffer-point buffer) temp-buffer)
               (with-pop-up-typeout-window (stream buffer)
                 (declare (ignore stream)))))))

(defvar *autodoc-message* nil)

(defun clear-autodoc-message ()
  (delete-popup-message *autodoc-message*))

(define-command lisp-autodoc () ()
  (autodoc (lambda (buffer)
             (setf *autodoc-message*
                   (display-popup-message buffer
                                          :timeout nil
                                          :destination-window *autodoc-message*))
             (redraw-frame (current-frame)))))

(let ((last-point nil))
  (labels ((hover-p (point)
             (let ((char (character-at point))
                   (before-char (character-at point -1)))
               (or (syntax-symbol-char-p char)
                   (and (syntax-space-char-p before-char)
                        (syntax-closed-paren-char-p char))
                   (syntax-symbol-char-p before-char))))
           (same-last-buffer-p ()
             (or (null last-point)
                 (eq (current-buffer)
                     (point-buffer last-point))))
           (moved-point-p ()
             (or (null last-point)
                 (not (eq (point-buffer (current-point))
                          (point-buffer last-point)))
                 (point/= (current-point) last-point)))
           (lisp-buffer-p ()
             (let ((major-mode (buffer-major-mode (current-buffer))))
               (member major-mode '(lisp-mode lisp-repl-mode))))
           (%autodocp ()
             (prog1 (and (lisp-buffer-p)
                         (hover-p (current-point))
                         (moved-point-p)
                         (same-last-buffer-p))
               (setq last-point (copy-point (current-point) :temporary)))))
    (defun autodocp ()
      (%autodocp))))

(defun command-loop-autodoc ()
  (if (autodocp)
      (lisp-autodoc)
      (clear-autodoc-message)))

(add-hook *post-command-hook* 'command-loop-autodoc)

(define-command lisp-insert-space-and-autodoc (n) ("p")
  (loop :repeat n :do (insert-character (current-point) #\space))
  (unless (continue-flag 'lisp-insert-space-and-autodoc)
    (lisp-autodoc)))

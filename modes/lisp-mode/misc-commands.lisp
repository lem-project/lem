(defpackage :lem-lisp-mode.misc-commands
  (:use :cl :lem :lem-lisp-mode))
(in-package :lem-lisp-mode.misc-commands)

(defparameter *defpackage-names*
  '("defpackage"
    "cl:defpackage"
    "common-lisp:defpackage"
    "define-package"
    "uiop:define-package"))

(defun go-to-defpackage-form (point)
  (buffer-start point)
  (loop
    (unless (scan-lists point 1 -1 t)
      (return nil))
    (skip-whitespace-forward point)
    (if (member (symbol-string-at-point point) *defpackage-names*
                :test #'string-equal)
        (return (scan-lists point -1 1))
        (scan-lists point 1 1 t))))

(defun fresh-line* (point)
  (unless (with-point ((p point))
            (skip-whitespace-backward p t)
            (start-line-p p))
    (insert-character point #\newline)))

(defun go-to-defpackage-spec-form (point spec-name)
  (when (go-to-defpackage-form point)
    (with-point ((limit point))
      (when (form-offset limit 1)
        (cond ((search-forward-symbol point spec-name limit)
               (values point t))
              (t
               (form-offset point 1)
               (scan-lists point -1 -1)
               (fresh-line* point)
               (indent-line point)
               (insert-string point (format nil "(~A)" spec-name))
               (character-offset point -1)
               (values point nil)))))))

(defun get-defun-symbol (point)
  (flet ((fail () (editor-error "scan error")))
    (with-point ((point point))
      (lem-lisp-syntax:top-of-defun point)
      (with-point ((limit point))
        (unless (and (scan-lists limit 1 0 t)
                     (scan-lists point 1 -1 t limit)
                     (form-offset point 2)
                     (form-offset point -1))
          (fail))
        (cond ((syntax-open-paren-char-p (character-at point))
               (scan-lists point 1 -1)
               (skip-whitespace-forward point)
               (let ((symbol-name (symbol-string-at-point point)))
                 (cond ((null symbol-name)
                        (fail))
                       ((equal "setf" symbol-name)
                        (form-offset point 1)
                        (skip-whitespace-forward point)
                        (alexandria:if-let (name (symbol-string-at-point point))
                          name
                          (fail)))
                       (t symbol-name))))
              (t
               (symbol-string-at-point point)))))))

(define-command lisp-add-export (symbol-name)
    ((list (prompt-for-string "Export: " (get-defun-symbol (current-point)))))
  (check-connection)
  (with-point ((point (current-point) :left-inserting))
    (multiple-value-bind (point exists)
        (go-to-defpackage-spec-form point ":export")
      (declare (ignore exists))
      (when point
        (with-point ((limit point :left-inserting))
          (scan-lists limit 1 1)
          (if (or (search-forward-symbol point symbol-name limit)
                  (search-forward-symbol point (format nil ":~A" symbol-name) limit)
                  (search-forward-symbol point (format nil "#:~A" symbol-name) limit))
              (message "~A already exported" symbol-name)
              (with-point ((point point :left-inserting))
                (scan-lists point 1 1)
                (scan-lists point -1 -1)
                (fresh-line* point)
                (indent-line point)
                (insert-string point (format nil "#:~A" symbol-name)))))))))

(defun find-symbol-matchies (symbol-name)
  (let ((symbols '()))
    (do-all-symbols (s)
      (when (and (string-equal s symbol-name) (fboundp s))
        (pushnew s symbols)))
    symbols))

(defun select-menu (items)
  (let (selected-item)
    (lem-if:display-popup-menu (implementation)
                               items
                               :print-spec #'princ-to-string
                               :action-callback (lambda (item)
                                                  (setf selected-item item)))
    (loop
      (redraw-display)
      (let ((key (read-key)))
        (cond ((or (match-key key :sym "Down")
                   (match-key key :ctrl t :sym "n"))
               (lem-if:popup-menu-down (implementation)))
              ((or (match-key key :sym "Up")
                   (match-key key :ctrl t :sym "p"))
               (lem-if:popup-menu-up (implementation)))
              ((match-key key :sym "Return")
               (lem-if:popup-menu-select (implementation))
               (lem-if:popup-menu-quit (implementation))
               (return selected-item))
              ((match-key key :sym "q")
               (lem-if:popup-menu-quit (implementation))
               (return nil)))))))

#|
(define-command lisp-add-missing-import-from (symbol-name)
    ((list (prompt-for-symbol-name "Symbol: " (symbol-string-at-point (current-point)))))
  (multiple-value-bind (symbol external-p symbol-name package)
      (swank::parse-symbol symbol-name)
    (let ((point (current-point)))
      (multiple-value-bind (point exists)
          (go-to-defpackage-spec-form point ":import-from")
        (when point
          )))))
|#

(defun find-utopian-route (point)
  (when (in-string-p point)
    (with-point ((start point)
                 (end point))
      (maybe-beginning-of-string start)
      (move-point end start)
      (character-offset start 1)
      (form-offset end 1)
      (character-offset end -1)
      (let* ((route (points-to-string start end))
             (parts (uiop:split-string route :separator ":")))
        (when (= 2 (length parts))
          (destructuring-bind (path name)
              parts
            (let ((filename (expand-file-name
                             (format nil "../controllers/~A.lisp" path)
                             (buffer-directory (current-buffer)))))
              (unless (probe-file filename)
                (editor-error "~A does not exists" filename))
              (lem.language-mode:make-xref-location
               :filespec (probe-file filename)
               :position (let ((buffer (find-file-buffer filename
                                                         :temporary t
                                                         :enable-undo-p nil)))
                           (with-point ((point (buffer-point buffer)))
                             (buffer-start point)
                             (search-forward-regexp
                              point
                              `(:sequence
                                "(defun"
                                (:greedy-repetition 1 nil :whitespace-char-class)
                                ,name
                                :whitespace-char-class))
                             (line-start point)
                             (position-at-point point)))))))))))

(pushnew 'find-utopian-route lem-lisp-mode::*find-definitions*)

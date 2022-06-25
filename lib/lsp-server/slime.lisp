(cl-lsp/defpackage:defpackage :cl-lsp/slime
  (:use :cl
        :lem-base)
  (:export :symbol-string-at-point*
           :beginning-of-defun-point
           :beginning-of-defun
           :form-string
           :map-buffer-symbols
           :search-buffer-package
           :compilation-notes))
(in-package :cl-lsp/slime)

(defun symbol-string-at-point* (point)
  (let ((string (symbol-string-at-point point)))
    (when string
      (values (ppcre:regex-replace "^(?:#\\.|,@)" string "")))))

(defun beginning-of-defun-point (point n)
  (with-point ((curr point))
    (if (minusp n)
        (dotimes (_ (- n) curr)
          (if (start-line-p curr)
              (line-offset curr -1)
              (line-start curr))
          (loop
            (when (char= #\( (character-at curr 0))
              (return))
            (unless (line-offset curr -1)
              (return-from beginning-of-defun-point curr))))
        (dotimes (_ n curr)
          (loop
            (unless (line-offset curr 1)
              (return-from beginning-of-defun-point curr))
            (when (char= #\( (character-at curr 0))
              (return)))))))

(defun beginning-of-defun (point n)
  (move-point point (beginning-of-defun-point point n)))

(defun form-string (point)
  (if (and (start-line-p point)
           (eql #\( (character-at point)))
      (with-point ((p point))
        (when (form-offset p 1)
          (points-to-string point p)))
      (with-point ((p point))
        (when (form-offset p -1)
          (points-to-string p point)))))

(defun map-buffer-symbols (buffer function)
  (with-point ((p (buffer-start-point buffer)))
    (loop
      (loop
        (when (= 0 (skip-chars-forward p
                                       (complement
                                        (lambda (c)
                                          (or (member c '(#\, #\' #\`))
                                              (syntax-symbol-char-p c))))))
          (return-from map-buffer-symbols))
        (alexandria:if-let ((str (looking-at p ",@|,|'|`|#\\.")))
          (character-offset p (length str))
          (return)))
      (cond
        ((maybe-beginning-of-string-or-comment p)
         (unless (form-offset p 1) (return)))
        (t
         (with-point ((start p))
           (form-offset p 1)
           (funcall function (points-to-string start p))))))))

(defun search-buffer-package (point)
  (with-point ((p point))
    (buffer-start p)
    (or (loop :while (search-forward-regexp p "^\\s*\\(in-package\\s")
              :do (with-point ((start p))
                    (when (form-offset p 1)
                      (handler-case (let ((name (symbol-name
                                                 (read-from-string
                                                  (points-to-string start p)))))
                                      (unless (equal name "CL-USER")
                                        (return (find-package name))))
                        (error ()
                          (find-package "CL-USER"))))))
        (find-package "CL-USER"))))

(defun compilation-notes (notes function)
  (dolist (note notes)
    (trivia:match note
      ((and (trivia:property :location
                             (or (list :location
                                       (list :buffer buffer-name)
                                       (list :offset pos _)
                                       _)
                                 (list :location
                                       (list :file file)
                                       (list :position pos)
                                       _)))
            (or (trivia:property :message message) (and))
            (or (trivia:property :severity severity) (and))
            (or (trivia:property :source-context _) (and)))
       (let* ((buffer (if buffer-name
                          (get-buffer buffer-name)
                          (get-file-buffer file)))
              (point (buffer-point buffer)))
         (move-to-position point pos)
         (skip-chars-backward point #'syntax-symbol-char-p)
         (with-point ((end point))
           (unless (form-offset end 1)
             #+(or)
             (when (eq severity :read-error) ;dead code
               (buffer-start point))
             (buffer-end end))
           (funcall function point end severity message)))))))

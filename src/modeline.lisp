(in-package :lem)

(export '(*modeline-default-format*
          modeline-read-only-p
          modeline-modified-p
          modeline-name
          modeline-major-mode
          modeline-minor-modes
          modeline-linum
          modeline-column))

(defvar *modeline-default-format*
  (list 'modeline-read-only-p
        'modeline-modified-p
        " "
        *program-name*
        ": "
        'modeline-name
        " ("
        'modeline-major-mode
        'modeline-minor-modes
        ") "
        "("
        'modeline-linum
        ", "
        'modeline-column
        ")"))

(defun posline (window)
  (cond
    ((<= (buffer-nlines (window-buffer window))
         (window-height window))
     "All")
    ((= 1 (window-view-linum window))
     "Top")
    ((<= (buffer-nlines (window-buffer window))
         (+ (window-view-linum window) (window-height window)))
     "Bot")
    (t
     (format nil "~2d%"
             (floor
              (* 100
                 (float (/ (window-view-linum window)
                           (buffer-nlines (window-buffer window))))))))))

(defun modeline-read-only-p (window)
  (if (buffer-read-only-p (window-buffer window)) "%" "-"))

(defun modeline-modified-p (window)
  (if (buffer-modified-p (window-buffer window)) "*" "-"))

(defun modeline-name (window)
  (buffer-name (window-buffer window)))

(defun modeline-major-mode (window)
  (string-downcase (buffer-major-mode (window-buffer window))))

(defun modeline-minor-modes (window)
  (let ((modes (buffer-minor-modes (window-buffer window))))
    (when (key-recording-p)
      (setf modes (append modes (list "Def"))))
    (if modes
        (format nil " ~(~{~a~^ ~}~)" modes)
        "")))

(defun modeline-linum (window)
  (window-current-linum window))

(defun modeline-column (window)
  (str-width (buffer-line-string (window-buffer window)
                                 (window-current-linum window))
             0
             (window-current-charpos window)))

(defun modeline-string (window)
  (let* ((line-pos (posline window))
         (winwidth (window-width window))
         (str (with-output-to-string (out)
                (dolist (x
                         (get-bvar :modeline-format
                                   :buffer (window-buffer window)
                                   :default *modeline-default-format*))
                  (if (or (symbolp x) (functionp x))
                      (princ (funcall x window) out)
                      (princ x out))))))
    (let ((n (- winwidth 7 (length str))))
      (if (minusp n)
          (format nil "~a ~a --" str line-pos)
          (format nil "~a~v,,,va ~a --"
                  str
                  n
                  (if (eq window (current-window)) #\- #\space)
                  #\space
                  line-pos)))))

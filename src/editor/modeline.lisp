(in-package :lem)

(export '(*modeline-default-format*
          modeline-add-status-list
          modeline-remove-status-list
          modeline-read-only-p
          modeline-modified-p
          modeline-name
          modeline-major-mode
          modeline-minor-modes
          modeline-linum
          modeline-column
          modeline-string))

(defvar *modeline-default-format*
  (list 'modeline-read-only-p
        'modeline-modified-p
        " "
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

(defvar *modeline-status-list* nil)

(defun modeline-add-status-list (x &optional (buffer nil bufferp))
  (if bufferp
      (push x (get-bvar :modeline-status-list :buffer buffer))
      (push x *modeline-status-list*))
  (values))

(defun modeline-remove-status-list (x &optional (buffer nil bufferp))
  (if bufferp
      (setf (get-bvar :modeline-status-list :buffer buffer)
            (remove x (get-bvar :modeline-status-list :buffer buffer)))
      (setf *modeline-status-list*
            (remove x *modeline-status-list*))))

(defun posline (window)
  (cond
    ((<= (buffer-nlines (window-buffer window))
         (window-height window))
     "All")
    ((first-line-p (window-view-point window))
     "Top")
    ((null (line-offset (copy-point (window-view-point window)
                                    :temporary)
                        (window-height window)))
     "Bot")
    (t
     (format nil "~2d%"
             (floor
              (* 100
                 (float (/ (line-number-at-point (window-view-point window))
                           (buffer-nlines (window-buffer window))))))))))

(defun modeline-read-only-p (window)
  (if (buffer-read-only-p (window-buffer window)) "%" "-"))

(defun modeline-modified-p (window)
  (if (buffer-modified-p (window-buffer window)) "*" "-"))

(defun modeline-name (window)
  (buffer-name (window-buffer window)))

(defun modeline-major-mode (window)
  (mode-name (buffer-major-mode (window-buffer window))))

(defun modeline-minor-modes (window)
  (with-output-to-string (*standard-output*)
    (let ((firstp t))
      (dolist (x (append (mapcar #'mode-name (buffer-minor-modes (window-buffer window)))
                         (get-bvar :modeline-status-list :buffer (window-buffer window))
                         *modeline-status-list*))
        (princ " ")
        (if (functionp x)
            (princ (funcall x window))
            (princ x))
        (setf firstp t)))))

(defun modeline-linum (window)
  (line-number-at-point (window-point window)))

(defun modeline-column (window)
  (point-column (window-point window)))

(defun modeline-string (window)
  (let* ((posline (posline window))
         (str (with-output-to-string (out)
                (dolist (x
                         (get-bvar :modeline-format
                                   :buffer (window-buffer window)
                                   :default *modeline-default-format*))
                  (if (or (symbolp x) (functionp x))
                      (princ (funcall x window) out)
                      (princ x out)))))
         (mdstr (make-string (window-width window)
                             :initial-element #\space)))
    (replace mdstr str)
    (replace mdstr posline :start1 (- (length mdstr) 5))))

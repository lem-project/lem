(in-package :lem)

(export '(*modeline-default-format*
          modeline-add-status-list
          modeline-remove-status-list
          modeline-read-only-p
          modeline-modified-p
          modeline-name
          modeline-major-mode
          modeline-minor-modes
          modeline-string))

(defvar *modeline-default-format*
  (list 'modeline-read-only-p
        'modeline-modified-p
        " "
        'modeline-name
        " ("
        'modeline-major-mode
        'modeline-minor-modes
        ") "))

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

(defun modeline-read-only-p (window)
  (if (buffer-read-only-p (window-buffer window)) "%" "-"))

(defun modeline-modified-p (window)
  (if (buffer-modified-p (window-buffer window)) "*" "-"))

(defun modeline-name (window)
  (buffer-name (window-buffer window)))

(defun modeline-major-mode (window)
  (string-downcase (buffer-major-mode (window-buffer window))))

(defun modeline-minor-modes (window)
  (with-output-to-string (*standard-output*)
    (let ((firstp t))
      (dolist (x (append (buffer-minor-modes (window-buffer window))
                         (append (get-bvar :modeline-status-list :buffer (window-buffer window))
                                 *modeline-status-list*)))
        (princ " ")
        (if (functionp x)
            (princ (funcall x window))
            (let ((*print-case* :downcase)) (princ x)))
        (setf firstp t)))))

(defun modeline-string (window)
  (let ((str (with-output-to-string (out)
               (dolist (x
                        (get-bvar :modeline-format
                                  :buffer (window-buffer window)
                                  :default *modeline-default-format*))
                 (if (or (symbolp x) (functionp x))
                     (princ (funcall x window) out)
                     (princ x out)))))
        (mdstr (make-string (window-width window) :initial-element #\space)))
    (replace mdstr str)
    mdstr))

(in-package :lem)

(export '(prompt-start-point
          prompt-active-p
          active-minibuffer-window
          prompt-for-character
          prompt-for-y-or-n-p
          prompt-for-string
          prompt-for-integer))

(defgeneric prompt-start-point (prompt))
(defgeneric caller-of-prompt-window (prompt))
(defgeneric prompt-active-p (prompt))
(defgeneric active-minibuffer-window ()) ;TOOD
(defgeneric prompt-for-character (prompt-string))
(defgeneric prompt-for-line (prompt initial comp-f existing-p history-name &optional syntax-table))

(defun prompt-for-y-or-n-p (prompt-string)
  (do () (nil)
    (let ((c (prompt-for-character (format nil "~a [y/n]? " prompt-string))))
      (case c
        (#\y (return t))
        (#\n (return nil))))))

(defun prompt-for-string (prompt &key initial-value
                                      completion-function
                                      test-function
                                      (history-symbol nil)
                                      (syntax-table (current-syntax)))
  (prompt-for-line prompt
                   initial-value
                   completion-function
                   test-function
                   history-symbol
                   syntax-table))

(defun prompt-for-integer (prompt &optional min max)
  (parse-integer
   (prompt-for-string prompt
                      :test-function (lambda (str)
                                       (multiple-value-bind (n len)
                                           (parse-integer str :junk-allowed t)
                                         (and
                                          n
                                          (/= 0 (length str))
                                          (= (length str) len)
                                          (if min (<= min n) t)
                                          (if max (<= n max) t))))
                      :history-symbol 'prompt-for-integer)))

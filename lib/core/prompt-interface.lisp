(in-package :lem)

(export '(prompt-start-point
          prompt-active-p))

(defgeneric prompt-start-point (prompt))
(defgeneric caller-of-prompt-window (prompt))
(defgeneric prompt-for-character (prompt-string))
(defgeneric prompt-active-p (prompt))

(defgeneric active-minibuffer-window ()) ;TOOD

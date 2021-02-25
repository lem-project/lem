(in-package :lem)

(export '(prompt-start-point))

(defgeneric prompt-start-point (prompt))
(defgeneric caller-of-prompt-window (prompt))
(defgeneric prompt-for-character (prompt-string))

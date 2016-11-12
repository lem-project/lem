(in-package :lem)

(export '(*mark-overlay-attribute*
          *modeline-attribute*
          *modeline-inactive-attribute*
          *control-char-attribute*
          *syntax-string-attribute*
          *syntax-comment-attribute*
          *syntax-keyword-attribute*
          *syntax-constant-attribute*
          *syntax-function-name-attribute*
          *syntax-variable-attribute*
          make-attribute))

(defstruct (attribute (:constructor %make-attribute))
  fg-color
  bg-color
  reverse-p
  bold-p
  underline-p
  %internal-value)

(defun make-attribute (fg-color bg-color &key reverse-p bold-p underline-p)
  (%make-attribute :fg-color fg-color
                   :bg-color bg-color
                   :reverse-p reverse-p
                   :bold-p bold-p
                   :underline-p underline-p))

(defvar *mark-overlay-attribute* (make-attribute "blue" nil :reverse-p t))
(defvar *modeline-attribute* (make-attribute nil nil :reverse-p t))
(defvar *modeline-inactive-attribute* (make-attribute nil nil :reverse-p t))
(defvar *control-char-attribute* (make-attribute nil nil :reverse-p t))
(defvar *syntax-string-attribute* (make-attribute "green" nil))
(defvar *syntax-comment-attribute* (make-attribute "red" nil))
(defvar *syntax-keyword-attribute* (make-attribute "blue" nil))
(defvar *syntax-constant-attribute* (make-attribute "magenta" nil))
(defvar *syntax-function-name-attribute* (make-attribute "cyan" nil))
(defvar *syntax-variable-attribute* (make-attribute "yellow" nil))

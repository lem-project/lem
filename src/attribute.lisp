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
          copy-attribute
          attribute-p
          attribute-name
          attribute-fg-color
          attribute-bg-color
          attribute-reverse-p
          attribute-bold-p
          attribute-underline-p
          attribute-%internal-value
          make-attribute
          set-attribute))

(defstruct (attribute (:constructor %make-attribute))
  name
  fg-color
  bg-color
  reverse-p
  bold-p
  underline-p
  %internal-value)

(defmethod print-object ((object attribute) stream)
  (print-unreadable-object (object stream)
    (format stream "ATTRIBUTE: ~A" (attribute-name object))))

(defun make-attribute (fg-color bg-color &key reverse-p bold-p underline-p name)
  (%make-attribute :name name
                   :fg-color fg-color
                   :bg-color bg-color
                   :reverse-p reverse-p
                   :bold-p bold-p
                   :underline-p underline-p))

(defun set-attribute (attribute &key
                                (fg-color nil fg-color-p)
                                (bg-color nil bg-color-p)
                                (reverse-p nil reverse-p-p)
                                (bold-p nil bold-p-p)
                                (underline-p nil underline-p-p)
                                (name nil name-p))
  (setf (attribute-%internal-value attribute) nil) ; これをnilにするのはncurses依存かもしれないのであまりよくない
  (when fg-color-p
    (setf (attribute-fg-color attribute) fg-color))
  (when bg-color-p
    (setf (attribute-bg-color attribute) bg-color))
  (when reverse-p-p
    (setf (attribute-reverse-p attribute) reverse-p))
  (when bold-p-p
    (setf (attribute-bold-p attribute) bold-p))
  (when underline-p-p
    (setf (attribute-underline-p attribute) underline-p))
  (when name-p
    (setf (attribute-name attribute) name))
  attribute)

(defvar *mark-overlay-attribute* (make-attribute "blue" nil :reverse-p t :name  "mark-overlay"))
(defvar *modeline-attribute* (make-attribute nil nil :reverse-p t :name  "modeline"))
(defvar *modeline-inactive-attribute* (make-attribute nil nil :reverse-p t :name  "modeline-inactive"))
(defvar *control-char-attribute* (make-attribute nil nil :reverse-p t :name  "control-char"))
(defvar *syntax-string-attribute* (make-attribute "green" nil :name  "string"))
(defvar *syntax-comment-attribute* (make-attribute "red" nil :name  "comment"))
(defvar *syntax-keyword-attribute* (make-attribute "blue" nil :name  "keyword"))
(defvar *syntax-constant-attribute* (make-attribute "magenta" nil :name  "constant"))
(defvar *syntax-function-name-attribute* (make-attribute "cyan" nil :name  "function"))
(defvar *syntax-variable-attribute* (make-attribute "yellow" nil :name  "variable"))

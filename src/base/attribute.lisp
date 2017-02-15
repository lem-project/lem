(in-package :lem-base)

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
          *syntax-type-attribute*
          copy-attribute
          attribute
          attribute-p
          attribute-name
          attribute-foreground
          attribute-background
          attribute-reverse-p
          attribute-bold-p
          attribute-underline-p
          attribute-%internal-value
          make-attribute
          set-attribute))

(defstruct (attribute (:constructor %make-attribute))
  name
  foreground
  background
  reverse-p
  bold-p
  underline-p
  %internal-value)

(defmethod print-object ((object attribute) stream)
  (print-unreadable-object (object stream)
    (format stream "ATTRIBUTE: ~A" (attribute-name object))))

(defun make-attribute (foreground background &key reverse-p bold-p underline-p name)
  (%make-attribute :name name
                   :foreground foreground
                   :background background
                   :reverse-p reverse-p
                   :bold-p bold-p
                   :underline-p underline-p))

(defun set-attribute (attribute &key
                                (foreground nil foreground-p)
                                (background nil background-p)
                                reverse-p
                                bold-p
                                underline-p
                                (name nil name-p))
  (setf (attribute-%internal-value attribute) nil) ; これをnilにするのはncurses依存かもしれないのであまりよくない
  (when foreground-p
    (setf (attribute-foreground attribute) foreground))
  (when background-p
    (setf (attribute-background attribute) background))
  (setf (attribute-reverse-p attribute) reverse-p)
  (setf (attribute-bold-p attribute) bold-p)
  (setf (attribute-underline-p attribute) underline-p)
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
(defvar *syntax-type-attribute* (make-attribute "cyan" nil :name "type"))

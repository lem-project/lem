(in-package :lem)

(defstruct (screen (:constructor %make-screen))
  view
  use-modeline
  modeline-elements
  x
  y
  left-lines
  left-width
  old-left-width
  lines
  old-lines
  wrap-lines
  width
  modified-p
  last-buffer-name
  last-buffer-modified-tick
  (horizontal-scroll-start 0))

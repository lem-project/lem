(in-package :lem-capi)

(defvar *default-font-spec*
  #+win32 (cons "Consolas" 10)
  #+linux (cons "DejaVu Sans Mono" 9)
  #+macosx (cons "Osaka" 10))

(defvar *lem-panel*)
(defvar *lem-process*)

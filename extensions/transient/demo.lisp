(in-package :transient)

(define-transient *demo-keymap*
  :display-style :row
  (:keymap
   :display-style :column
   :description "file operations"
   (:key "o" :suffix demo-open :description "demo open")
   (:key "s" :suffix demo-save :description "demo save (disabled)" :active-p nil)
   (:key "w" :suffix demo-write :description "demo write")
   (:key "x"
    :suffix (:keymap
             (:key "p" :suffix demo-pdf :description "pdf")
             (:key "h" :suffix demo-html :description "html")
             (:key "m" :suffix demo-md :description "markdown"))
    :description "export format"))
  (:keymap
   :display-style :column
   :description "edit operations"
   (:key "c" :suffix demo-copy)
   (:key "v" :suffix demo-paste)
   (:key "u" :suffix demo-undo))
  (:key "f"
   :suffix (:keymap
            (:key "g" :suffix demo-grep :description "grep")
            (:key "f" :suffix demo-find :description "find")
            (:key "r" :suffix demo-replace :description "replace"))
   :description "search menu")
  (:key "t"
   :suffix (:keymap
            :display-style :row
            (:keymap
             :description "languages"
             (:key "l"
              :type :choice
              :choices ("lisp" "python" "js")
              :description "mode"))
            (:keymap
             :description "editor"
             (:key "v"
              :type :choice
              :choices ("vim" "emacs")
              :description "keys")))
   :description "langs demo")
  (:key "d"
   :type :choice
   :choices ("on" "off")
   :description "debug toggle"))

(define-key *global-keymap* "C-c t" *demo-keymap*)
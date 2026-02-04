(in-package :lem/transient)

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
             (:key "m" :suffix demo-md :description "markdown")
             (:key "b" :behavior :back :description "back"))
    :description "export format"))
  (:keymap
   :display-style :column
   :description "edit operations"
   (:key "c" :suffix demo-copy)
   (:key "v" :suffix demo-paste)
   (:key "u" :suffix demo-undo)
   (:key "q" :behavior :cancel :description "quit"))
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
              :id :mode
              :choices ("lisp" "python" "js")
              :value "python"
              :description "mode"))
            (:keymap
             :description "editor"
             (:key "v"
              :type :choice
              :choices ("vim" "emacs")
              :description "keys")))
   :description "langs demo")
  (:key "a"
   :type :choice
   :choices ("value1" "value2" "value3")
   :description "multi-value infix")
  (:key "R" :suffix demo-run :description "run with mode")
  (:key "T" :type toggle :value t :suffix demo-toggle :description "demo toggle")
  (:key "e e" :type toggle :value t :suffix demo-toggle :description "another demo toggle")
  (:key "e a" :type toggle :value t :suffix demo-toggle :description "and another demo toggle"))

(define-command demo-run () ()
  (let ((mode-prefix (find-prefix-by-id *demo-keymap* :mode)))
    (message "mode thing value: ~A" (prefix-value mode-prefix))))

(define-key *global-keymap* "C-c t" *demo-keymap*)
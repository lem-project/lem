(defsystem "lem-notmuch"
  :depends-on ("lem" "yason" "alexandria")
  :serial t
  :components (
	       (:file "package")
	       (:file "backend")
	       (:file "notmuch-message")
	       (:file "notmuch-search")
	       (:file "notmuch-hello")
	       (:file "notmuch")))

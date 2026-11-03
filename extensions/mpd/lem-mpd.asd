(defsystem "lem-mpd"
  :description "An MPD client with a directory-mode style browser."
  :depends-on ("lem/core"
               "usocket"
               "babel")
  :serial t
  :components ((:file "mpd-client")
               (:file "mpd")))

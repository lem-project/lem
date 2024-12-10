
(defpackage :lem-notmuch
  (:use :cl
        :lem)
  (:export
   :*notmuch-executable*
   :*notmuch-saved-searches*
   :notmuch-hello-mode
   :notmuch-open-saved-query
   ; todo: for testing
   :notmuch-search
   :notmuch-count
   :run-notmuch
   ; main entrypoint
   :notmuch))


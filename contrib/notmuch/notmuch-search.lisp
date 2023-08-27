
(in-package :lem-notmuch)

(define-major-mode notmuch-search-mode ()
    (:name "notmuch-search"
     :keymap *notmuch-search-mode-keymap*))


; todo
;(define-key *notmuch-search-mode-keymap* "Return" 'lem-notmuch:notmuch-search-show-thread)

(define-key *notmuch-search-mode-keymap* "q" 'quit-active-window)


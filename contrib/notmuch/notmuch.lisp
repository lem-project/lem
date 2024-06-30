(in-package :lem-notmuch)

(defvar *notmuch-saved-searches*
  '((:name "inbox" :query "tag:inbox")
    (:name "unread" :query "tag:inbox and tag:unread")))


(define-attribute notmuch-saved-search-attribute
  (t :bold t))


(define-command notmuch () ()
  (let ((buffer (make-buffer "*notmuch-hello*")))
    (switch-to-buffer buffer)
    (notmuch-hello-mode)
    (erase-buffer buffer)
    (notmuch-hello buffer)))


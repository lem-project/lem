(defpackage lem-mailbox
  (:use :cl)
  (:export
   :list-mailbox-messages
   :mailbox
   :mailbox-count
   :mailbox-empty-p
   :mailbox-name
   :mailboxp
   :make-mailbox
   :receive-message
   :receive-message-no-hang
   :receive-pending-messages
   :send-message))



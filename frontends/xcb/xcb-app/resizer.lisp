(in-package :xcb)
;;=============================================================================
;;
;; Resizing issues
;;
;; Interactive resizing events come way too often, and resizing client app may
;; be prohibitively expensive in terms of reallocation of buffers.  It is
;; much more palatable to wait till the interactive resize is done, and then
;; notify the client of the new size.
;;
;; Currently, this is accomplished by delaying client code until
;; two sequential resize requests in with the same size information.  This
;; behavior is not particularly documented, and it may be better to check for
;; the mouse press instead, unless there is a real way to do this...
;;
;;
;; Since resizing is an non-reentrant operation, it is sufficient to have
;; global resize parameters for tracking the above consecutive events...




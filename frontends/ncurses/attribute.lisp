(defpackage :lem-ncurses/attribute
  (:use :cl :lem)
  (:export :attribute-to-attr-and-pair
           :%wattr-set))
(in-package :lem-ncurses/attribute)

;; wattr_set with the real ncurses signature: the color pair is a short
;; passed BY VALUE. cl-charms' charms/ll:wattr-set declares the pair
;; argument as a pointer (confused with wattr_get), so it cannot be
;; used. One wattr_set call applies attributes and the color pair
;; together, replacing the wattr-on + wcolor-set pair (and the two-call
;; reset) — halving the FFI round-trips of every printed cell.
;; CHARMS/LL::ATTR_T is an internal cl-charms symbol, not an exported
;; one, but it is the correct C type (attr_t) for wattr_set's attrs
;; argument and cannot be replaced by a plain :int: attr_t is a chtype
;; based type that is wider than int on some platforms, so hardcoding
;; :int would mis-pass the bits on those ABIs.
(cffi:defcfun ("wattr_set" %wattr-set)
    :int
  "Apply ATTRS and PAIR to WIN in a single FFI call, ncurses'
wattr_set(3). The pair is a short BY VALUE (cl-charms' own
charms/ll:wattr-set mis-declares it as a pointer, hence this binding).
WIN and OPTS are raw window / option pointers; OPTS is always passed
nil by lem. Returns OK (0) or ERR (-1)."
  (win :pointer)
  (attrs charms/ll::attr_t)
  (pair :short)
  (opts :pointer))

(defun underline-color (attribute)
  (cond ((eq t (attribute-underline attribute))
         nil)
        ((and (attribute-underline attribute)
              (parse-color (attribute-underline attribute)))
         (attribute-underline attribute))
        (t
         nil)))

(defun compute-attribute-value (attribute cursorp)
  "Return (values attrs pair-number) for ATTRIBUTE: ATTRS holds the
non-color attributes (bold, underline, reverse) and PAIR-NUMBER the
curses color pair. The pair is kept separate from the attribute bits
because the A_COLOR field of OR'd attribute bits is only 8 bits wide;
pair numbers above 255 (common in direct-color mode, where every
distinct RGB combination gets its own pair) would spill into other
attribute bits. Callers must apply the pair through wattr_set
(%wattr-set), whose pair argument is a short taking the full range."
  (let* ((underline-color (underline-color attribute))
         (foreground (or underline-color (attribute-foreground attribute)))
         (background (or (attribute-background attribute)
                         lem-if:*background-color-of-drawing-window*))
         (pair (lem-ncurses/term:get-color-pair foreground background)))
    (values (logior (if (attribute-bold attribute)
                        charms/ll:a_bold
                        0)
                    (if (attribute-underline attribute)
                        charms/ll:a_underline
                        0)
                    (if (or cursorp (attribute-reverse attribute))
                        charms/ll:a_reverse
                        0))
            pair)))

(defun attribute-to-attr-and-pair (attribute-or-name)
  "Resolve ATTRIBUTE-OR-NAME (a lem:attribute instance, or a symbol
naming one via ensure-attribute) to (values attrs pair) for rendering:
ATTRS are the OR'd non-color attribute bits (bold, underline, reverse,
plus :cursor handling) and PAIR the curses color pair number. Results
are memoized per attribute instance and drawing-window background via
get-attribute-cache, so repeated lookups of the same attribute within a
redisplay cost one cache hit instead of a full color-pair resolution.
Apply the results with %wattr-set, never wattron (pair numbers above
255 do not fit the attribute bits)."
  (let* ((attribute (ensure-attribute attribute-or-name nil))
         (cursorp (or (eq attribute-or-name 'cursor)
                      (and attribute (lem-core:attribute-value attribute :cursor)))))
    (when (and lem-if:*background-color-of-drawing-window* (null attribute))
      (setf attribute (make-attribute :background lem-if:*background-color-of-drawing-window*)))
    (if (null attribute)
        (values 0 0)
        (let ((cached (get-attribute-cache
                       attribute
                       :background lem-if:*background-color-of-drawing-window*)))
          (if (consp cached)
              (values (car cached) (cdr cached))
              (multiple-value-bind (attrs pair)
                  (compute-attribute-value attribute cursorp)
                (setf (get-attribute-cache
                       attribute
                       :background lem-if:*background-color-of-drawing-window*)
                      (cons attrs pair))
                (values attrs pair)))))))

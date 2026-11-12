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
(cffi:defcfun ("wattr_set" %wattr-set)
    :int
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
attribute bits. Callers must apply the pair with wcolor-set, not
wattron."
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

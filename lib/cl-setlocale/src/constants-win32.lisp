(in-package :cl-setlocale)

(defconstant +lc-all+ 0
  "Set the entire locale generically.")

(defconstant +lc-collate+ 1
  "Set a locale for string collation routines. This controls
alphabetic ordering in strcoll() and strxfrm().")

(defconstant +lc-ctype+ 2
  "Set a locale for the ctype(3) and multibyte(3) functions. This
  controls recognition of upper and lower case, alphabetic or
  non-alphabetic characters, and so on.")

(defconstant +lc-monetary+ 3
  "Set a locale for formatting monetary values; this affects the
  localeconv() function.")

(defconstant +lc-numeric+ 4
  "Set a locale for formatting numbers.  This controls the
formatting of decimal points in input and output of floating point numbers
in functions such as printf() and scanf(), as well as values returned by
localeconv().")

(defconstant +lc-time+ 5
  "Set a locale for formatting dates and times using the strftime()
function.")

;; FIXME: No LC_MESSAGES on Windows. Does it have a replacements?
(defconstant +lc-messages+ 6)

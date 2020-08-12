(in-package :cl-setlocale)
(include "locale.h")

(constant (+lc-all+ "LC_ALL")
  :documentation "Set the entire locale generically.")

(constant (+lc-collate+ "LC_COLLATE")
  :documentation "Set a locale for string collation routines. This controls
alphabetic ordering in strcoll() and strxfrm().")

(constant (+lc-ctype+ "LC_CTYPE")
  :documentation "Set a locale for the ctype(3) and multibyte(3) functions.
This controls recognition of upper and lower case, alphabetic or
non-alphabetic characters, and so on.")

(constant (+lc-messages+ "LC_MESSAGES")
  :documentation "Set a locale for message catalogs, see catopen(3)
function.")

(constant (+lc-monetary+ "LC_MONETARY")
  :documentation "Set a locale for formatting monetary values; this affects
the localeconv() function.")

(constant (+lc-numeric+ "LC_NUMERIC")
  :documentation "Set a locale for formatting numbers.  This controls the
formatting of decimal points in input and output of floating point numbers
in functions such as printf() and scanf(), as well as values returned by
localeconv().")

(constant (+lc-time+ "LC_TIME")
  :documentation "Set a locale for formatting dates and times using the
strftime() function.")

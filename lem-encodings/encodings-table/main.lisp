(uiop/package:define-package :lem-encodings-table/main (:nicknames) (:use :cl)
                             (:shadow) (:export :generate-table) (:intern))
(in-package :lem-encodings-table/main)
;;don't edit above

(defun generate-table ()
  (lem-encodings-table/sjis:generate-table 
   (asdf:system-relative-pathname :lem-encodings "cp932.table")
   "CP932")
  (lem-encodings-table/euc:generate-table 
   (asdf:system-relative-pathname :lem-encodings "euc-jp.table")
   "EUC-JP" :country :jp)
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "koi8-u.table")
   "KOI8-U")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-2.table")
   "ISO-8859-2")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-5.table")
   "ISO-8859-5")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-6.table")
   "ISO-8859-6")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-7.table")
   "ISO-8859-7")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-8.table")
   "ISO-8859-8")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-9.table")
   "ISO-8859-9")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-13.table")
   "ISO-8859-13"))

;;don't edit
(DEFSYSTEM "lem-setlocale" :CLASS :PACKAGE-INFERRED-SYSTEM :COMPONENTS
 (#-darwin(:FILE "cffi")
  #+darwin(:FILE "cffi_darwin"))
  :AUTHOR "SANO Masatoshi" :MAILTO "snmsts@gmail.com")

;;don't edit
(DEFSYSTEM "lem-setlocale" :CLASS :PACKAGE-INFERRED-SYSTEM :COMPONENTS
 (#-darwin(:FILE "cffi")
  #+darwin(:FILE "cffi_darwin"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com"
  :license "MIT")

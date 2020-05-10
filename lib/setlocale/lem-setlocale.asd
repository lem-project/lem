;;don't edit
(defsystem "lem-setlocale"
  :depends-on ("lem-core")
  :class :package-inferred-system
  :components (#-(or darwin win32)(:FILE "cffi")
               #+darwin(:FILE "cffi_darwin")
               #+win32(:FILE "cffi_windows"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com")

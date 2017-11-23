(defpackage :lem-webview
  (:use :cl :lem :lem-jsonrpc)
  (:export :webview-open))
(in-package :lem-webview)

(define-command webview-open (url) ("sUrl: ")
  (import-electron-module
   (merge-pathnames "webview.js"
                    (asdf:system-source-directory :lem-webview)))
  (notify "webview-open"
          (alexandria:plist-hash-table `("url" ,url)
                                       :test #'equal)))

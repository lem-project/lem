(defpackage :lem-webview
  (:use :cl :lem :lem-jsonrpc :lem-electron-backend :parenscript)
  (:export :webview-open))
(in-package :lem-webview)

(define-command webview-open (url) ("sUrl: ")
  (js-eval (gen-webview-html))
  (notify "webview-open"
          (alexandria:plist-hash-table `("url" ,url)
                                       :test #'equal)))

(defun gen-webview-html ()
  (ps
   (when (= 0 (@ ((@ document get-elements-by-tag-name) "webview") length))
     (let* ((lem-editor ((@ document get-element-by-id) "lem-editor"))
            (view ((@ document create-element) "webview")))
       (setf (@ view autosize) "on")
       (setf (@ view style height) "100%")
       ((@ lem-editor on) "webview-open"
                          (lambda (params)
                            (setf (@ view src) (@ params url))))
       ((@ lem-editor set-pane) view)
       nil))))

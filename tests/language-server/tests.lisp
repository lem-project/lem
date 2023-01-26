(defpackage :lem-tests/language-server/tests
  (:use :cl
        :testif
        :lem-language-server
        :lem-tests/language-server/utils)
  (:import-from :lem-language-server/protocol/yason-utils
                :parse-json)
  (:import-from :lem-language-server/protocol/lsp-type
                :make-lsp-map)
  (:import-from :lem-language-server/protocol/converter
                :convert-to-json
                :convert-from-json))
(in-package :lem-tests/language-server/tests)

(defun call-initialize-request ()
  (call-lsp-method (make-instance 'initialize-request)
                   (convert-to-json
                    (make-instance 'lsp:initialize-params
                                   :process-id (random 10000)
                                   :client-info (make-lsp-map :name "test-client"
                                                              :version "1.2.3")
                                   :root-uri "file://hoge/piyo/"
                                   :root-path "/hoge/piyo/"
                                   :capabilities (lem-lsp-mode::client-capabilities)
                                   :trace "off"
                                   :workspace-folder :null))))

(defun call-shutdown-request ()
  (call-lsp-method (make-instance 'shutdown-request)
                   nil))

(defun call-exit-request ()
  (call-lsp-method (make-instance 'exit-request)
                   nil))

(test "initialize"
  (with-mock-server ()
    (let ((response (call-initialize-request)))
      (ok (convert-from-json response 'lsp:initialize-result)))))

(test "shutdown"
  (with-mock-server ()
    (ok (signals (call-shutdown-request) 'uninitialized-error)))
  (with-mock-server ()
    (let ((response (call-initialize-request)))
      (ok (convert-from-json response 'lsp:initialize-result))
      (call-shutdown-request))))

(test "exit"
  (with-mock-server ()
    (call-initialize-request)
    (call-shutdown-request)
    (call-exit-request)
    (ok (eql 0 (mock-server-exit-status (current-server)))))
  (with-mock-server ()
    (call-initialize-request)
    (call-exit-request)
    (ok (eql 1 (mock-server-exit-status (current-server))))))

(defun call-did-open-text-document-request (&key uri language-id version text)
  (call-lsp-method (make-instance 'text-document-did-open-request)
                   (convert-to-json
                    (make-instance 'lsp:did-open-text-document-params
                                   :text-document (make-instance 'lsp:text-document-item
                                                                 :uri uri
                                                                 :language-id language-id
                                                                 :version version
                                                                 :text text)))))

(test "textDocument/didOpen"
  (with-mock-server ()
    (call-initialize-request)
    (call-did-open-text-document-request :uri "file:///hoge/piyo/foo.lisp"
                                         :language-id "lisp"
                                         :version 1
                                         :text "(cons 1 2)")
    (let ((text-document
            (find-text-document (make-instance 'lsp:text-document-identifier
                                               :uri "file:///hoge/piyo/foo.lisp"))))
      (ok (equal "file:///hoge/piyo/foo.lisp" (text-document-uri text-document)))
      (ok (equal "lisp" (text-document-language-id text-document)))
      (ok (equal 1 (text-document-version text-document)))
      (let ((buffer (text-document-buffer text-document)))
        (ok (lem:buffer-temporary-p buffer))
        (ok (eq lem-lisp-syntax:*syntax-table* (lem:buffer-syntax-table buffer)))
        (ok (equal "(cons 1 2)" (lem:buffer-text buffer)))))))

(defun call-did-change-text-document-request (uri content-changes)
  (call-lsp-method
   (make-instance 'text-document-did-change-request)
   (convert-to-json
    (make-instance 'lsp:did-change-text-document-params
                   :text-document (make-instance 'lsp:versioned-text-document-identifier
                                                 :version 1
                                                 :uri uri)
                   :content-changes content-changes))))

(defun make-range (start-position end-position)
  (make-instance 'lsp:range
                 :start start-position
                 :end end-position))

(defun make-position (line character)
  (make-instance 'lsp:position
                 :line line
                 :character character))

(defun make-content-change (text range)
  (make-lsp-map
   :text text
   :range range))

(test "textDocument/didChange"
  (flet ((make-document (text)
           (call-did-open-text-document-request :uri "file:///hoge/piyo/foo.lisp"
                                                :language-id "lisp"
                                                :version 1
                                                :text text))
         (change-content (content-change)
           (call-did-change-text-document-request "file:///hoge/piyo/foo.lisp"
                                                  content-change))
         (get-text ()
           (let ((text-document
                   (find-text-document (make-instance 'lsp:text-document-identifier
                                                      :uri "file:///hoge/piyo/foo.lisp"))))
             (lem:buffer-text (text-document-buffer text-document)))))

    (test "Change the whole document"
      (with-mock-server ()
        (call-initialize-request)
        (make-document "(list 1 2)")
        (change-content (vector (make-lsp-map :text "x")))
        (ok (equal "x" (get-text)))))

    (test "insert"
      (with-mock-server ()
        (call-initialize-request)
        (make-document (lines "hoge" "piyo" "fuga"))
        (change-content (vector (make-content-change
                                 "abc"
                                 (make-range (make-position 0 0)
                                             (make-position 0 0)))
                                (make-content-change
                                 "xyz"
                                 (make-range (make-position 1 0)
                                             (make-position 1 0)))))
        (ok (equal (lines "abchoge" "xyzpiyo" "fuga")
                   (get-text)))))

    (test "delete"
      (with-mock-server ()
        (call-initialize-request)
        (make-document (lines "hoge" "piyo" "fuga"))
        (change-content (vector
                         (make-content-change
                          ""
                          (make-range (make-position 1 0)
                                      (make-position 1 2)))))
        (ok (equal (lines "hoge" "yo" "fuga") (get-text)))
        (change-content (vector
                         (make-content-change
                          ""
                          (make-range (make-position 1 0)
                                      (make-position 2 0)))))
        (ok (equal (lines "hoge" "fuga")
                   (get-text)))))))

(test "textDocument/didClose"
  (with-mock-server ()
    (call-initialize-request)
    (call-did-open-text-document-request :uri "file:///hoge/piyo/foo.lisp"
                                         :language-id "lisp"
                                         :version 1
                                         :text "")
    (call-lsp-method
     (make-instance 'text-document-did-close-request)
     (convert-to-json
      (make-instance 'lsp:did-close-text-document-params
                     :text-document (make-instance 'lsp:text-document-identifier
                                                   :uri "file:///hoge/piyo/foo.lisp"))))
    (ok (null (find-text-document (make-instance 'lsp:text-document-identifier
                                                 :uri "file:///hoge/piyo/foo.lisp"))))))

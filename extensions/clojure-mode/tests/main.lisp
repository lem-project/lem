(defpackage :lem-clojure-mode/tests/main
  (:use :cl :rove :lem)
  (:import-from :lem-clojure-mode
                :clojure-guess-namespace
                :*clojure-syntax-table*))
(in-package :lem-clojure-mode/tests/main)

(defun setup-clojure-buffer (text)
  "Create a test buffer with Clojure syntax and the given TEXT."
  (let ((buffer (make-buffer "*clojure test*" :syntax-table *clojure-syntax-table*)))
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    buffer))

(defun cleanup-buffer (buffer)
  "Delete the test BUFFER."
  (delete-buffer buffer))

(defmacro with-clojure-buffer ((buffer-var text) &body body)
  "Execute BODY with a temporary Clojure buffer containing TEXT."
  `(let ((,buffer-var (setup-clojure-buffer ,text)))
     (unwind-protect
         (progn ,@body)
       (cleanup-buffer ,buffer-var))))

(deftest clojure-guess-namespace-basic
  (testing "simple namespace"
    (with-clojure-buffer (buffer "(ns foo.bar)")
      (ok (equal "foo.bar"
                 (clojure-guess-namespace (buffer-point buffer))))))

  (testing "namespace with nested names"
    (with-clojure-buffer (buffer "(ns my.app.core)")
      (ok (equal "my.app.core"
                 (clojure-guess-namespace (buffer-point buffer))))))

  (testing "namespace with single segment"
    (with-clojure-buffer (buffer "(ns myapp)")
      (ok (equal "myapp"
                 (clojure-guess-namespace (buffer-point buffer)))))))

(deftest clojure-guess-namespace-with-forms
  (testing "namespace with require"
    (with-clojure-buffer (buffer "(ns foo.bar
  (:require [clojure.string :as str]))")
      (ok (equal "foo.bar"
                 (clojure-guess-namespace (buffer-point buffer))))))

  (testing "namespace with multiple clauses"
    (with-clojure-buffer (buffer "(ns my.app.handler
  (:require [ring.util.response :refer [response]]
            [compojure.core :refer [defroutes GET POST]])
  (:import [java.util Date]))")
      (ok (equal "my.app.handler"
                 (clojure-guess-namespace (buffer-point buffer)))))))

(deftest clojure-guess-namespace-with-code-before
  (testing "namespace after shebang"
    (with-clojure-buffer (buffer "#!/usr/bin/env clojure
(ns script.main)")
      (ok (equal "script.main"
                 (clojure-guess-namespace (buffer-point buffer))))))

  (testing "namespace after comments"
    (with-clojure-buffer (buffer ";; This is a comment
;; Another comment
(ns commented.ns)")
      (ok (equal "commented.ns"
                 (clojure-guess-namespace (buffer-point buffer)))))))

(deftest clojure-guess-namespace-no-namespace
  (testing "no namespace in buffer"
    (with-clojure-buffer (buffer "(defn hello []
  (println \"Hello, World!\"))")
      (ok (null (clojure-guess-namespace (buffer-point buffer))))))

  (testing "empty buffer"
    (with-clojure-buffer (buffer "")
      (ok (null (clojure-guess-namespace (buffer-point buffer)))))))

(deftest clojure-guess-namespace-cursor-position
  (testing "cursor at middle of buffer"
    (with-clojure-buffer (buffer "(ns position.test)

(defn foo []
  :bar)")
      (let ((point (buffer-point buffer)))
        ;; Move cursor to end of buffer
        (buffer-end point)
        (ok (equal "position.test"
                   (clojure-guess-namespace point)))))))

(defpackage :lem-tests/command-line-arguments
  (:use :cl
        :rove))
(in-package :lem-tests/command-line-arguments)

(defun command-line-arguments-equal (arg1 arg2)
  (and (equal (lem-core::command-line-arguments-help arg2)
              (lem-core::command-line-arguments-help arg1))
       (equal (lem-core::command-line-arguments-debug arg2)
              (lem-core::command-line-arguments-debug arg1))
       (equal (lem-core::command-line-arguments-version arg2)
              (lem-core::command-line-arguments-version arg1))
       (equal (lem-core::command-line-arguments-no-init-file arg2)
              (lem-core::command-line-arguments-no-init-file arg1))
       (equal (lem-core::command-line-arguments-log-filename arg2)
              (lem-core::command-line-arguments-log-filename arg1))
       (equal (lem-core::command-line-arguments-interface arg2)
              (lem-core::command-line-arguments-interface arg1))
       (equal (lem-core::command-line-arguments-filenames arg2)
              (lem-core::command-line-arguments-filenames arg1))))

(deftest parse-args-test
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("foo"))
       (lem-core::make-command-line-arguments
        :filenames '("foo"))))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("foo" "bar"))
       (lem-core::make-command-line-arguments
        :filenames '("foo" "bar"))))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("foo" "bar" "-i" "ncurses"))
       (lem-core::make-command-line-arguments
        :filenames '("foo" "bar")
        :interface :ncurses)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-i" "ncurses" "foo" "bar"))
       (lem-core::make-command-line-arguments
        :filenames '("foo" "bar")
        :interface :ncurses)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("foo" "-i" "ncurses" "bar"))
       (lem-core::make-command-line-arguments
        :filenames '("foo" "bar")
        :interface :ncurses)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-i" "ncurses"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :interface :ncurses)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-h"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :help t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--help"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :help t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-h" "foo"))
       (lem-core::make-command-line-arguments
        :filenames '("foo")
        :help t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--debug"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :debug t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--debug" "foo"))
       (lem-core::make-command-line-arguments
        :filenames '("foo")
        :debug t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-v"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :version t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--version"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :version t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-q"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :no-init-file t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--no-init-file"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :no-init-file t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--log-filename" "test.log"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :log-filename "test.log")))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--log-filename" "test.log" "foo.txt"))
       (lem-core::make-command-line-arguments
        :filenames '("foo.txt")
        :log-filename "test.log")))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-i" "sdl2"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :interface :sdl2)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--interface" "sdl2"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :interface :sdl2)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--debug" "--no-init-file" "foo.txt"))
       (lem-core::make-command-line-arguments
        :filenames '("foo.txt")
        :debug t
        :no-init-file t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-h" "-v" "--debug"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :help t
        :version t
        :debug t))))

(deftest parse-args-error-test
  (testing "missing argument for --log-filename"
    (ok (signals (lem-core::parse-args '("--log-filename")) 'lem-core::command-line-arguments-error)))
  (testing "missing argument for -i"
    (ok (signals (lem-core::parse-args '("-i")) 'lem-core::command-line-arguments-error)))
  (testing "missing argument for --interface"
    (ok (signals (lem-core::parse-args '("--interface")) 'lem-core::command-line-arguments-error))))

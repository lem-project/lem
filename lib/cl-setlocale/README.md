# cl-setlocale
[![Build Status](https://travis-ci.com/shamazmazum/cl-setlocale.svg?branch=master)](https://travis-ci.com/shamazmazum/cl-setlocale)

This library is a tiny wrapper around setlocale(3) and can be used in
conjunction with other FFI wrappers like cl-charms.

Examples:

~~~~
* (asdf:load-system :cl-setlocale)
T
* (cl-setlocale:setlocale :lc-all "")
T
"ru_RU.UTF-8"
* (cl-setlocale:setlocale :lc-time "C")
T
"C"
~~~~

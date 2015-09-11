# Inquisitor

[![Build Status](https://travis-ci.org/t-sin/inquisitor.svg)](https://travis-ci.org/t-sin/inquisitor)
[![Circle CI](https://circleci.com/gh/t-sin/inquisitor.svg?style=svg)](https://circleci.com/gh/t-sin/inquisitor)
[![Coverage Status](https://coveralls.io/repos/t-sin/inquisitor/badge.svg?branch=master&service=github)](https://coveralls.io/github/t-sin/inquisitor?branch=master)

Encoding/end-of-line detecter and wrapper of external-format for Common Lisp.


> The Library is a sphere whose exact center is any one of its hexagons and whose circumference is inaccessible.
> -- ["The Library of Babel" by Jorge Luis Borges](http://hyperdiscordia.crywalt.com/library_of_babel.html)


## Goal

* encoding/end-of-line detection
* external-format abstraction
  * make external-format for each implementations
  * make external-format from byte-array, stream and pathname (with auto-detection)
* many implementations (installable with [CIM](https://github.com/KeenS/CIM)) support
  * GNU CLISP
  * Embeddable Common Lisp
  * Steel Bank Common Lisp
  * Clozure CL
  * Armed Bear Common Lisp


## Installation

Put in ASDF-path and type your REPL:

    (require :inquisitor)


## Usage

### Detecting encoding

To detect encoding, use `(inquisitor:detect-encoding stream scheme)`.
About `scheme', see `Encoding scheme`.

for example:

    (with-open-file (in "/path/to/utf8-lf.ja"
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (inquisitor:detect-encoding in :jp))
    =>:UTF-8 ; SBCL's external-format

#### Encoding scheme

Scheme is a language speaking-world to detect encoding.
Supported scheme is as follows:

* :jp -- japanese
* :tw -- taiwanese
* :cn -- chinese
* :kr -- korean
* :ru -- russian
* :ar -- arabic
* :tr -- turkish
* :gr -- greek
* :hw -- hebrew
* :pl -- polish
* :bl -- baltic


### Detecting end-of-line type

    (with-open-file (in "/path/to/utf8-lf.ja"
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (inquisitor:detect-end-of-line in))
    =>:LF
      :CANNOT-TREAT ; SBCL can't treat end-of-line with external-format

#### If you want to know eol is available on your implementation

Use `inquisitor.eol:eol-available-p`.


### Making external-format implementation independently

    (inquisitor:make-external-format
      (inquisitor.keyword:utf8-keyword) ; implementation independent name of UTF-8
      (inquisitor.keyword:lf-keyword)) ; implementation independent name of LF
    =>:UTF-8 ; on SBCL
    =>#<EXTERNAL-FORMAT :CP932/:DOS #xxxxxxxxxxx> ; on CCL


#### Auto detecting and making external-format, from vector, stream and pathname

In case of vector (on CCL):

    (inquisitor:detect-external-format
              (encode-string-to-octets "公的な捜索係、調査官がいる。
    わたしは彼らが任務を遂行しているところを見た。")
              :jp)
    =>#<EXTERNAL-FORMAT :UTF-8/:UNIX #xxxxxxxxxx>

In case of stream (on CCL):

    (with-open-file (in "/path/to/utf8-lf.ja"
                     :direction :input
                     :element-type '(unsigned-byte 8))
       (inquisitor:detect-external-format in :jp)
    =>#<EXTERNAL-FORMAT :UTF-8/:UNIX #xxxxxxxxxx>

In case of pathname (on CCL):

    (inquisitor:detect-external-format #P"/path/to/utf8-lf.ja" :jp)
    =>#<EXTERNAL-FORMAT :UTF-8/:UNIX #xxxxxxxxxx>


## Author

* [Shiro Kawai](https://github.com/shirok) - original code of encoding detection for [Gauche](https://github.com/shirok/Gauche/tree/master/ext/charconv)
* [Masayuki Onjo](http://lispuser.net/index) - [porting](http://lispuser.net/commonlisp/japanese.html#sec-2.1) from Gauche to Common Lisp
* [zqwell](https://github.com/zqwell) - [porting](https://github.com/zqwell/guess) multilingual encoding detection from [libguess](https://github.com/kaniini/libguess)
* gray (shinichi.tanaka45@gmail.com) - adding end-of-line detection and wrapping external-format


## Copyright

Copyright (c) 2000-2007 Shiro Kawai (shiro@acm.org)  
Copyright (c) 2007 Masayuki Onjo (onjo@lispuser.net)  
Copyright (c) 2011 zqwell (zqwell@gmail.com)  
Copyright (c) 2015 gray (shinichi.tanaka45@gmail.com)


## License

Licensed under the MIT License.

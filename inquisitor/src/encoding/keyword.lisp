#|
  This code is a part of inquisitor project
  and a derivate from guess (https://github.com/zqwell/guess).
|#
;;; This code is derivative of libguess-1.0 and guess-0.1.0 for common lisp.
;;; 
;;; Copyright (c) 2011 zqwell <zqwell@gmail.com>
;;; 
;;; The following is the original copyright notice.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;; 3. Neither the name of the authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this
;;;    software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
;;; 

(in-package :cl-user)
(defpackage inquisitor.encoding.keyword
  (:use :cl)
  (:export :enc-name->keyword

           ; unicode 
           :utf8-keyword
           :ucs-2le-keyword
           :ucs-2be-keyword
           :utf16-keyword

           ; japanese
           :iso-2022-jp-keyword
           :eucj-keyword
           :sjis-keyword

           ; taiwanese
           :big5-keyword
           :iso-2022-tw-keyword

           ; chinese
           :gb2312-keyword
           :gb18030-keyword
           :iso-2022-cn-keyword

           ; korean
           :euck-keyword
           :johab-keyword
           :iso-2022-kr-keyword

           ; arabic
           :iso8859-6-keyword
           :cp1256-keyword

           ; greek
           :iso8859-7-keyword
           :cp1253-keyword

           ; hebrew
           :iso8859-8-keyword
           :cp1255-keyword

           ; turkish
           :iso8859-9-keyword
           :cp1254-keyword

           ; russian
           :iso8859-5-keyword
           :koi8-r-keyword
           :koi8-u-keyword
           :cp866-keyword
           :cp1251-keyword

           ; polish
           :iso8859-2-keyword
           :cp1250-keyword

           ; baltic
           :iso8859-13-keyword
           :cp1257-keyword))
(in-package :inquisitor.encoding.keyword)



;;; eval-when is needed to compile generate-order
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun enc-name->keyword (enc)
    (funcall (symbol-function (find-symbol (string-upcase (format nil "~A-keyword" (symbol-name enc))) :inquisitor.encoding.keyword))))
  
;;;; japanese (:jp)
  (defun iso-2022-jp-keyword () ;; jis
    ;; #+allegro :jis
    ;; #+lispworks :jis
    #+clisp 'charset:iso-2022-jp
    #+ecl :iso-2022-jp
    #+abcl :iso-2022-jp
    #+(or sbcl ccl) (values :jis ; these implementations cannot treat JIS
                            :cannot-treat)
    #-(or clisp ecl sbcl ccl abcl) :jis)

  ;; (defun jis-keyword ())

  (defun eucj-keyword ()
    ;; #+lispworks :euc-jp
    #+clisp 'charset:euc-jp
    #+ecl (values :euc-jp ; ecl cannot treat EUC-JP
                  :cannot-treat)
    ;; #+sbcl :euc-jp
    ;; #+ccl :euc-jp
    ;; #+abcl :euc-jp
    #-(or clisp ecl) :euc-jp)
  (defun sjis-keyword ()
    ;; #+lispworks :sjis
    #+clisp 'charset:shift-jis
    #+ecl :cp932
    ;; #+sbcl :sjis
    #+ccl :cp932
    #+abcl :|Shift_JIS|
    #-(or clisp ecl ccl) :sjis)
  (defun utf8-keyword ()
    ;; #+lispworks :utf-8
    #+clisp 'charset:utf-8
    ;; #+ecl :utf-8
    ;; #+sbcl :utf-8
    ;; #+ccl :utf-8
    ;; #+abcl :utf-8
    #-clisp :utf-8)

;;;; unicode
  (defun ucs-2le-keyword ()
    ;; #+lispworks :unicode ; default endian is :little-endian
    #+clisp 'charset:unicode-16-little-endian
    ;; #+ecl :ucs-2le
    ;; #+sbcl :ucs-2le
    ;; #+ccl :ucs-2le
    #+abcl :utf-16le
    #-(or clisp abcl) :ucs-2le)
  (defun ucs-2be-keyword ()
    ;; #+lispworks :unicode ; default endian is :little-endian
    #+clisp 'charset:unicode-16-big-endian
    ;; #+ecl :ucs-2be
    ;; #+sbcl :ucs-2be
    ;; #+ccl :ucs-2be
    #+abcl :utf-16be
    #-(or clisp abcl) :ucs-2be)
  (defun utf16-keyword ()
    ;; #+lispworks nil
    #+clisp 'charset:utf-16
    ;; #+ecl nil
    #+sbcl :utf-16be ; or :utf-16le
    #+ccl :utf-16
    #+abcl :utf-16
    #-(or clisp sbcl ccl) nil)

;;;; taiwanese (:tw)
  (defun big5-keyword ()
    #+clisp 'charset:big5
    #+ecl :cp950
    #+abcl :|Big5|
    #+(or sbcl ccl) (values :big5 ; these implementations cannot treat big5
                            :cannot-treat)
    #-(and clisp sbcl) :big5)
  (defun iso-2022-tw-keyword ()
    #+(or clisp ecl sbcl ccl abcl) ; these implementations cannot treat iso-2022-tw
    (values :iso-2022-tw
            :cannot-treat)
    #-(or clisp ecl sbcl ccl abcl) :iso-2022-tw)


;;;; chinese (:cn)
  (defun gb2312-keyword ()
    #+clisp 'charset:euc-cn
    ; #+ccl :gb2312
    ; #+abcl :gb2312
    #+(or ecl sbcl) ; these implementations cannot treat gb2312 (EUC-CN)
    (values :gb2312 ; but ecl, sbcl (and ccl) can treat GBK
            :cannot-treat)
    #-(or clisp ecl sbcl) :gb2312)
  (defun gb18030-keyword ()
    #+clisp 'charset:gb18030
    ; #+abcl :gb18030
    #+(or ecl sbcl ccl) ; these implementations cannot treat gb18030
    (values :gb18030
            :cannot-treat)
    #-(or clisp ecl sbcl ccl) :gb18030)
  (defun iso-2022-cn-keyword ()
    #+clisp 'charset:iso-2022-cn
    #+(or ecl sbcl ccl) ; these implementations cannot treat iso-2022-cn
    (values :iso-2022-cn :cannot-treat)
    ; #+abcl :iso-2022-cn
    #-(or clisp ecl sbcl ccl) :iso-2022-cn)
  
;;;; korean (:kr)
  (defun euck-keyword ()
    #+clisp 'charset:euc-kr
    ; #+abcl :euc-kr
    #+(or ecl sbcl ccl) ; these implementations cannot treat euc-kr
    (values :euc-kr :cannot-treat)
    #-(or clisp ecl sbcl ccl) :euc-kr)
  (defun johab-keyword ()
    #+clisp 'charset:johab
    #+ecl :cp949
    #+abcl :|x-Johab|
    #+(or sbcl ccl) ; these implementations cannot treat johab
    (values :johab :cannot-treat)
    #-(or clisp ecl sbcl ccl abcl) :johab)
  (defun iso-2022-kr-keyword ()
    #+clisp 'charset:iso-2022-kr
    #+(or ecl sbcl ccl abcl) ; these implementations cannot treat iso-2022-kr
    (values :iso-2022-kr :cannot-treat)
    #-(or clisp ecl sbcl ccl abcl) :iso-2022-kr)
  
;;;; arabic (:ar)
  (defun iso8859-6-keyword ()
    #+clisp 'charset:iso-8859-6
    #+ecl :iso-8859-6
    ; #+sbcl :iso-8859-6
    ; #+ccl :iso-8859-6
    ; #+abcl :iso-8859-6
    #+(or sbcl ccl abcl) :iso-8859-6
    #-(or clisp sbcl ccl abcl ecl) :iso8859-6)
  (defun cp1256-keyword ()
    #+clisp 'charset:cp1256
    #+ecl :ms-arab
    ; #+sbcl :cp1256
    #+ccl (values :cp1256
                  :cannot-treat)
    #+abcl :|windows-1256|
    #-(or clisp ecl ccl abcl) :cp1256)

;;;; greek (:gr)
  (defun iso8859-7-keyword ()
    #+clisp 'charset:iso-8859-7
    #+ecl :iso-8859-7
    ; #+sbcl :iso-8859-7
    ; #+ccl :iso-8859-7
    ; #+abcl :iso-8859-7
    #-(or clisp ecl) :iso-8859-7)
  (defun cp1253-keyword ()
    #+clisp 'charset:cp1253
    #+ecl :ms-greek
    #+sbcl :cp1253
    #+ccl (values :cp1253
                  :cannot-treat)
    #+abcl  :|windows-1253|
    #-(or clisp ecl ccl abcl) :cp1253)

;;;; hebrew (:hw)
  (defun iso8859-8-keyword ()
    #+clisp 'charset:iso-8859-8
    #+ecl :iso-8859-8
    ; #+sbcl :iso-8859-8
    ; #+ccl :iso-8859-8
    ; #+abcl :iso-8859-8
    #+(or sbcl ccl abcl) :iso-8859-8
    #-(or clisp ecl sbcl ccl abcl) :iso8859-8)
  (defun cp1255-keyword ()
    #+clisp 'charset:cp1255
    #+ecl :ms-hebr
    ; #+sbcl :cp1255
    #+ccl (values :cp1255
                  :cannot-treat)
    #+abcl :|windows-1255|
    #-(or clisp ecl ccl abcl) :cp1255)

;;;; turkish (:tr)
  (defun iso8859-9-keyword ()
    #+clisp 'charset:iso-8859-9
    #+ecl :iso-8859-9
    ; #+sbcl :iso-8859-9
    ; #+ccl :iso-8859-9
    ; #+abcl :iso-8859-9
    #+(or sbcl ccl abcl) :iso-8859-9
    #-(or clisp ecl sbcl ccl abcl) :iso8859-9)
  (defun cp1254-keyword ()
    #+clisp 'charset:cp1254
    #+ecl :ms-turk
    ; #+sbcl :cp1254
    #+ccl (values :cp1254
                  :cannot-treat)
    #+abcl :|windows-1254|
    #-(or clisp ecl ccl abcl) :cp1254)

;;;; russian (:ru)
  (defun iso8859-5-keyword ()
    #+clisp 'charset:iso-8859-5
    #+ecl :iso-8859-5
    ; #+sbcl :iso-8859-5
    ; #+ccl :iso-8859-5
    ; #+abcl :iso-8859-5
    #+(or sbcl ccl abcl) :iso-8859-5
    #-(or clisp ecl sbcl ccl abcl) :iso8859-5)
  (defun koi8-r-keyword ()
    #+clisp 'charset:koi8-r
    #+sbcl :koi8-r
    #+(or ecl ccl) (values :koi8-r
                           :cannot-treat)
    ; #+abcl :koi8-r
    #-(or clisp sbcl ecl ccl) :koi8-r)
  (defun koi8-u-keyword ()
    #+clisp 'charset:koi8-u
    ; #+sbcl :koi8-u
    ; #+abcl :koi8-u
    #+(or ecl ccl) (values :koi8-u
                           :cannot-treat)
    #-(or clisp ecl ccl) :koi8-u)
  (defun cp866-keyword ()
    #+clisp 'charset:cp866
    #+ecl :cp866
    ; #+sbcl :cp866
    #+ccl (values :cp866
                  :cannot-treat)
    #+abcl :ibm866
    #-(or clisp ecl ccl abcl) :cp866)
  (defun cp1251-keyword ()
    #+clisp 'charset:cp1251
    #+ecl :ms-cyrl
    ; #+sbcl :cp1251
    #+ccl (values :cp1251
                  :cannot-treat)
    #+abcl :|windows-1251|
    #-(or clisp ecl ccl abcl) :cp1251)

;;;; polish (:pl)
  (defun iso8859-2-keyword ()
    #+clisp 'charset:iso-8859-2
    #+ecl :iso-8859-2
    ; #+sbcl :iso-8859-2
    ; #+ccl :iso-8859-2
    ; #+abcl :iso-8859-2
    #+(or sbcl ccl abcl) :iso-8859-2
    #-(or clisp ecl sbcl ccl abcl) :iso8859-2)
  (defun cp1250-keyword ()
    #+clisp 'charset:cp1250
    #+ecl :ms-ee
    ; #+sbcl :cp1250
    #+ccl (values :cp1250
                  :cannot-treat)
    #+abcl :|windows-1250|
    #-(or clisp ecl ccl abcl) :cp1250)

;;;; baltic (:bl)
  (defun iso8859-13-keyword ()
    #+clisp 'charset:iso-8859-13
    #+ecl :iso-8859-13
    ; #+sbcl :iso-8859-13
    ; #+ccl :iso-8859-13
    ; #+abcl :iso-8859-13
    #+(or sbcl ccl abcl) :iso-8859-13
    #-(or clisp ecl) :iso8859-13)
  (defun cp1257-keyword ()
    #+clisp 'charset:cp1257
    #+ecl :winbaltrim
    ; #+sbcl :cp1257
    #+ccl (values :cp1257
                  :cannot-treat)
    #+abcl :|windows-1257|
    #-(or clisp ecl ccl abcl) :cp1257)
  ) ;; eval-when

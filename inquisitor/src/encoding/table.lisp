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
(defpackage inquisitor.encoding.table
  (:use :cl)
  (:import-from :inquisitor.encoding.dfa
                :define-dfa))
(in-package :inquisitor.encoding.table)


;;;;; state date from Gauche's guess.scm

;;;============================================================
;;; DFA definitions
;;;

;;;
;;; EUC-JP
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-dfa eucj
    ;; first byte
    (init
     (((#x00 #x7f)) init         1.0)   ; ASCII range
     ((#x8e)        jis0201_kana 0.8)   ; JISX 0201 kana
     ((#x8f)        jis0213_2    0.95)  ; JISX 0213 plane 2
     (((#xa1 #xfe)) jis0213_1    1.0))  ; JISX 0213 plane 1
    ;; jis x 0201 kana
    (jis0201_kana
     (((#xa1 #xdf)) init         1.0))
    ;; jis x 0208 and jis x 0213 plane 1
    (jis0213_1
     (((#xa1 #xfe)) init         1.0))
    ;; jis x 0213 plane 2
    (jis0213_2
     (((#xa1 #xfe)) init         1.0)))

;;;
;;; Shift_JIS
;;;

  (define-dfa sjis
    ;; first byte
    (init
     (((#x00 #x7f)) init         1.0)        ;ascii
     (((#x81 #x9f) (#xe1 #xef)) jis0213 1.0) ;jisx0213 plane 1
     (((#xa1 #xdf)) init         0.8)        ;jisx0201 kana
     (((#xf0 #xfc)) jis0213      0.95)       ;jisx0213 plane 2
     (((#xfd #xff)) init         0.8))       ;vendor extension
    (jis0213
     (((#x40 #x7e) (#x80 #xfc)) init 1.0)))

;;;
;;; UTF-8
;;;

  (define-dfa utf8
    (init
     (((#x00 #x7f)) init         1.0)
     (((#xc2 #xdf)) 1byte_more   1.0)
     (((#xe0 #xe0)) 2byte_more1  1.0)
     (((#xe1 #xec)) 2byte_more2  1.0)
     (((#xed #xed)) 2byte_more3  1.0)
     (((#xee #xef)) 2byte_more2  1.0)
     (((#xf0 #xf0)) 3byte_more1  1.0)
     (((#xf1 #xf3)) 3byte_more2  1.0)
     (((#xf4 #xf4)) 3byte_more3  1.0))
    (1byte_more
     (((#x80 #xbf)) init         1.0))
    (2byte_more1
     (((#xa0 #xbf)) 1byte_more   1.0))
    (2byte_more2
     (((#x80 #xbf)) 1byte_more   1.0))
    (2byte_more3
     (((#x80 #x9f)) 1byte_more   1.0))
    (3byte_more1
     (((#x90 #xbf)) 2byte_more2  1.0))
    (3byte_more2
     (((#x80 #xbf)) 2byte_more2  1.0))
    (3byte_more3
     (((#x80 #x8f)) 2byte_more2  1.0)))

;;;
;;; UCS-2LE
;;;
  (define-dfa ucs2le
    (init
     ((#xfe) bom-be 1.0)
     ((#xff) bom-le 1.0)
     (((#x00 #xfd)) byte2 1.0))
    (bom-le
     (((#x00 #xff)) init 1.0))
    (bom-be
     (((#x00 #xfe)) init 1.0))  ;; if be (0xfeff), die.
    (byte2
     (((#x00 #xff)) init 1.0)))

;;;
;;; UCS-2BE
;;;
  (define-dfa ucs2be
    (init
     ((#xfe) bom-be 1.0)
     ((#xff) bom-le 1.0)
     (((#x00 #xfd)) byte2 1.0))
    (bom-le
     (((#x00 #xfd)) init 1.0)
     ((#xff) init 1.0)) ;; if le (0xfffe), die.
    (bom-be
     (((#x00 #xff)) init 1.0))
    (byte2
     (((#x00 #xff)) init 1.0)))

;;;
;;; UTF-16
;;;
  (define-dfa utf16
    (init
     ((#xfe) bom-be 1.0)
     ((#xff) bom-le 1.0))
    (init1
     (((#x00 #xff)) byte2 1.0))
    (bom-be
     ((#xff) init1 1.0))
    (bom-le
     ((#xfe) init1 1.0))
    (byte2
     (((#x00 #xff)) init1 1.0)))

;;;
;;; ISO2022JP (JIS)
;;;

  ;; NB: for now, we just check the sequence of <ESC> $ or <ESC> '('.
  (define-dfa jis
    (init
     ((#x1b)        esc          1.0)
     (((#x00 #x1a)  (#x1c #x1f)) init 1.0) ;C0
     (((#x20 #x7f)) init         1.0)      ;ASCII
     (((#xa1 #xdf)) init         0.7))     ;JIS8bit kana
    (esc
     ((#x0d #x0a)   init         0.9)      ;cancel
     ((#\( )        esc-paren    1.0)
     ((#\$ )        esc-$        1.0)
     ((#\& )        esc-&        1.0))
    (esc-paren
     ((#\B #\J #\H) init         1.0)
     ((#\I)         jis0201kana  0.8))
    (esc-$
     ((#\@ #\B)     kanji        1.0)
     ((#\( )        esc-$-paren  1.0))
    (esc-$-paren
     ((#\D #\O #\P) kanji        1.0))
    (esc-&
     ((#\@ )        init         1.0))
    (jis0201kana
     ((#x1b)        esc          1.0)
     (((#x20 #x5f)) jis0201kana  1.0))
    (kanji
     ((#x1b)        esc          1.0)
     (((#x21 #x7e)) kanji-2      1.0))
    (kanji-2
     (((#x21 #x7e)) kanji        1.0)) )

;;;
;;; Big5
;;;

  (define-dfa big5
    ;; first byte
    (init
     (((#x00 #x7f)) init         1.0)      ;ascii
     (((#xa1 #xfe)) 2byte        1.0))     ;big5-2byte
    (2byte
     (((#x40 #x7e) (#xa1 #xfe)) init 1.0)))

;;;
;;; GB2312 (EUC-CN?)
;;;

  (define-dfa gb2312
    ;; first byte
    (init
     (((#x00 #x7f)) init         1.0)      ;ascii
     (((#xa1 #xfe)) 2byte        1.0))     ;gb2312 2byte
    (2byte
     (((#xa1 #xfe)) init 1.0)))

;;;
;;; GB18030
;;;

  (define-dfa gb18030
    ;; first byte
    (init
     (((#x00 #x80)) init         1.0)     ;ascii
     (((#x81 #xfe)) 2byte        1.0)     ;gb18030 2byte
     (((#x81 #xfe)) 4byte2       1.0))     ;gb18030 2byte
    (2byte
     (((#x40 #x7e) (#x80 #xfe)) init 1.0))
    (4byte2
     (((#x30 #x39)) 4byte3 1.0))
    (4byte3
     (((#x81 #xfe)) 4byte4 1.0))
    (4byte4
     (((#x30 #x39)) init   1.0)) )

;;;
;;; EUC-KR
;;;

  (define-dfa euck
    ;; first byte
    (init
     (((#x00 #x7f)) init      1.0)   ; ASCII range
     (((#xa1 #xfe)) ks1001    1.0))   ; KSX 1001
    ;; ks x 1001
    (ks1001
     (((#xa1 #xfe)) init      1.0)))

;;;
;;; Johab
;;;

  (define-dfa johab
    ;; first byte
    (init
     (((#x00 #x7f)) init         1.0)   ; ASCII range
     (((#x84 #xd3)) jamo51       1.0)   ; jamo51
     (((#xd8 #xde) (#xe0 #xf9)) jamo42  0.95))   ; jamo42
    ;; second byte
    (jamo51
     (((#x41 #x7e) (#x81 #xfe)) init         1.0))
    (jamo42
     (((#x31 #x7e) (#x91 #xfe)) init         1.0)))






;;;
;;; arabic
;;;

  (define-dfa iso8859-6
    (init
     (((#x00 #x7f)) init         1.0)     ;ascii
     ((#xa0)        init         1.0)
     ((#xa4)        init         1.0)
     ((#xac)        init         1.0)
     ((#xad)        init         1.0)     ;SHY xxx
     ((#xbb)        init         1.0)
     ((#xbf)        init         1.0)
     (((#xc1 #xda)) init         1.0)
     (((#xe0 #xf2)) init         1.0)))

  (define-dfa cp1256
    (init
     (((#x00 #x7f)) init         1.0)     ;ascii
     (((#x80 #xff)) init         1.0)))   ;high bit


;;;
;;; greek
;;;

  (define-dfa iso8859-7
    (init
     (((#x00 #x7f)) init         1.0)     ;ascii
     (((#xa0 #xad)) init         1.0)
     (((#xaf #xd1)) init         1.0)
     (((#xd3 #xfe)) init         1.0)))

  (define-dfa cp1253
    (init
     (((#x00 #x7f)) init         1.0)     ;ascii
     ((#x80)        init         1.0)
     (((#x82 #x87)) init         1.0)
     ((#x89)        init         1.0)
     ((#x8b)        init         1.0)
     (((#x91 #x97)) init         1.0)
     ((#x99)        init         1.0)
     ((#x9b)        init         1.0)
     (((#xa0 #xa9)) init         1.0)
     (((#xab #xd1)) init         1.0)
     (((#xd3 #xfe)) init         1.0)))

;;;
;;; hebrew
;;;

  (define-dfa iso8859-8
    (init
     (((#x00 #x7f)) init         1.0)     ;ascii
     ((#xa0)        init         1.0)
     (((#xa2 #xbe)) init         1.0)
     (((#xdf #xfa)) init         1.0)
     (((#xfd #xfe)) init         1.0)))

  (define-dfa cp1255
    (init
     (((#x00 #x7f)) init         1.0)     ;ascii
     ((#x80)        init         1.0)
     (((#x82 #x89)) init         1.0)
     ((#x8b)        init         1.0)
     (((#x91 #x99)) init         1.0)
     ((#x9b)        init         1.0)
     (((#xa0 #xc9)) init         1.0)
     (((#xcb #xd8)) init         1.0)
     (((#xe0 #xfa)) init         1.0)
     (((#xfd #xfe)) init         1.0)))

;;;
;;; turkish
;;;

  (define-dfa iso8859-9
    (init
     (((#x00 #x7f)) init         1.0)     ;ascii
     (((#xa0 #xff)) init         1.0)))

  (define-dfa cp1254
    (init
     (((#x00 #x7f)) init         1.0)     ;ascii
     ((#x80)        init         1.0)
     (((#x82 #x8c)) init         1.0)
     (((#x91 #x9c)) init         1.0)
     (((#x9f #xff)) init         1.0)))

;;;
;;; russian
;;;

  (define-dfa iso8859-5
    (init
     (((#x00 #x7f)) init         1.0)     ; ascii
     (((#xa0 #xff)) init         1.0)))
  
  (define-dfa koi8-r
    (init
     (((#x20 #x7f)) init         1.0)     ; ascii
     ((#xa3)        init         1.0)
     ((#xb3)        init         1.0)
     (((#xc0 #xff)) init         1.0)))

  (define-dfa koi8-u
    (init
     (((#x20 #x7f)) init         1.0)     ; ascii
     (((#xa3 #xa4)) init         1.0)
     (((#xa6 #xa7)) init         1.0)
     ((#xad)        init         1.0)
     (((#xb3 #xb4)) init         1.0)
     (((#xb6 #xb7)) init         1.0)
     ((#xbd)        init         1.0)
     (((#xc0 #xff)) init         1.0)))

  (define-dfa cp866
    (init
     (((#x00 #x7f)) init         1.0)     ; ascii
     (((#x80 #xaf)) init         1.0)
     (((#xe0 #xf7)) init         1.0)))

  (define-dfa cp1251
    (init
     (((#x00 #x7f)) init         1.0)     ; ascii
     (((#x80 #x81)) init         1.0)
     ((#x83)        init         1.0)
     ((#x8a)        init         1.0)
     (((#x8c #x90)) init         1.0)
     ((#x9a)        init         1.0)
     (((#x9c #xa3)) init         1.0)
     ((#xa5)        init         1.0)
     ((#xa8)        init         1.0)
     ((#xaa)        init         1.0)
     ((#xaf)        init         1.0)
     (((#xb2 #xb4)) init         1.0)
     ((#xb8)        init         1.0)
     ((#xba)        init         1.0)
     (((#xbc #xff)) init         1.0)))

;;;
;;; polish
;;;

  (define-dfa iso8859-2
    (init
     (((#x20 #x7e)) init          1.0)
     ((#xa1)        init          1.0)
     ((#xa3)        init          1.0)
     (((#xa5 #xa6)) init          1.0)
     (((#xa9 #xac)) init          1.0)
     (((#xae #xaf)) init          1.0)
     ((#xb1)        init          1.0)
     ((#xb3)        init          1.0)
     (((#xb5 #xb6)) init          1.0)
     (((#xb9 #xbc)) init          1.0)
     (((#xbe #xbf)) init          1.0)
     (((#xc0 #xd6)) init          1.0)
     (((#xd8 #xf6)) init          1.0)
     (((#xf8 #xfe)) init          1.0)))

  (define-dfa cp1250
    (init
     (((#x20 #x7e)) init          1.0)
     (((#x8a #x8f)) init          1.0)
     ((#x9a)        init          1.0)
     (((#x9c #xa1)) init          1.0)
     ((#xa3)        init          1.0)
     ((#xa5)        init          1.0)
     ((#xaa)        init          1.0)
     ((#xaf)        init          1.0)
     ((#xb3)        init          1.0)
     (((#xb9 #xba)) init          1.0)
     ((#xbc)        init          1.0)
     (((#xbe #xbf)) init          1.0)
     (((#xc0 #xd6)) init          1.0)
     (((#xd8 #xf6)) init          1.0)
     (((#xf8 #xfe)) init          1.0)))

;;;
;;; baltic (estonia/latvia/lithuania)
;;;

  (define-dfa iso8859-13
    (init
     (((#x20 #x7e)) init          1.0)
     ((#xa8)        init          1.0)
     ((#xaa)        init          1.0)
     ((#xaf)        init          1.0)
     ((#xb8)        init          1.0)
     ((#xba)        init          1.0)
     ((#xbf)        init          1.0)
     (((#xc0 #xd6)) init          1.0)
     (((#xd8 #xf6)) init          1.0)
     (((#xf8 #xfe)) init          1.0)))

  (define-dfa cp1257
    (init
     (((#x20 #x7e)) init          1.0)
     ((#x80)        init          1.0)
     ((#x82)        init          1.0)
     (((#x84 #x87)) init          1.0)
     ((#x89)        init          1.0)
     ((#x8b)        init          1.0)
     (((#x8d #x8f)) init          1.0)
     (((#x91 #x97)) init          1.0)
     ((#x99)        init          1.0)
     ((#x9b)        init          1.0)
     (((#x9d #x9e)) init          1.0)
     ((#xa0)        init          1.0)
     (((#xa2 #xa4)) init          1.0)
     (((#xa6 #xff)) init          1.0)))
  ) ;; eval-when


(in-package :cl-user)
(defpackage inquisitor.keyword
  (:use :cl)
  (:import-from :inquisitor.encoding.keyword
                :utf8-keyword
                :ucs-2le-keyword
                :ucs-2be-keyword
                :utf16-keyword
                :iso-2022-jp-keyword
                :eucj-keyword
                :sjis-keyword
                :big5-keyword
                :iso-2022-tw-keyword
                :gb2312-keyword
                :gb18030-keyword
                :iso-2022-cn-keyword
                :euck-keyword
                :johab-keyword
                :iso-2022-kr-keyword
                :iso8859-6-keyword
                :cp1256-keyword
                :iso8859-7-keyword
                :cp1253-keyword
                :iso8859-8-keyword
                :cp1255-keyword
                :iso8859-9-keyword
                :cp1254-keyword
                :iso8859-5-keyword
                :koi8-r-keyword
                :koi8-u-keyword
                :cp866-keyword
                :cp1251-keyword
                :iso8859-2-keyword
                :cp1250-keyword
                :iso8859-13-keyword
                :cp1257-keyword)
  (:import-from :inquisitor.eol
                :lf-keyword
                :cr-keyword
                :crlf-keyword)

  (:export ; unicode
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
           :cp1257-keyword

           ; end-of-line
           :lf-keyword
           :cr-keyword
           :crlf-keyword))



(in-package :lem)
(let ((removed (remove "fetch-via-roswell" ql-http:*fetch-scheme-functions*
                      :key 'cdr :test 'string-equal)))
  (setf ql-http:*fetch-scheme-functions*
        (if (find "http" removed :test 'equal :key #'car)
            removed
            (acons "http" 'ql-http:http-fetch removed))))


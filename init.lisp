(setf lem:*find-directory-function* 'lem.dired:dired-buffer)

#+sbcl
(push #'(lambda (x)
          (if x
              (lem:lem x)
              (lem:lem))
          t)
      sb-ext:*ed-functions*)

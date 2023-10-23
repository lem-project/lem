(defpackage :lem-sdl2/clipboard
  (:use :cl
        :lem-sdl2/sdl2))
(in-package :lem-sdl2/clipboard)

#-windows
(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (lem-sdl2/log:with-debug ("clipboard-paste")
    (lem-sdl2::with-renderer ()
      (sdl2-ffi.functions:sdl-get-clipboard-text))))


#+windows
(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (lem-sdl2/log:with-debug ("clipboard-paste")
    (lem-sdl2::with-renderer ()
      (with-output-to-string (out)
        (let ((text (sdl2-ffi.functions:sdl-get-clipboard-text)))
          (loop :for string :in (split-sequence:split-sequence #\newline text)
                :do (if (and (< 0 (length string))
                             (char= #\return (char string (1- (length string)))))
                        (write-line (subseq string 0 (1- (length string))) out)
                        (write-string string out))))))))

(defmethod lem-if:clipboard-copy ((implementation sdl2) text)
  (lem-sdl2/log:with-debug ("clipboard-copy")
    (lem-sdl2::with-renderer ()
      (sdl2-ffi.functions:sdl-set-clipboard-text text))))

(lem:enable-clipboard)

(defpackage :lem-sdl2/font
  (:use :cl)
  (:export :make-font-config
           :open-font
           :merge-font-config))
(in-package :lem-sdl2/font)

(defstruct (font-config (:constructor %make-font-config))
  size
  latin-normal-file
  latin-bold-file
  unicode-normal-file
  unicode-bold-file)

(defun make-font-config (&key size
                              latin-normal-file
                              latin-bold-file
                              unicode-normal-file
                              unicode-bold-file)
  (%make-font-config
   :size (lem:config :font-size (or size 20))
   :latin-normal-file (or latin-normal-file
                          (lem:config :normal-font
                                      (asdf:system-relative-pathname
                                       :lem-sdl2
                                       "resources/NotoSansMono/NotoSansMono-Regular.ttf")))
   :latin-bold-file (or latin-bold-file
                        (lem:config :bold-font
                                    (asdf:system-relative-pathname
                                     :lem-sdl2
                                     "resources/NotoSansMono/NotoSansMono-Bold.ttf")))
   :unicode-normal-file (or unicode-normal-file
                            (lem:config :unicode-normal-font
                                        (asdf:system-relative-pathname
                                         :lem-sdl2
                                         "resources/NotoSansJP/NotoSansJP-Regular.otf")))
   :unicode-bold-file (or unicode-bold-file
                          (lem:config :unicode-bold-font
                                      (asdf:system-relative-pathname
                                       :lem-sdl2
                                       "resources/NotoSansJP/NotoSansJP-Bold.otf")))))

(defun open-font (&optional font-config)
  (let ((font-config (or font-config (make-font-config))))
    (values (sdl2-ttf:open-font (font-config-latin-normal-file font-config)
                                (font-config-size font-config))
            (sdl2-ttf:open-font (font-config-latin-bold-file font-config)
                                (font-config-size font-config))
            (sdl2-ttf:open-font (font-config-unicode-normal-file font-config)
                                (font-config-size font-config))
            (sdl2-ttf:open-font (font-config-unicode-bold-file font-config)
                                (font-config-size font-config)))))

(defun merge-font-config (new old)
  (%make-font-config :size (or (font-config-size new)
                               (font-config-size old))
                     :latin-normal-file (or (font-config-latin-normal-file new)
                                            (font-config-latin-normal-file old))
                     :latin-bold-file (or (font-config-latin-bold-file new)
                                          (font-config-latin-bold-file old))
                     :unicode-normal-file (or (font-config-unicode-normal-file new)
                                              (font-config-unicode-normal-file old))
                     :unicode-bold-file (or (font-config-unicode-bold-file new)
                                            (font-config-unicode-bold-file old))))

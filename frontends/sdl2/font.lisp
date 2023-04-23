(defpackage :lem-sdl2/font
  (:use :cl)
  (:export :font
           :font-latin-normal-font
           :font-latin-bold-font
           :font-cjk-normal-font
           :font-cjk-bold-font
           :font-emoji-font
           :font-char-width
           :font-char-height
           :make-font-config
           :font-config-size
           :merge-font-config
           :change-size
           :open-font
           :close-font))
(in-package :lem-sdl2/font)

(defstruct (font-config (:constructor %make-font-config))
  size
  latin-normal-file
  latin-bold-file
  cjk-normal-file
  cjk-bold-file
  emoji-file)

(defstruct font
  latin-normal-font
  latin-bold-font
  cjk-normal-font
  cjk-bold-font
  emoji-font
  char-width
  char-height)

(defun make-font-config (&key size
                              latin-normal-file
                              latin-bold-file
                              cjk-normal-file
                              cjk-bold-file
                              emoji-file)
  (%make-font-config
   :size (lem:config :font-size (or size 20))
   :latin-normal-file (or latin-normal-file
                          (lem:config :normal-font
                                      (asdf:system-relative-pathname
                                       :lem-sdl2
                                       "resources/fonts/NotoSansMono-Regular.ttf")))
   :latin-bold-file (or latin-bold-file
                        (lem:config :bold-font
                                    (asdf:system-relative-pathname
                                     :lem-sdl2
                                     "resources/fonts/NotoSansMono-Bold.ttf")))
   ;; TODO: lem:config
   :cjk-normal-file (or cjk-normal-file
                        (lem:config :cjk-normal-font
                                    (asdf:system-relative-pathname
                                     :lem-sdl2
                                     "resources/fonts/NotoSansCJK-Regular.ttc")))
   :cjk-bold-file (or cjk-bold-file
                      (lem:config :cjk-bold-font
                                  (asdf:system-relative-pathname
                                   :lem-sdl2
                                   "resources/fonts/NotoSansCJK-Bold.ttc")))
   :emoji-file (or emoji-file
                   (lem:config :emoji-font
                               (asdf:system-relative-pathname
                                :lem-sdl2
                                "resources/fonts/NotoColorEmoji.ttf")))))

(defun merge-font-config (new old)
  (%make-font-config :size (or (font-config-size new)
                               (font-config-size old))
                     :latin-normal-file (or (font-config-latin-normal-file new)
                                            (font-config-latin-normal-file old))
                     :latin-bold-file (or (font-config-latin-bold-file new)
                                          (font-config-latin-bold-file old))
                     :cjk-normal-file (or (font-config-cjk-normal-file new)
                                          (font-config-cjk-normal-file old))
                     :cjk-bold-file (or (font-config-cjk-bold-file new)
                                        (font-config-cjk-bold-file old))
                     :emoji-file (or (font-config-emoji-file new)
                                     (font-config-emoji-file old))))

(defun change-size (font-config size)
  (setf (font-config-size font-config) size)
  font-config)

(defun get-character-size (font)
  (let* ((surface (sdl2-ttf:render-text-solid font "A" 0 0 0 0))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (list width height)))

(defun open-font (&optional font-config)
  (let* ((font-config (or font-config (make-font-config)))
         (latin-normal-font (sdl2-ttf:open-font (font-config-latin-normal-file font-config)
                                                (font-config-size font-config)))
         (latin-bold-font (sdl2-ttf:open-font (font-config-latin-bold-file font-config)
                                              (font-config-size font-config)))
         (cjk-normal-font (sdl2-ttf:open-font (font-config-cjk-normal-file font-config)
                                              (font-config-size font-config)))
         (cjk-bold-font (sdl2-ttf:open-font (font-config-cjk-bold-file font-config)
                                            (font-config-size font-config)))
         (emoji-font (sdl2-ttf:open-font (font-config-emoji-file font-config)
                                         (font-config-size font-config))))
    (destructuring-bind (char-width char-height)
        (get-character-size latin-normal-font)
      (make-font :latin-normal-font latin-normal-font
                 :latin-bold-font latin-bold-font
                 :cjk-normal-font cjk-normal-font
                 :cjk-bold-font cjk-bold-font
                 :emoji-font emoji-font
                 :char-width char-width
                 :char-height char-height))))

(defun close-font (font)
  (sdl2-ttf:close-font (font-latin-normal-font font))
  (sdl2-ttf:close-font (font-latin-bold-font font))
  (sdl2-ttf:close-font (font-cjk-normal-font font))
  (sdl2-ttf:close-font (font-cjk-bold-font font))
  (sdl2-ttf:close-font (font-emoji-font font))
  (values))

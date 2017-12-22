(in-package :xcb)

(cffi:define-foreign-library libxcb
    (:unix "libxcb.so"))
(cffi:use-foreign-library libxcb)

(cffi:define-foreign-library libxcb-render
    (:unix "libxcb-render.so"))
(cffi:use-foreign-library libxcb-render)

(cffi:define-foreign-library libxcb-render-util
    (:unix "libxcb-render-util.so"))
(cffi:use-foreign-library libxcb-render-util)

(cffi:define-foreign-library libxcb-icccm
  (:unix "libxcb-icccm.so"))
(cffi:use-foreign-library libxcb-icccm)

(uiop:define-package #:lem-vue-mode/lsp-config
  (:use #:cl #:lem-lsp-mode)
  (:import-from :lem-lsp-base/type
                :make-lsp-array
                :make-lsp-map)
  (:import-from :lem-vue-mode
                :vue-mode
                :*vue-language-server-location*)
  (:export))

(in-package :lem-vue-mode/lsp-config)

(define-language-spec (vue-spec vue-mode
                                :parent-spec lem-js-mode/lsp-config::js-spec)
  :language-id "vue"
  :install-command "npm install -g typescript-language-server typescript @vue/language-server @vue/typescript-plugin"
  :readme-url "https://github.com/vuejs/language-tools")

(defmethod spec-initialization-options ((spec vue-spec))
  (make-lsp-map
   "plugins" (make-lsp-array
              (make-lsp-map
               "name" "@vue/typescript-plugin"
               "location" (etypecase *vue-language-server-location*
                            (pathname (uiop:native-namestring *vue-language-server-location*))
                            (string *vue-language-server-location*))
               "languages" (make-lsp-array "vue")))))
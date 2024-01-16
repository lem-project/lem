(in-package :lem-translator)

(defclass deepl (service) ())

(defun auth-key ()
  (or (lem:config :deepl-auth-key)
      (setf (lem:config :deepl-auth-key)
            (lem:prompt-for-string "Auth key: "))))

(defun ja-char-p (char)
  (cond ((char<= (code-char 12354) ;#\HIRAGANA_LETTER_A
                 char
                 (code-char 12435) ;#\HIRAGANA_LETTER_N
                 ))
        ((char<= (code-char 12450) ;#\KATAKANA_LETTER_A
                 char
                 (code-char 12531) ;#\KATAKANA_LETTER_N
                 ))
        ((or (<= #x4E00
                 (char-code char)
                 #x9FFF)
             (find char "仝々〆〇ヶ")))))

(defun ja-text-p (text)
  (loop :for c :across text
        :when (ja-char-p c)
        :return t))

(defmethod translate-string ((service deepl) &key from to string)
  (let* ((source-lang (or from "EN"))
         (target-lang (or to "JA"))
        (response
          (dex:post "https://api-free.deepl.com/v2/translate"
                    ;;TODO: Use the object key instead of calling the auth-key function
                    :headers `(("Authorization" . ,(format nil "DeepL-Auth-Key ~A" (auth-key))))
                    :content `(("text" . ,string)
                               ("target_lang" . ,target-lang)
                               ("source_lang" . ,source-lang)))))
    (with-output-to-string (out)
      (loop :for tr :in (gethash "translations" (yason:parse response))
            :do (fresh-line out)
                (write-string (gethash "text" tr) out)))))

(defmethod translate-region ((service deepl) &key from to
                                                  region-start region-end
                                                  replace)
  (declare (ignore from to))
  (let* ((source-text (points-to-string region-start region-end))
         (is-japanese (ja-text-p source-text))
         (translated-text (translate-string
                           service
                           :string (ppcre:regex-replace-all "\\s+" source-text " ")
                           :from (if is-japanese "EN" "JA")
                           :to (if is-japanese "JA" "EN")))
         (buffer (translate-output
                  (make-buffer "*deepl*")
                  source-text
                  translated-text)))
    (cond (replace
           (delete-between-points region-start region-end)
           (insert-string region-start translated-text))
          (t
           (pop-to-buffer buffer)))))

(in-package :lem-base)

(export '(kill-buffer-hook
          buffer-list
          any-modified-buffer-p
          get-buffer
          unique-buffer-name
          delete-buffer
          get-next-buffer
          get-previous-buffer
          unbury-buffer
          bury-buffer
          get-file-buffer))

(define-editor-variable kill-buffer-hook '())

(defvar *buffer-list* '())

(defun buffer-list ()
  "`buffer`のリストを返します。"
  *buffer-list*)

(defun set-buffer-list (buffer-list)
  (setf *buffer-list* buffer-list))

(defun add-buffer (buffer)
  (check-type buffer buffer)
  (assert (not (get-buffer (buffer-name buffer))))
  (set-buffer-list (cons buffer (buffer-list))))

(defun any-modified-buffer-p ()
  (some (lambda (buffer)
          (and (buffer-filename buffer)
               (buffer-modified-p buffer)))
        (buffer-list)))

(defun get-buffer (buffer-or-name)
  "`buffer-or-name`がバッファならそのまま返し、
文字列ならその名前のバッファを返します。"
  (check-type buffer-or-name (or buffer string))
  (if (bufferp buffer-or-name)
      buffer-or-name
      (find-if (lambda (buffer)
                 (string= buffer-or-name
                          (buffer-name buffer)))
               (buffer-list))))

(defun unique-buffer-name (name)
  (check-type name string)
  (if (null (get-buffer name))
      name
      (loop :for n :from 1
            :for sub-name := (format nil "~A<~D>" name n)
            :do (unless (get-buffer sub-name)
                  (return sub-name)))))

(defun delete-buffer (buffer)
  "`buffer`をバッファのリストから消します。
エディタ変数`kill-buffer-hook`がバッファが消される前に実行されます。"
  (check-type buffer buffer)
  (alexandria:when-let ((hooks (variable-value 'kill-buffer-hook :buffer buffer)))
    (run-hooks hooks buffer))
  (alexandria:when-let ((hooks (variable-value 'kill-buffer-hook :global)))
    (run-hooks hooks buffer))
  (buffer-free buffer)
  (set-buffer-list (delete buffer (buffer-list))))

(defun get-next-buffer (buffer)
  "バッファリスト内にある`buffer`の次のバッファを返します。"
  (check-type buffer buffer)
  (let ((rest (member buffer (buffer-list))))
    (cadr rest)))

(defun get-previous-buffer (buffer)
  "バッファリスト内にある`buffer`の前のバッファを返します。"
  (check-type buffer buffer)
  (loop :for prev := nil :then curr
        :for curr :in (buffer-list)
        :do (when (eq buffer curr)
              (return prev))))

(defun unbury-buffer (buffer)
  (check-type buffer buffer)
  (unless (buffer-temporary-p buffer)
    (set-buffer-list
     (cons buffer
           (delete buffer (buffer-list)))))
  buffer)

(defun bury-buffer (buffer)
  "`buffer`をバッファリストの一番最後に移動させ、バッファリストの先頭を返します。"
  (check-type buffer buffer)
  (unless (buffer-temporary-p buffer)
    (set-buffer-list
     (nconc (delete buffer (buffer-list))
            (list buffer))))
  (car (buffer-list)))

(defun get-file-buffer (filename)
  "`filename`に対応するバッファを返します。
見つからなければNILを返します。"
  (check-type filename (or string pathname))
  (dolist (buffer (buffer-list))
    (when (uiop:pathname-equal filename (buffer-filename buffer))
      (return buffer))))

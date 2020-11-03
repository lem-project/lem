(defpackage :lem-lsp-mode/lsp-mode
  (:use :cl :lem)
  (:import-from :lem-lsp-mode/utils)
  (:import-from :lem-lsp-mode/protocol)
  (:import-from :lem-lsp-mode/request)
  (:import-from :lem-lsp-mode/client))
(in-package :lem-lsp-mode/lsp-mode)

(cl-package-locks:lock-package :lem-lsp-mode/lsp-mode)
(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-mode/protocol)
(lem-lsp-mode/project:local-nickname :utils :lem-lsp-mode/utils)
(lem-lsp-mode/project:local-nickname :request :lem-lsp-mode/request)

(defvar *workspaces* '())

(defstruct workspace
  root-uri
  client
  language-id
  server-capabilities
  server-info)

(defun find-workspace (root-uri language-id)
  (dolist (workspace *workspaces*)
    (when (and (equal (workspace-root-uri workspace)
                      root-uri)
               (equal (workspace-root-uri workspace)
                      language-id))
      (return workspace))))

(defun buffer-workspace (buffer)
  (buffer-value buffer 'workspace))

(defun (setf buffer-workspace) (workspace buffer)
  (setf (buffer-value buffer 'workspace) workspace))

(lem:define-minor-mode lsp-mode
    (:name "Language Client"
     :enable-hook 'enable-hook))

(defun enable-hook ()
  (ensure-lsp-connection (current-buffer)))

(defun find-root-pathname (directory uri-patterns)
  (utils:find-root-pathname directory
                            (lambda (file)
                              (let ((file-name (file-namestring file)))
                                (dolist (uri-pattern uri-patterns)
                                  (when (search uri-pattern file-name)
                                    (return t)))))))

(defgeneric make-client (mode spec))

(defmethod make-client ((mode (eql :tcp)) spec)
  (make-instance 'lem-lsp-mode/client:tcp-client :port (spec-port spec)))

(defun initialize (workspace)
  (let ((initialize-result
          (request:lsp-call-method (workspace-client workspace)
                                   (make-instance 'request:initialize-request
                                                  :root-uri (workspace-root-uri workspace)))))
    (setf (workspace-server-capabilities workspace)
          (protocol:initialize-result-capabilities initialize-result))
    (setf (workspace-server-info workspace)
          (protocol:initialize-result-server-info initialize-result)))
  (values))

(defun initialized (workspace)
  (request:lsp-call-method (workspace-client workspace)
                           (make-instance 'request:initialized-request)))

(defun ensure-lsp-connection (buffer)
  (let* ((spec (get-language-spec (buffer-major-mode buffer)))
         (root-pathname (find-root-pathname (buffer-directory buffer)
                                            (spec-root-uri-patterns spec)))
         (root-uri (utils:pathname-to-uri root-pathname))
         (language-id (spec-langauge-id spec)))
    (let ((workspace (find-workspace root-uri language-id)))
      (cond ((null workspace)
             (let* ((client (make-client (spec-mode spec) spec))
                    (workspace (make-workspace :client client
                                               :root-uri root-uri
                                               :language-id language-id)))
               (push workspace *workspaces*)
               (setf (buffer-workspace buffer) workspace)
               (initialize workspace)
               (initialized workspace)))
            (t
             (setf (buffer-workspace buffer) workspace))))))

(defvar *language-spec-table* (make-hash-table))

(defun get-language-spec (major-mode)
  (get *language-spec-table* major-mode))

(defun spec-langauge-id (spec)
  (getf spec :language-id))

(defun spec-root-uri-patterns (spec)
  (getf spec :root-uri-patterns))

(defun spec-mode (spec)
  (getf spec :mode))

(defun spec-port (spec)
  (getf spec :port))

(defmacro def-language-spec (major-mode &rest plist)
  `(setf (gethash ',major-mode *language-spec-table*)
         (list ,@plist)))

(def-language-spec lem-go-mode:go-mode
  :language-id "go"
  :root-uri-patterns '("go.mod")
  :mode :tcp
  :port 12345)

#|
ファイルを開く
そのファイルがどの言語のファイルかを判別し、それをlanguage-idとする
そのファイルのルートディレクトリをlanguage-idを元に探す
ルートディレクトリに対応するworkspaceが既に作られていないか確認する
あればそのファイルとworkspaceを紐付ける
なければ新しくworkspaceを作る

workspaceは以下のような要素を含んだ構造
- 一つ以上のルートディレクトリ
- language-id
- サーバとのコネクション
- etc

workspaceを作るとき、新しくサーバプログラムを起動してstdioかtcpで接続する(tcpの場合は既に立ち上がってるサーバに接続したい場合もある?)
そのときクライアントはサーバに対してinitializeリクエストを行う
返ってきたレスポンスinitialized-resultにserverの情報が入っているので、それをワーススペースに保存する
|#

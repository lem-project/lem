# buffer
`buffer`はバッファ名、ファイル名、テキスト、テキストを指す位置等が入った、文書を管理するオブジェクトです。  
複数の`buffer`はリストで管理されています。  

## buffer用オペレーション
* `lem:current-buffer: ()`  
  現在の`buffer`を返します。

* `(setf lem:current-buffer): (buffer)`  
  現在の`buffer`を変更します。

* `lem:make-buffer: (name &key filename read-only-p (enable-undo-p t) (syntax-table
                                                                    (fundamental-syntax-table)))`  
  新しい`buffer`を作って返します。  
既に`name`と同じ名前のバッファがある場合はエラーになります。

* `lem:bufferp: (x)`  
  `x`が`buffer`ならT、それ以外ならNILを返します。

* `lem:buffer-modified-p: (&optional (buffer (current-buffer)))`  
  `buffer`が変更されていたらT、それ以外ならNILを返します。

* `lem:buffer-enable-undo-p: (&optional (buffer (current-buffer)))`  
  `buffer`でアンドゥが有効ならT、それ以外ならNILを返します。

* `lem:buffer-enable-undo: (buffer)`  
  `buffer`のアンドゥを有効にします。

* `lem:buffer-disable-undo: (buffer)`  
  `buffer`のアンドゥを無効にして編集履歴を空にします。

* `lem:buffer-name: (&optional (buffer (current-buffer)))`  
  `buffer`の名前を返します。

* `lem:buffer-filename: (&optional (buffer (current-buffer)))`  
  `buffer`のファイル名を返します。

* `lem:buffer-directory: (&optional (buffer (current-buffer)))`  
  `buffer`のディレクトリを返します。

* `lem:buffer-unmark: (buffer)`  
  `buffer`の変更フラグを下ろします。

* `lem:buffer-rename: (buffer name)`  
  `buffer`の名前を`name`に変更します。


##
* `lem:buffer-list: ()`  
  `buffer`のリストを返します。


## エディタ変数

# buffer
* `lem:buffer`  
  `buffer`はバッファ名、ファイル名、テキスト、テキストを指す位置等が入った、
文書を管理するオブジェクトです。  
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


* `lem:buffer-list: ()`  
  `buffer`のリストを返します。

* `lem:get-buffer: (buffer-or-name)`  
  `buffer-or-name`がバッファならそのまま返し、
文字列ならその名前のバッファを返します。

* `lem:get-buffer-create: (name)`  
  バッファ名`name`のバッファがあればそれを返し、
無ければ作って返します。

* `lem:delete-buffer: (buffer)`  
  `buffer`をバッファのリストから消します。
エディタ変数`kill-buffer-hook`がバッファが消される前に実行されます。

* `lem:get-next-buffer: (buffer)`  
  バッファリスト内にある`buffer`の次のバッファを返します。

* `lem:bury-buffer: (buffer)`  
  `buffer`をバッファリストの一番最後に移動させます。


# point
* `lem:point`  
  `point`はバッファ内のテキストの位置を指すオブジェクトです。  
`buffer`とその位置の行、行頭からの0始まりのオフセット`charpos`をもっています。  
`point`には`kind`があり、バッファ内に挿入、削除したときに位置を調整する動作を制御します。  
`kind`が`:temporary`の時は`point`を一時的な読み取りに使います。  
作成、削除時のオーバーヘッドが低く、明示的に削除する必要もありませんが、
その位置より前を編集した後は正しく使用できません。  
`kind`が`:left-inserting`または`:right-inserting`の時はそれより前の位置を編集したときに、
編集した長さだけ位置を移動します。  
`point`と同じ位置に挿入したときは`:right-inserting`は元の位置のままで、`:left-inserting`の時は移動します。  
`:left-inserting`または`:right-inserting`の時はバッファがその`point`を管理しているので、
使用後は`delete-point`で明示的に削除するか`with-point`を使ってください。


* `lem:point-buffer: (object)`  
  `point`が指す`buffer`を返します。

* `lem:point-kind: (object)`  
  `point`の種類(`:temporary`、`:left-inserting`または`:right-inserting`)を返します。

* `lem:current-point: ()`  
  現在の`point`を返します。

* `lem:pointp: (x)`  
  `x`が`point`ならT、それ以外ならNILを返します。

* `lem:copy-point: (point &optional kind)`  
  `point`のコピーを作って返します。
`kind`は`:temporary`、`:left-inserting`または `right-inserting`です。

* `lem:delete-point: (point)`  
  `point`を削除します。
`point-kind`が:temporaryの場合はこの関数を使う必要はありません。

* `lem:point=: (point1 point2)`  
  `point1`と`point2`が同じ位置にあるならT、それ以外はNILを返します。

* `lem:point/=: (point1 point2)`  
  `point1`と`point2`が同じ位置ではないならT、それ以外はNILを返します。

* `lem:point<: (point1 point2)`  
  `point1`が`point2`よりも前にあるならT、それ以外はNILを返します。

* `lem:point<=: (point1 point2)`  
  `point1`が`point2`と同じ位置、または前にあるならT、それ以外はNILを返します。

* `lem:point>: (point1 point2)`  
  `point1`が`point2`よりも後にあるならT、それ以外はNILを返します。

* `lem:point>=: (point1 point2)`  
  `point1`が`point2`と同じ位置、または後にあるならT、それ以外はNILを返します。

* `lem:with-point: (bindings &body body)`  
  このマクロは`body`内で使う各`point`を`bindings`で作り、
`body`を抜けると各`point`を削除して`body`の値を返します。  
`body`でエラーがあっても`point`は削除されます。  
`bindings`の形式は(`var` `point` [`kind`])のリストです。  
`kind`は省略可能でデフォルトで`:temporary`です。  



# エディタ変数
* `lem:buffer-value: (buffer name &optional default)`  
  `buffer`のバッファ変数`name`に束縛されている値を返します。
変数が設定されていない場合は`default`を返します。

* `(setf lem:buffer-value): (value buffer name &optional default)`  
  `buffer`のバッファ変数`name`に`value`を束縛します。

* `lem:buffer-unbound: (buffer name)`  
  `buffer`のバッファ変数`name`の束縛を消します。

* `lem:clear-buffer-variables: (&key (buffer (current-buffer)))`  
  `buffer`に束縛されているすべてのバッファ変数を消します。


# バッファ
*TYPE* `lem:buffer`  
`buffer`はバッファ名、ファイル名、テキスト、テキストを指す位置等が入った、
文書を管理するオブジェクトです。  
複数の`buffer`はリストで管理されています。

*FUNCTION* `lem:current-buffer ()`  
現在の`buffer`を返します。

*SETF* `(setf lem:current-buffer) (buffer)`  
現在の`buffer`を変更します。

*FUNCTION* `lem:make-buffer (name &key filename read-only-p (enable-undo-p t) (syntax-table
                                                                               (fundamental-syntax-table)))`  
新しい`buffer`を作って返します。  
既に`name`と同じ名前のバッファがある場合はエラーになります。

*FUNCTION* `lem:bufferp (x)`  
`x`が`buffer`ならT、それ以外ならNILを返します。

*FUNCTION* `lem:buffer-modified-p (&optional (buffer (current-buffer)))`  
`buffer`が変更されていたらT、それ以外ならNILを返します。

*FUNCTION* `lem:buffer-enable-undo-p (&optional (buffer (current-buffer)))`  
`buffer`でアンドゥが有効ならT、それ以外ならNILを返します。

*FUNCTION* `lem:buffer-enable-undo (buffer)`  
`buffer`のアンドゥを有効にします。

*FUNCTION* `lem:buffer-disable-undo (buffer)`  
`buffer`のアンドゥを無効にしてアンドゥ用の情報を空にします。

*FUNCTION* `lem:buffer-name (&optional (buffer (current-buffer)))`  
`buffer`の名前を返します。

*FUNCTION* `lem:buffer-filename (&optional (buffer (current-buffer)))`  
`buffer`のファイル名を返します。

*FUNCTION* `lem:buffer-directory (&optional (buffer (current-buffer)))`  
`buffer`のディレクトリを返します。

*FUNCTION* `lem:buffer-unmark (buffer)`  
`buffer`の変更フラグを下ろします。

*FUNCTION* `lem:buffer-rename (buffer name)`  
`buffer`の名前を`name`に変更します。

*FUNCTION* `lem:buffer-list ()`  
`buffer`のリストを返します。

*FUNCTION* `lem:get-buffer (buffer-or-name)`  
`buffer-or-name`がバッファならそのまま返し、
文字列ならその名前のバッファを返します。

*FUNCTION* `lem:get-buffer-create (name)`  
バッファ名`name`のバッファがあればそれを返し、
無ければ作って返します。

*FUNCTION* `lem:delete-buffer (buffer)`  
`buffer`をバッファのリストから消します。
エディタ変数`kill-buffer-hook`がバッファが消される前に実行されます。

*FUNCTION* `lem:get-next-buffer (buffer)`  
バッファリスト内にある`buffer`の次のバッファを返します。

*FUNCTION* `lem:bury-buffer (buffer)`  
`buffer`をバッファリストの一番最後に移動させ、バッファリストの先頭を返します。

*FUNCTION* `lem:get-file-buffer (filename)`  
`filename`に対応するバッファを返します。
見つからなければNILを返します。


# ポイント
*TYPE* `lem:point`  
`point`はバッファ内のテキストの位置を指すオブジェクトです。  
`buffer`とその位置の行、行頭からの0始まりのオフセット`charpos`をもっています。  
`point`には`kind`があり、バッファ内に挿入、削除した後の位置が`kind`の値によって変わります。  
`kind`が`:temporary`の時は`point`を一時的な読み取りに使います。  
作成、削除時のオーバーヘッドが低く、明示的に削除する必要もありませんが、
その位置より前を編集した後はその`point`は正しく使用できません。  
`kind`が`:left-inserting`または`:right-inserting`の時はそれより前の位置を編集したときに、
編集した長さだけ位置を調整します。  
`point`と同じ位置に挿入すると
`:right-inserting`では元の位置のままで、`:left-inserting`では移動します。  
`:left-inserting`または`:right-inserting`の場合は、使用後に`delete-point`で明示的に削除するか、
`with-point`を使う必要があります。


*FUNCTION* `lem:point-buffer (object)`  
`point`が指す`buffer`を返します。

*FUNCTION* `lem:point-kind (object)`  
`point`の種類(`:temporary`、`:left-inserting`または`:right-inserting`)を返します。

*FUNCTION* `lem:current-point ()`  
現在の`point`を返します。

*FUNCTION* `lem:pointp (x)`  
`x`が`point`ならT、それ以外ならNILを返します。

*FUNCTION* `lem:copy-point (point &optional kind)`  
`point`のコピーを作って返します。
`kind`は`:temporary`、`:left-inserting`または `right-inserting`です。
省略された場合は`point`と同じ値です。

*FUNCTION* `lem:delete-point (point)`  
`point`を削除します。
`point-kind`が:temporaryの場合はこの関数を使う必要はありません。

*FUNCTION* `lem:point= (point1 point2)`  
`point1`と`point2`が同じ位置にあるならT、それ以外はNILを返します。

*FUNCTION* `lem:point/= (point1 point2)`  
`point1`と`point2`が同じ位置ではないならT、それ以外はNILを返します。

*FUNCTION* `lem:point< (point1 point2)`  
`point1`が`point2`よりも前にあるならT、それ以外はNILを返します。

*FUNCTION* `lem:point<= (point1 point2)`  
`point1`が`point2`と同じ位置、または前にあるならT、それ以外はNILを返します。

*FUNCTION* `lem:point> (point1 point2)`  
`point1`が`point2`よりも後にあるならT、それ以外はNILを返します。

*FUNCTION* `lem:point>= (point1 point2)`  
`point1`が`point2`と同じ位置、または後にあるならT、それ以外はNILを返します。


*FUNCTION* `lem:set-current-mark (point)`  
`point`を現在のマークに設定します。


*FUNCTION* `lem:character-at (point &optional (offset 0))`  
`point`から`offset`ずらした位置の文字を返します。
バッファの範囲外ならNILを返します。

*FUNCTION* `lem:line-string (point)`  
`point`の行の文字列を返します。


*FUNCTION* `lem:line-number-at-point (point)`  
`point`の行番号を返します。

*FUNCTION* `lem:point-column (point)`  
`point`の行頭からの列幅を返します。

*FUNCTION* `lem:position-at-point (point)`  
`point`のバッファの先頭からの1始まりのオフセットを返します。


*FUNCTION* `lem:with-point (bindings &body body)`  
このマクロは`body`内で使う各`point`を`bindings`で作り、
`body`を抜けると各`point`を削除して`body`の値を返します。  
`body`でエラーがあっても各`point`は削除されます。  
`bindings`の形式は(`var` `point` &optional `kind`)のリストです。  
`kind`は省略可能でデフォルトで`:temporary`です。  
```
例
(with-point ((p3 expr1)
             (p1 expr2 :left-inserting)
             (p2 expr3 :right-inserting))
  ...)
```


*FUNCTION* `lem:save-excursion (&body body)`  
現在の`point`と`mark`を保存し、`body`の評価後に復元し`body`の結果を返します。  
`body`でエラーがあっても復元されます。


## ポイントの取得
*FUNCTION* `lem:buffer-point (object)`  
`buffer`の現在の`point`を返します。

*FUNCTION* `lem:buffer-mark (object)`  
`buffer`の現在のマークの`point`を返します。

*FUNCTION* `lem:buffer-start-point (object)`  
`buffer`の最初の位置の`point`を返します。

*FUNCTION* `lem:buffer-end-point (object)`  
`buffer`の最後の位置の`point`を返します。


## ポイント位置の検査
*FUNCTION* `lem:first-line-p (point)`  
`point`が最初の行ならT、それ以外ならNILを返します。

*FUNCTION* `lem:last-line-p (point)`  
`point`が最後の行ならT、それ以外ならNILを返します。

*FUNCTION* `lem:start-line-p (point)`  
`point`が行頭ならT、それ以外ならNILを返します。

*FUNCTION* `lem:end-line-p (point)`  
`point`が行末ならT、それ以外ならNILを返します。

*FUNCTION* `lem:start-buffer-p (point)`  
`point`がバッファの最初の位置ならT、それ以外ならNILを返します。

*FUNCTION* `lem:end-buffer-p (point)`  
`point`がバッファの最後の位置ならT、それ以外ならNILを返します。

*FUNCTION* `lem:same-line-p (point1 point2)`  
`point1`と`point2`が同じ位置ならT、それ以外ならNILを返します。


## ポイントの移動
*FUNCTION* `lem:move-point (point new-point)`  
`point`を`new-point`の位置に移動します。

*FUNCTION* `lem:line-start (point)`  
`point`を行頭に移動します。

*FUNCTION* `lem:line-end (point)`  
`point`を行末に移動します。

*FUNCTION* `lem:buffer-start (point)`  
`point`をバッファの最初の位置に移動します。

*FUNCTION* `lem:buffer-end (point)`  
`point`をバッファの最後の位置に移動します。

*FUNCTION* `lem:line-offset (point n &optional (charpos 0))`  
`point`を`n`が正の数なら下に、負の数なら上に行を移動し、移動後の`point`を返します。
`n`行先に行が無ければ`point`の位置はそのままでNILを返します。
`charpos`は移動後の行頭からのオフセットです。


*FUNCTION* `lem:character-offset (point n)`  
`point`を`n`が正の数なら後に、負の数なら前に移動し、移動後の`point`を返します。
`n`文字先がバッファの範囲外なら`point`の位置はそのままでNILを返します。

*FUNCTION* `lem:move-to-column (point column &optional force)`  
`point`を行頭から列幅`column`まで移動し、移動後の`point`を返します。
`force`が非NILの場合は、行の長さが`column`より少なければ空白を挿入して移動し、
`force`がNILの場合は、行末まで移動し、移動後の`point`を返します。

*FUNCTION* `lem:move-to-position (point position)`  
`point`をバッファの先頭からの1始まりのオフセット`position`に移動してその位置を返します。
`position`がバッファの範囲外なら`point`は移動せず、NILを返します。

*FUNCTION* `lem:move-to-line (point line-number)`  
`point`を行番号`line-number`に移動し、移動後の位置を返します。
`line-number`がバッファの範囲外なら`point`は移動せず、NILを返します。

*FUNCTION* `lem:skip-chars-forward (point test)`  
`point`からその位置の文字を`test`で評価して非NILの間、後の方向に移動します。  
`test`が文字のリストならその位置の文字が`test`のリスト内に含まれるか  
`test`が関数ならその位置の文字を引数として一つ取り、返り値が非NILであるか


*FUNCTION* `lem:skip-chars-backward (point test)`  
`point`からその位置の前の文字を`test`で評価して非NILの間、前の方向に移動します。  
`test`が文字のリストならその位置の前の文字が`test`のリスト内に含まれるか  
`test`が関数ならその位置の前の文字を引数として一つ取り、返り値が非NILであるか



## リージョン
*FUNCTION* `lem:region-beginning (&optional (buffer (current-buffer)))`  
`buffer`内のリージョンの始まりの位置の`point`を返します。

*FUNCTION* `lem:region-end (&optional (buffer (current-buffer)))`  
`buffer`内のリージョンの終わりの位置の`point`を返します。

*FUNCTION* `lem:points-to-string (start-point end-point)`  
`start-point`から`end-point`までの範囲の文字列を返します。

*FUNCTION* `lem:count-characters (start-point end-point)`  
`start-point`から`end-point`までの文字列の長さを返します。

*FUNCTION* `lem:count-lines (start-point end-point)`  
`start-point`から`end-point`までの行数を返します。


## テキストの編集
*FUNCTION* `lem:insert-character (point char &optional (n 1))`  
`point`に文字`char`を`n`回挿入します。

*FUNCTION* `lem:insert-string (point string &rest plist)`  
`point`に文字列`string`を挿入します。  
`plist`を指定すると`string`を挿入した範囲にテキストプロパティを設定します。

*FUNCTION* `lem:delete-character (point &optional (n 1))`  
`point`から`n`個文字を削除し、削除した文字列を返します。 
`n`個の文字を削除する前にバッファの末尾に達した場合はNILを返します。

*FUNCTION* `lem:erase-buffer (&optional (buffer (current-buffer)))`  
`buffer`のテキストをすべて削除します。

*FUNCTION* `lem:delete-between-points (start-point end-point)`  
`start-point`から`end-point`までの範囲を削除し、削除した文字列を返します。

*FUNCTION* `lem:filter-region-lines (start-point end-point function)`  
`start-point`から`end-point`までの範囲の行に`function`を適用します。
`function`は行の文字列を引数に取り新しい行の文字列を返す関数です。


## テキストプロパティ
*FUNCTION* `lem:text-property-at (point prop &optional (offset 0))`  
`point`から`offset`ずらした位置の`prop`のプロパティを返します。

*FUNCTION* `lem:put-text-property (start-point end-point prop value)`  
`start-point`から`end-point`の間のテキストプロパティ`prop`を`value`にします。

*FUNCTION* `lem:remove-text-property (start-point end-point prop)`  
`start-point`から`end-point`までのテキストプロパティ`prop`を削除します。

*FUNCTION* `lem:next-single-property-change (point prop &optional limit-point)`  
`point`からテキストプロパティ`prop`の値が異なる位置まで後の方向に移動し、
移動後の`point`を返します。  
バッファの最後まで走査が止まらないか、`limit-point`を越えると走査を中断しNILを返します。

*FUNCTION* `lem:previous-single-property-change (point prop &optional limit-point)`  
`point`からテキストプロパティ`prop`の値が異なる位置まで前の方向に移動し、
移動後の`point`を返します。  
バッファの最初の位置まで走査が止まらないか、`limit-point`を越えると走査を中断しNILを返します。


# エディタ変数
*FUNCTION* `lem:buffer-value (buffer name &optional default)`  
`buffer`のバッファ変数`name`に束縛されている値を返します。  
`buffer`の型は`buffer`または`point`です。  
変数が設定されていない場合は`default`を返します。

*SETF* `(setf lem:buffer-value) (value buffer name &optional default)`  
`buffer`のバッファ変数`name`に`value`を束縛します。  
`buffer`の型は`buffer`または`point`です。

*FUNCTION* `lem:buffer-unbound (buffer name)`  
`buffer`のバッファ変数`name`の束縛を消します。

*FUNCTION* `lem:clear-buffer-variables (&key (buffer (current-buffer)))`  
`buffer`に束縛されているすべてのバッファ変数を消します。


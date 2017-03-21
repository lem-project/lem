# buffer
`buffer`はバッファ名、ファイル名、テキスト、テキストを指す位置等が入った、文書を管理するオブジェクトです。  
複数の`buffer`はリストで管理されています。  

## buffer用オペレーション
{lem:current-buffer function}
{lem:current-buffer setf}
{lem:make-buffer function}
{lem:bufferp function}
{lem:buffer-modified-p function}
{lem:buffer-enable-undo-p function}
{lem:buffer-enable-undo function}
{lem:buffer-disable-undo function}
{lem:buffer-name function}
{lem:buffer-filename function}
{lem:buffer-directory function}
{lem:buffer-unmark function}
{lem:buffer-rename function}

{lem:buffer-list function}
{lem:get-buffer function}
{lem:get-buffer-create function}
{lem:delete-buffer function}
{lem:get-next-buffer function}
{lem:bury-buffer function}

# point
{lem:point type}


# エディタ変数
{lem:buffer-value function}
{lem:buffer-value setf}
{lem:buffer-unbound function}
{lem:clear-buffer-variables function}

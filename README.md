# lem
common lispで書いた小さいEmacsライクなエディタ

# 特徴
* emacsに似たバッファ, ウィンドウ, ミニバッファ,
  ミニバッファの補完, ミニバッファの履歴, インクリメンタルサーチ,
  キーボードマクロ, 数引数, undo, redo, キルリング, リージョン, grep, abbrev,
  メジャーモード, マイナーモード, フック等
* UTF-8のサポート, 東アジアの文字幅に対応
* lisp-mode: S式単位の移動と編集, S式のインデント, シンボルの補完,
  エディタ内でのmacroexpand, シンボルのdescribe, S式の評価,
  バックトレース, デバッガ等に対応
* エディタのエラー時に落ちずにバックトレースを表示して実行を継続

# 必要なもの
sbclに対応  
cclやeclはある程度動きますが  
一部の機能(lisp-modeのバックトレースやevalの割り込み)に対応していません  
依存するライブラリはcl-charms, bordeaux-threads, trivial-gray-streams

# インストール
quicklispの下のlocal-projects/に入れて  
`(ql:quickload :lem)`

# ビルド
sbclは用意済み  
`sbcl --load build.lisp`

# 使い方
`(lem:lem)`  
または  
`(lem:lem "ファイル名")`

# License
[MIT](https://github.com/cxxxr/lem/blob/master/LICENCE)

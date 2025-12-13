# Living Canvas - 設計書

Lemエディタの新機能「Living Canvas」の設計ドキュメント。

## 概要

Living Canvasは、コードを**関数ノードとエッジのグラフ**として可視化し、
**Figmaライクなキャンバス**上で操作できる革新的な機能です。

### ビジョン

```
┌─────────────────────────────────────────────────────────────┐
│  Figma-like Canvas                                          │
│                                                             │
│   ┌──────────┐         ┌──────────┐        ┌──────────┐    │
│   │ main     │────────▶│ parse    │───────▶│ validate │    │
│   │          │         │          │        │   ⚡実行中 │    │
│   │ [📝AI]   │         │ [📝AI]   │        │          │    │
│   └──────────┘         └──────────┘        └──────────┘    │
│        │                                         │          │
│        │              ┌──────────┐               │          │
│        └─────────────▶│ log      │◀──────────────┘          │
│                       │          │                          │
│                       └──────────┘                          │
│                                                             │
│   [付箋: "この関数をエラーハンドリング付きで書き直して"]      │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### 差別化ポイント

| 既存ツール | Living Canvas |
|-----------|---------------|
| ビジュアルプログラミング → テキストの代替 | テキスト**と**ビジュアルの融合 |
| 静的コールグラフ | **動的**実行状態の可視化 |
| AIはチャットで分離 | AIが**キャンバス上に統合** |
| コード編集は別ウィンドウ | **同一キャンバス**で全て完結 |

---

## フェーズ別計画

### Phase 1: 基盤（グラフ可視化）

**目標**: パッケージの関数をグラフとして表示し、基本操作を可能にする

**機能**:
- 関数をノードとして表示
- 関数呼び出しをエッジとして表示
- ノードのドラッグ&ドロップ
- ノードクリックでソースにジャンプ
- 自動レイアウト（dagre）

### Phase 2: 実行可視化

**目標**: コードの実行状態をリアルタイムで可視化

**機能**:
- trace統合による実行追跡
- 実行中の関数をハイライト
- パフォーマンスヒートマップ
- 呼び出し回数の表示

### Phase 3: AI統合

**目標**: 付箋を通じてAIと連携

**機能**:
- ノードに付箋を追加
- 付箋にプロンプトを記述
- AIによるコード生成
- 生成結果のプレビューと適用

### Phase 4: 時間軸編集

**目標**: 編集履歴を時間軸で管理

**機能**:
- 編集履歴のタイムライン表示
- 任意の時点へのジャンプ
- 2時点間の差分表示
- スナップショット機能

---

## アーキテクチャ

### 全体構成

```
┌─────────────────────────────────────────────────────────┐
│                    Living Canvas                        │
│  ┌─────────────────────────────────────────────────┐   │
│  │     WebView Frontend (HTML/CSS/JS)              │   │
│  │     - Cytoscape.js for graph rendering          │   │
│  │     - ノード描画、ドラッグ&ドロップ                │   │
│  │     - 実行状態のリアルタイム可視化                 │   │
│  └─────────────────────────────────────────────────┘   │
│                         ↕ JSON-RPC                      │
│  ┌─────────────────────────────────────────────────┐   │
│  │         Lem Core (Common Lisp)                  │   │
│  │     - バッファ/ウィンドウ管理                     │   │
│  │     - Lisp評価エンジン                           │   │
│  │     - コールグラフ解析                            │   │
│  │     - AI API統合                                 │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

### 既存機能の活用

| 既存機能 | 用途 |
|---------|------|
| `html-buffer` | Canvas表示のベースクラス |
| `change-view-to-html` | ViewをHTMLモードに切替 |
| `js-eval` | Lisp→JS通信（グラフ更新等） |
| `register-method` | JS→Lisp通信（イベント受信） |

### ファイル構成

```
extensions/
└── living-canvas/
    ├── lem-living-canvas.asd       # システム定義
    ├── package.lisp                # パッケージ定義
    ├── call-graph.lisp             # 関数呼び出しグラフ解析
    ├── canvas-buffer.lisp          # Canvasバッファクラス
    ├── commands.lisp               # ユーザーコマンド
    └── trace.lisp                  # 実行追跡 (Phase 2)
```

---

## データ構造

### Lisp側

```lisp
;; ノードの表現
(defstruct graph-node
  id              ; ユニークID (シンボル名 + パッケージ)
  name            ; 関数名
  package         ; パッケージ名
  type            ; :function, :macro, :generic-function
  docstring       ; ドキュメント文字列
  source-location ; (file . line-number)
  position)       ; (x . y) キャンバス上の位置

;; エッジの表現
(defstruct graph-edge
  source          ; 呼び出し元ノードID
  target          ; 呼び出し先ノードID
  call-type)      ; :direct, :funcall, :apply

;; グラフ全体
(defstruct call-graph
  nodes           ; ノードのハッシュテーブル
  edges           ; エッジのリスト
  root-package)   ; 解析対象パッケージ
```

### JSON-RPC API

#### Lisp → JavaScript

| メソッド | 説明 | パラメータ |
|---------|------|-----------|
| `canvas:init` | キャンバス初期化 | `{width, height, theme}` |
| `canvas:update-graph` | グラフ更新 | `{nodes, edges}` |
| `canvas:highlight-node` | ノードハイライト | `{nodeId, color}` |
| `canvas:set-node-position` | ノード位置設定 | `{nodeId, x, y}` |

#### JavaScript → Lisp

| メソッド | 説明 | パラメータ |
|---------|------|-----------|
| `canvas:node-clicked` | ノードクリック | `{nodeId}` |
| `canvas:node-moved` | ノード移動完了 | `{nodeId, x, y}` |
| `canvas:request-refresh` | グラフ再計算要求 | `{packageName}` |
| `canvas:open-source` | ソース表示要求 | `{nodeId}` |

---

## Phase 1 詳細実装

### call-graph.lisp

```lisp
(defpackage :lem-living-canvas/call-graph
  (:use :cl)
  (:export #:analyze-package
           #:analyze-buffer
           #:get-callees
           #:graph-to-json))
(in-package :lem-living-canvas/call-graph)

;; 関数本体からシンボル参照を抽出
(defun extract-called-functions (form)
  "フォームから呼び出されている関数を抽出する"
  (let ((calls '()))
    (labels ((walk (form)
               (cond
                 ((symbolp form)
                  (when (fboundp form)
                    (pushnew form calls)))
                 ((consp form)
                  (case (car form)
                    ((function)
                     (when (symbolp (cadr form))
                       (pushnew (cadr form) calls)))
                    ((funcall apply)
                     (when (and (consp (cadr form))
                                (eq 'function (caadr form)))
                       (pushnew (cadadr form) calls)))
                    (otherwise
                     (mapc #'walk form)))))))
      (walk form))
    calls))

;; パッケージ内の全関数を解析
(defun analyze-package (package-designator)
  "指定パッケージの関数呼び出しグラフを構築する"
  (let ((package (find-package package-designator))
        (nodes (make-hash-table :test 'eq))
        (edges '()))
    (do-symbols (sym package)
      (when (and (fboundp sym)
                 (eq (symbol-package sym) package))
        ;; ノード作成
        (setf (gethash sym nodes)
              (make-graph-node
               :id (format nil "~A:~A"
                           (package-name package)
                           (symbol-name sym))
               :name (symbol-name sym)
               :package (package-name package)
               :type (cond ((macro-function sym) :macro)
                           ((typep (fdefinition sym) 'generic-function)
                            :generic-function)
                           (t :function))
               :docstring (documentation sym 'function)
               :source-location (get-source-location sym)))
        ;; エッジ作成
        (let ((callees (get-callees sym)))
          (dolist (callee callees)
            (when (gethash callee nodes)
              (push (make-graph-edge
                     :source sym
                     :target callee
                     :call-type :direct)
                    edges))))))
    (make-call-graph :nodes nodes :edges edges :root-package package)))
```

### canvas-buffer.lisp

```lisp
(defpackage :lem-living-canvas/buffer
  (:use :cl :lem)
  (:export #:canvas-buffer
           #:make-canvas-buffer
           #:canvas-buffer-graph
           #:canvas-buffer-source-buffer
           #:update-canvas))
(in-package :lem-living-canvas/buffer)

(defclass canvas-buffer (html-buffer)
  ((graph :initarg :graph
          :accessor canvas-buffer-graph
          :documentation "コールグラフデータ")
   (source-buffer :initarg :source-buffer
                  :accessor canvas-buffer-source-buffer
                  :documentation "解析対象のソースバッファ")
   (node-positions :initform (make-hash-table :test 'equal)
                   :accessor canvas-buffer-node-positions
                   :documentation "ノード位置のキャッシュ")))

(defun make-canvas-buffer (name source-buffer graph)
  "Canvasバッファを作成する"
  (let ((buffer (make-buffer name)))
    (change-class buffer 'canvas-buffer
                  :graph graph
                  :source-buffer source-buffer
                  :html (generate-canvas-html graph))
    buffer))
```

### commands.lisp

```lisp
(defpackage :lem-living-canvas/commands
  (:use :cl :lem)
  (:export #:living-canvas
           #:living-canvas-refresh
           #:living-canvas-jump-to-source))
(in-package :lem-living-canvas/commands)

;; JS→Lisp コールバック登録
(lem-server:register-method "canvas:node-selected"
  (lambda (args)
    (let ((node-id (gethash "nodeId" args)))
      (message "Selected: ~A" node-id))))

(lem-server:register-method "canvas:open-source"
  (lambda (args)
    (let ((node-id (gethash "nodeId" args)))
      (lem:send-event
       (lambda ()
         (jump-to-node-source node-id))))))

(lem-server:register-method "canvas:node-moved"
  (lambda (args)
    (let ((node-id (gethash "nodeId" args))
          (x (gethash "x" args))
          (y (gethash "y" args)))
      (save-node-position node-id x y))))

;; メインコマンド
(define-command living-canvas (package-name) ((:string "Package: "))
  "パッケージの関数呼び出しグラフをCanvasで表示"
  (let* ((source-buffer (current-buffer))
         (graph (analyze-package package-name))
         (canvas-buffer (make-canvas-buffer
                         (format nil "*Canvas: ~A*" package-name)
                         source-buffer
                         graph)))
    (pop-to-buffer canvas-buffer)))

(define-command living-canvas-current-buffer () ()
  "現在のバッファの関数グラフを表示"
  (let* ((buffer (current-buffer))
         (package (or (buffer-package buffer) *package*)))
    (living-canvas (package-name package))))

(define-command living-canvas-refresh () ()
  "Canvasを更新"
  (when (typep (current-buffer) 'canvas-buffer)
    (let* ((buffer (current-buffer))
           (source-buffer (canvas-buffer-source-buffer buffer))
           (graph (analyze-buffer source-buffer)))
      (setf (canvas-buffer-graph buffer) graph)
      (js-eval (current-window)
               (format nil "updateGraph(~A)"
                       (graph-to-cytoscape-json graph))))))
```

---

## JavaScript側実装

### Canvas Surface (Cytoscape.js)

```javascript
// canvas-surface.js
const cy = cytoscape({
  container: document.getElementById('cy'),
  style: [
    {
      selector: 'node',
      style: {
        'background-color': '#3c3c3c',
        'border-color': '#5a5a5a',
        'border-width': 2,
        'label': 'data(name)',
        'color': '#d4d4d4',
        'text-valign': 'center',
        'text-halign': 'center',
        'font-size': '11px',
        'font-family': 'Consolas, monospace',
        'width': 'label',
        'height': 32,
        'padding': '12px',
        'shape': 'roundrectangle'
      }
    },
    {
      selector: 'node[type="macro"]',
      style: { 'border-color': '#c586c0' }
    },
    {
      selector: 'node[type="generic-function"]',
      style: { 'border-color': '#4ec9b0' }
    },
    {
      selector: 'edge',
      style: {
        'width': 1.5,
        'line-color': '#454545',
        'target-arrow-color': '#454545',
        'target-arrow-shape': 'triangle',
        'curve-style': 'bezier'
      }
    },
    {
      selector: 'node:selected',
      style: {
        'border-color': '#007acc',
        'border-width': 3,
        'background-color': '#264f78'
      }
    },
    {
      selector: '.executing',
      style: {
        'background-color': '#4a9eff',
        'border-color': '#2d7ad9'
      }
    }
  ],
  layout: {
    name: 'dagre',
    rankDir: 'LR',
    nodeSep: 50,
    rankSep: 80
  }
});

// イベントハンドラ
cy.on('tap', 'node', (e) => {
  invokeLem('canvas:node-selected', { nodeId: e.target.id() });
});

cy.on('dbltap', 'node', (e) => {
  invokeLem('canvas:open-source', { nodeId: e.target.id() });
});

cy.on('dragfree', 'node', (e) => {
  const pos = e.target.position();
  invokeLem('canvas:node-moved', {
    nodeId: e.target.id(),
    x: pos.x,
    y: pos.y
  });
});

// Lisp側から呼び出される関数
window.updateGraph = (data) => {
  cy.json({ elements: data.elements });
  cy.layout({ name: 'dagre', rankDir: 'LR' }).run();
};

window.highlightNode = (nodeId) => {
  cy.$('.executing').removeClass('executing');
  cy.$('#' + nodeId).addClass('executing');
};
```

---

## 実装順序

### Phase 1 実装スケジュール

```
Week 1: 基盤
├── [1] call-graph.lisp - 関数抽出と依存解析
├── [2] package.lisp + .asd - プロジェクト構造
└── [3] 基本的なJSON出力

Week 2: 表示
├── [4] canvas-surface.js - Cytoscape統合
├── [5] canvas-mode.lisp - モード定義
└── [6] HTML生成とWebView統合

Week 3: インタラクション
├── [7] ノードクリック → ソースジャンプ
├── [8] ドラッグ&ドロップ
└── [9] 位置の永続化

Week 4: 仕上げ
├── [10] スタイル調整
├── [11] エラーハンドリング
└── [12] ドキュメント
```

---

## 将来の拡張

### Phase 2: 実行可視化

```lisp
;; trace-integration.lisp
(defun notify-canvas-enter (fn-name)
  "関数呼び出し開始をCanvasに通知"
  (when (canvas-visible-p)
    (js-eval-async
     (format nil "highlightNode('~A')" fn-name))))
```

### Phase 3: AI統合

```lisp
;; 付箋データ
(defstruct sticky-note
  id              ; ユニークID
  node-id         ; 関連するノードID
  content         ; プロンプトテキスト
  ai-response     ; AI生成結果
  status)         ; :pending, :generating, :ready, :applied
```

### Phase 4: 時間軸

```lisp
;; スナップショット
(defstruct graph-snapshot
  timestamp
  graph-data
  buffer-state
  description)
```

---

## 全体ロードマップ

```
Phase 1 (基盤)          Phase 2 (実行可視化)
├── 関数抽出            ├── trace統合
├── グラフ描画          ├── リアルタイムハイライト
├── ノード操作          └── パフォーマンス表示
└── ソースジャンプ
        │                       │
        └───────────┬───────────┘
                    ▼
            Phase 3 (AI統合)
            ├── 付箋システム
            ├── AI呼び出し
            └── コード適用
                    │
                    ▼
            Phase 4 (時間軸)
            ├── 履歴タイムライン
            ├── スナップショット
            └── 差分表示
                    │
                    ▼
              ★ 覇権への道 ★
```

---

## 作成日

2025-12-13

## 関連ドキュメント

- [ARCHITECTURE.md](./ARCHITECTURE.md) - Lemのアーキテクチャ
- [extension-development.md](./extension-development.md) - 拡張開発ガイド

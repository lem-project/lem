# Living Canvas Phase 2 実装計画

## 現在のステータス (2025-12-14更新)

| 項目 | 状態 |
|------|------|
| trace.lisp | ⬚ 未作成 |
| package.lisp更新 | ⬚ 未着手 |
| lem-living-canvas.asd更新 | ⬚ 未着手 |
| canvas-buffer.lisp修正 | ⬚ 未着手 |
| commands.lisp修正 | ⬚ 未着手 |
| JavaScript API追加 | ⬚ 未着手 |

**次のアクション**: Task 1から開始（パッケージ・システム定義更新）

---

## 概要

Phase 2では、コードの実行状態をリアルタイムでCanvasに可視化する機能を実装します。

## 調査結果サマリー

### 利用可能な基盤

| コンポーネント | 状態 | 用途 |
|--------------|------|------|
| `*event-hooks*` | 設計済み・未使用 | メッセージインターセプト |
| `js-eval` | 実装済み | Canvas通知 |
| `register-method` | 実装済み | JS→Lisp通信 |
| `micros/trace` | 実装済み | トレース制御 |
| `sb-introspect` | 使用中 | 関数解析 |

### アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│  SBCL trace出力 → *trace-output* (editor-io-stream)         │
│                          ↓                                   │
│  micros server → :write-string メッセージ                    │
│                          ↓                                   │
│  message-dispatcher.lisp: dispatch-message()                │
│      ├─ *event-hooks* ← 【Phase 2ここでインターセプト】      │
│      │       ↓                                               │
│      │   parse-trace-output()                               │
│      │       ↓                                               │
│      │   notify-canvas-execution-event()                    │
│      │       ↓                                               │
│      │   js-eval → JavaScript: handleExecutionEvent()       │
│      │                                                       │
│      └─ write-string-to-repl() → REPL表示                   │
└─────────────────────────────────────────────────────────────┘
```

---

## 実装ファイル

### 新規作成

```
extensions/living-canvas/
└── trace.lisp              # Phase 2 メイン実装
```

### 修正対象

```
extensions/living-canvas/
├── lem-living-canvas.asd   # trace.lisp 追加
├── package.lisp            # エクスポート追加
├── canvas-buffer.lisp      # JavaScript API追加、スロット追加
└── commands.lisp           # トレースコマンド追加
```

---

## 実装タスク

### Task 1: パッケージ・システム定義更新

**ファイル**: `package.lisp`, `lem-living-canvas.asd`

```lisp
;; package.lisp に追加
(defpackage :lem-living-canvas/trace
  (:use :cl :lem)
  (:import-from :lem-lisp-mode/message-dispatcher
                :*event-hooks*)
  (:import-from :lem-living-canvas/buffer
                :canvas-buffer
                :canvas-buffer-graph)
  (:export :execution-tracker
           :start-canvas-trace
           :stop-canvas-trace
           :canvas-trace-message-hook))

;; lem-living-canvas.asd に追加
:components (... (:file "trace"))
```

### Task 2: trace.lisp 作成

#### 2.1 データ構造

```lisp
;; 実行イベント
(defstruct execution-event
  node-id           ; "PACKAGE:FUNCTION"
  event-type        ; :enter, :exit, :error
  timestamp         ; (get-internal-real-time)
  depth             ; 呼び出し深度
  duration-ms       ; 実行時間（:exit時）
  args              ; 引数（:enter時、オプション）
  return-value)     ; 戻り値（:exit時、オプション）

;; 関数統計
(defstruct function-stats
  node-id
  call-count        ; 呼び出し回数
  total-time-ms     ; 合計実行時間
  max-time-ms       ; 最大実行時間
  min-time-ms       ; 最小実行時間
  error-count)      ; エラー回数

;; 実行追跡器
(defclass execution-tracker ()
  ((canvas-buffer :accessor tracker-canvas-buffer
                  :initarg :canvas-buffer)
   (traced-functions :accessor tracker-traced-functions
                     :initform '())
   (active-p :accessor tracker-active-p
             :initform nil)
   (stats :accessor tracker-stats
          :initform (make-hash-table :test 'equal))
   (event-history :accessor tracker-event-history
                  :initform '())
   (call-stack :accessor tracker-call-stack
               :initform '())
   (event-buffer :accessor tracker-event-buffer
                 :initform '())
   (flush-timer :accessor tracker-flush-timer
                :initform nil)))
```

#### 2.2 Trace出力パーサー

```lisp
(defun trace-output-p (string)
  "文字列がSBCL trace出力形式かチェック"
  ;; SBCL形式: "  0: (FUNC arg1 arg2)" または "  0: FUNC returned VALUE"
  (ppcre:scan "^\\s*\\d+:\\s*(?:\\(|[A-Z].*returned)" string))

(defun parse-trace-output (string)
  "SBCL trace出力をexecution-eventに変換"
  ;; 深度: leading spaces の数から計算
  ;; event-type: 括弧あり → :enter, 'returned' → :exit
  ;; node-id: 関数名を抽出
  ...)
```

#### 2.3 メッセージフック

```lisp
(defvar *execution-tracker* nil
  "現在アクティブなexecution-tracker")

(defun canvas-trace-message-hook (message)
  "trace出力をキャプチャしてCanvasに通知"
  (when (and *execution-tracker*
             (tracker-active-p *execution-tracker*)
             (eq (car message) :write-string))
    (let ((string (cadr message)))
      (when (trace-output-p string)
        (let ((event (parse-trace-output string)))
          (when event
            (record-execution-event *execution-tracker* event)
            (queue-canvas-notification event))))))
  nil)  ;; nil = 標準ハンドラも実行
```

#### 2.4 Canvas通知（バッチ処理）

```lisp
(defvar *notification-interval* 0.1
  "Canvas通知の間隔（秒）")

(defun queue-canvas-notification (event)
  "イベントをバッファに追加、タイマーで一括送信"
  (push event (tracker-event-buffer *execution-tracker*))
  (unless (tracker-flush-timer *execution-tracker*)
    (setf (tracker-flush-timer *execution-tracker*)
          (lem:start-timer *notification-interval* nil
                           'flush-execution-events))))

(defun flush-execution-events ()
  "バッファのイベントをCanvasに送信"
  (when *execution-tracker*
    (let ((events (nreverse (tracker-event-buffer *execution-tracker*))))
      (setf (tracker-event-buffer *execution-tracker*) '())
      (setf (tracker-flush-timer *execution-tracker*) nil)
      (when events
        (notify-canvas-execution-events events)))))

(defun notify-canvas-execution-events (events)
  "イベントリストをJavaScriptに送信"
  (let ((json (events-to-json events)))
    (lem:js-eval (canvas-window)
                 (format nil "handleExecutionEvents(~A)" json))))
```

#### 2.5 統計収集

```lisp
(defun record-execution-event (tracker event)
  "イベントを記録して統計を更新"
  (let ((node-id (execution-event-node-id event))
        (event-type (execution-event-event-type event)))
    ;; イベント履歴に追加
    (push event (tracker-event-history tracker))
    ;; 統計更新
    (case event-type
      (:enter
       (push (cons node-id (get-internal-real-time))
             (tracker-call-stack tracker)))
      (:exit
       (let* ((start-entry (pop (tracker-call-stack tracker)))
              (duration-ms (/ (- (get-internal-real-time) (cdr start-entry))
                              (/ internal-time-units-per-second 1000.0))))
         (update-function-stats tracker node-id duration-ms)))
      (:error
       (incf-error-count tracker node-id)))))

(defun update-function-stats (tracker node-id duration-ms)
  "関数統計を更新"
  (let ((stats (gethash node-id (tracker-stats tracker))))
    (unless stats
      (setf stats (make-function-stats :node-id node-id
                                       :call-count 0
                                       :total-time-ms 0.0
                                       :max-time-ms 0.0
                                       :min-time-ms most-positive-single-float
                                       :error-count 0))
      (setf (gethash node-id (tracker-stats tracker)) stats))
    (incf (function-stats-call-count stats))
    (incf (function-stats-total-time-ms stats) duration-ms)
    (setf (function-stats-max-time-ms stats)
          (max (function-stats-max-time-ms stats) duration-ms))
    (setf (function-stats-min-time-ms stats)
          (min (function-stats-min-time-ms stats) duration-ms))))
```

#### 2.6 トレース制御

```lisp
(defun start-canvas-trace (canvas-buffer)
  "Canvas表示中の関数のトレースを開始"
  (let ((tracker (make-instance 'execution-tracker
                                :canvas-buffer canvas-buffer)))
    (setf *execution-tracker* tracker)
    ;; グラフ内の全関数をトレース
    (let ((graph (canvas-buffer-graph canvas-buffer)))
      (maphash (lambda (node-id node)
                 (declare (ignore node))
                 (let ((symbol (node-id-to-symbol node-id)))
                   (when (and symbol (fboundp symbol))
                     (lem-lisp-mode:lisp-eval
                      `(trace ,symbol))
                     (push symbol (tracker-traced-functions tracker)))))
               (call-graph-nodes graph)))
    ;; フック登録
    (pushnew 'canvas-trace-message-hook
             lem-lisp-mode/message-dispatcher:*event-hooks*)
    (setf (tracker-active-p tracker) t)
    ;; JavaScript側を初期化
    (lem:js-eval (canvas-window) "initializeTracking()")
    tracker))

(defun stop-canvas-trace ()
  "トレースを停止"
  (when *execution-tracker*
    (setf (tracker-active-p *execution-tracker*) nil)
    ;; 全関数をuntrace
    (dolist (symbol (tracker-traced-functions *execution-tracker*))
      (lem-lisp-mode:lisp-eval `(untrace ,symbol)))
    ;; フック解除
    (setf lem-lisp-mode/message-dispatcher:*event-hooks*
          (remove 'canvas-trace-message-hook
                  lem-lisp-mode/message-dispatcher:*event-hooks*))
    ;; JavaScript側を停止
    (lem:js-eval (canvas-window) "stopTracking()")
    (setf *execution-tracker* nil)))
```

### Task 3: canvas-buffer.lisp 修正

#### 3.1 スロット追加

```lisp
(defclass canvas-buffer (lem:html-buffer)
  ((graph ...)
   (source-buffer ...)
   (node-positions ...)
   ;; Phase 2 追加
   (trace-enabled :initform nil
                  :accessor canvas-buffer-trace-enabled-p)
   (execution-tracker :initform nil
                      :accessor canvas-buffer-execution-tracker)))
```

#### 3.2 JavaScript API追加

HTMLテンプレートに以下を追加:

```javascript
// Phase 2: 実行可視化API
window.initializeTracking = function() {
  console.log('Tracking initialized');
  // トラッキング状態の表示
  document.getElementById('tracking-status').textContent = 'Tracking: ON';
};

window.stopTracking = function() {
  console.log('Tracking stopped');
  cy.elements().removeClass('executing');
  document.getElementById('tracking-status').textContent = 'Tracking: OFF';
};

window.handleExecutionEvent = function(event) {
  const nodeId = event.nodeId;
  const type = event.type;
  const node = cy.$('#' + CSS.escape(nodeId));

  if (node.length === 0) return;

  if (type === 'enter') {
    node.addClass('executing');
    // アニメーション
    cy.animate({
      center: { eles: node },
      duration: 200
    });
  } else if (type === 'exit') {
    node.removeClass('executing');
  } else if (type === 'error') {
    node.addClass('error');
  }
};

window.handleExecutionEvents = function(events) {
  events.forEach(e => window.handleExecutionEvent(e));
};

window.updateStats = function(stats) {
  // ヒートマップ更新
  Object.keys(stats).forEach(nodeId => {
    const node = cy.$('#' + CSS.escape(nodeId));
    if (node.length > 0) {
      const s = stats[nodeId];
      // 呼び出し回数に応じて色を変更
      const intensity = Math.min(s.callCount / 100, 1);
      node.style('background-color', `rgba(255, ${255 - intensity * 200}, 0, 0.8)`);
      // ツールチップ更新
      node.data('stats', `Calls: ${s.callCount}, Total: ${s.totalTimeMs.toFixed(1)}ms`);
    }
  });
};

window.clearStats = function() {
  cy.nodes().style('background-color', '#3c3c3c');
  cy.nodes().removeData('stats');
};
```

#### 3.3 CSS追加

```css
/* 実行中ノード */
node.executing {
  background-color: #4a9eff !important;
  border-color: #2d7ad9 !important;
  border-width: 3px !important;
}

/* エラーノード */
node.error {
  background-color: #ff4444 !important;
  border-color: #cc0000 !important;
}

/* ステータス表示 */
#tracking-status {
  position: fixed;
  top: 10px;
  right: 10px;
  padding: 5px 10px;
  background: rgba(0,0,0,0.7);
  color: #4a9eff;
  font-family: monospace;
  border-radius: 4px;
}
```

### Task 4: commands.lisp 修正

```lisp
;; トレース切り替え
(lem:define-command living-canvas-toggle-trace () ()
  "Toggle execution tracing for the canvas"
  (when (typep (lem:current-buffer) 'canvas-buffer)
    (let ((buffer (lem:current-buffer)))
      (if (canvas-buffer-trace-enabled-p buffer)
          (progn
            (stop-canvas-trace)
            (setf (canvas-buffer-trace-enabled-p buffer) nil)
            (lem:message "Trace stopped"))
          (progn
            (start-canvas-trace buffer)
            (setf (canvas-buffer-trace-enabled-p buffer) t)
            (lem:message "Trace started"))))))

;; 全関数トレース
(lem:define-command living-canvas-trace-all () ()
  "Trace all functions in the graph"
  (when (typep (lem:current-buffer) 'canvas-buffer)
    (start-canvas-trace (lem:current-buffer))
    (lem:message "All functions traced")))

;; 全関数アントレース
(lem:define-command living-canvas-untrace-all () ()
  "Untrace all functions"
  (stop-canvas-trace)
  (lem:message "All functions untraced"))

;; 統計表示
(lem:define-command living-canvas-show-stats () ()
  "Show execution statistics"
  (when *execution-tracker*
    (let ((stats-json (stats-to-json (tracker-stats *execution-tracker*))))
      (lem:js-eval (canvas-window)
                   (format nil "updateStats(~A)" stats-json)))))

;; 統計クリア
(lem:define-command living-canvas-clear-stats () ()
  "Clear execution statistics"
  (when *execution-tracker*
    (clrhash (tracker-stats *execution-tracker*))
    (setf (tracker-event-history *execution-tracker*) '())
    (lem:js-eval (canvas-window) "clearStats()")))

;; キーバインド
(lem:define-key *living-canvas-mode-keymap* "t" 'living-canvas-toggle-trace)
(lem:define-key *living-canvas-mode-keymap* "T" 'living-canvas-trace-all)
(lem:define-key *living-canvas-mode-keymap* "U" 'living-canvas-untrace-all)
(lem:define-key *living-canvas-mode-keymap* "s" 'living-canvas-show-stats)
(lem:define-key *living-canvas-mode-keymap* "C" 'living-canvas-clear-stats)
```

---

## 実装順序

```
Step 1: 基盤準備
├── [1.1] package.lisp 更新（trace パッケージ追加）
├── [1.2] lem-living-canvas.asd 更新（trace.lisp 追加）
└── [1.3] trace.lisp 骨格作成

Step 2: データ構造
├── [2.1] execution-event 構造体
├── [2.2] function-stats 構造体
└── [2.3] execution-tracker クラス

Step 3: Trace出力処理
├── [3.1] trace-output-p 判定関数
├── [3.2] parse-trace-output パーサー
└── [3.3] canvas-trace-message-hook フック

Step 4: Canvas通知
├── [4.1] queue-canvas-notification バッファリング
├── [4.2] flush-execution-events 一括送信
└── [4.3] notify-canvas-execution-events JSON送信

Step 5: 統計収集
├── [5.1] record-execution-event イベント記録
└── [5.2] update-function-stats 統計更新

Step 6: トレース制御
├── [6.1] start-canvas-trace 開始
└── [6.2] stop-canvas-trace 停止

Step 7: JavaScript実装
├── [7.1] initializeTracking / stopTracking
├── [7.2] handleExecutionEvent / handleExecutionEvents
├── [7.3] updateStats / clearStats
└── [7.4] CSS追加

Step 8: コマンド実装
├── [8.1] living-canvas-toggle-trace
├── [8.2] living-canvas-trace-all / untrace-all
├── [8.3] living-canvas-show-stats / clear-stats
└── [8.4] キーバインド設定

Step 9: テスト・デバッグ
├── [9.1] 単体テスト
├── [9.2] 統合テスト
└── [9.3] パフォーマンステスト
```

---

## 依存関係

### Lisp側

```lisp
;; 必要なパッケージ
:lem-lisp-mode/message-dispatcher  ; *event-hooks*
:lem-lisp-mode/internal            ; lisp-eval
:lem-living-canvas/call-graph      ; call-graph-nodes
:lem-living-canvas/buffer          ; canvas-buffer
:yason                             ; JSONエンコード
:cl-ppcre                          ; 正規表現パース
```

### JavaScript側

```javascript
// 既存ライブラリ
Cytoscape.js  // グラフ描画（既存）

// 追加ライブラリ不要
```

---

## 注意事項

### パフォーマンス

- trace出力が大量の場合、バッチ処理で負荷軽減
- `*notification-interval*` で送信頻度を調整
- イベント履歴の容量制限（10000件程度）

### スレッド安全性

- `*event-hooks*` 操作時のスレッド考慮
- イベントバッファのアトミック操作

### エラーハンドリング

- trace対象関数が見つからない場合の警告
- JSON変換失敗時のフォールバック
- Canvas接続切断時の graceful degradation

---

## 作成日

2025-12-14

## 関連ドキュメント

- [LIVING-CANVAS-DESIGN.md](./LIVING-CANVAS-DESIGN.md) - 全体設計
- [ARCHITECTURE.md](./ARCHITECTURE.md) - Lemアーキテクチャ

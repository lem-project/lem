---
title: "ARCHITECTURE"
repo: "lem"
version: "v1"
generated_at: "2025-12-20T10:54:19+09:00"
commit: "1913485a10fa7ed52b78ec6a78ac28f911f1d366"
confidence: "High"
focus: "feature/tree-sitter-syntax-highlighting branch"
---

# Tree-sitter Syntax Highlighting Architecture

このドキュメントはfeature/tree-sitter-syntax-highlightingブランチで導入されるtree-sitter統合機能のアーキテクチャを記述する。

## 1. Overview

### 目的
- Lemエディタにtree-sitterベースの構文ハイライトを導入
- 既存のTextMateスタイルの正規表現パーサーを補完・置換
- より正確で高速な構文解析を提供

### ユースケース
- JSON/YAML/Markdownファイルの構文ハイライト
- インクリメンタルパース（編集時の差分更新）
- 将来の言語サポート拡張の基盤

### 非目標
- 全言語モードの即時置換（オプトイン方式）
- tree-sitterが利用不可な環境での必須化

## 2. System Architecture

```mermaid
flowchart TB
    subgraph Core ["Lem Core"]
        SyntaxParser["syntax-parser (base class)"]
        SyntaxScan["syntax-scan-region"]
        Buffer["Buffer"]
    end

    subgraph TreeSitter ["lem-tree-sitter extension"]
        TSParser["treesitter-parser"]
        Highlight["highlight.lisp"]
        Integration["integration.lisp"]
        Queries["queries/*.scm"]
    end

    subgraph External ["External Library"]
        TSLib["tree-sitter-cl"]
        Grammars["Language Grammars (JSON/YAML/Markdown)"]
    end

    subgraph Modes ["Language Modes"]
        JSON["json-mode"]
        YAML["yaml-mode"]
        Markdown["markdown-mode"]
    end

    SyntaxParser --> TSParser
    TSParser --> Integration
    Integration --> Highlight
    Highlight --> Queries
    Integration --> TSLib
    TSLib --> Grammars

    JSON --> Integration
    YAML --> Integration
    Markdown --> Integration

    SyntaxScan --> TSParser
    Buffer --> SyntaxScan
```

## 3. Execution Flow

### 3.1 構文スキャンシーケンス

```mermaid
sequenceDiagram
    participant Buffer
    participant SyntaxScan as syntax-scan-region
    participant TSParser as treesitter-parser
    participant Integration as integration.lisp
    participant Highlight as highlight.lisp
    participant TSLib as tree-sitter-cl

    Buffer->>SyntaxScan: syntax-scan-region(start, end)
    SyntaxScan->>TSParser: %syntax-scan-region(parser, start, end)
    TSParser->>Integration: get-buffer-text(buffer)
    Integration->>Integration: clear-attributes-in-region(start, end)
    Integration->>TSLib: parser-parse-string(parser, source, old-tree)
    TSLib-->>Integration: syntax tree
    Integration->>TSLib: query-captures-in-range(query, root, start-byte, end-byte)
    TSLib-->>Integration: captures list
    loop For each capture
        Integration->>Highlight: capture-to-attribute(capture-name)
        Highlight-->>Integration: lem attribute
        Integration->>Buffer: put-text-property(start, end, :attribute, attr)
    end
```

### 3.2 インクリメンタルパースフロー

```mermaid
sequenceDiagram
    participant User
    participant Buffer
    participant Hook as after-change-functions
    participant TSParser as treesitter-parser
    participant TSLib as tree-sitter-cl

    User->>Buffer: Edit text
    Buffer->>Hook: trigger(start, end, old-len)
    Hook->>TSParser: record-tree-sitter-edit(start, end, old-len)
    TSParser->>TSParser: push edit to pending-edits
    Note over TSParser: On next syntax-scan-region
    TSParser->>TSLib: tree-edit(old-tree, edit)
    TSParser->>TSLib: parser-parse-string(parser, source, edited-tree)
    TSLib-->>TSParser: Incremental parse result
```

## 4. Modules & Dependencies

### 4.1 レイヤー構成

| Layer | Package | Role |
|-------|---------|------|
| External | `tree-sitter-cl` | tree-sitterのCommon Lispバインディング |
| Extension | `lem-tree-sitter` | Lem統合レイヤー |
| Extension | `lem-tree-sitter/highlight` | キャプチャ→属性マッピング |
| Mode | `lem-json-mode`, `lem-yaml-mode`, `lem-markdown-mode` | 言語固有の有効化 |

### 4.2 主要コンポーネント

#### treesitter-parser class
- `extensions/tree-sitter/integration.lisp:5-25`
- `lem/buffer/internal::syntax-parser`を継承
- スロット: `language-name`, `parser`, `tree`, `highlight-query`, `source-cache`, `pending-edits`

#### enable-tree-sitter-for-mode
- `extensions/tree-sitter/integration.lisp:151-169`
- 既存モードのsyntax-tableにtree-sitterパーサーを設定
- grammar/queryが存在しない場合は静かにフォールバック

#### capture-to-attribute
- `extensions/tree-sitter/highlight.lisp:12-20`
- tree-sitterのキャプチャ名をLemの属性に変換
- 階層的ルックアップ対応（`keyword.control` → `keyword`）

### 4.3 依存関係

```
lem-tree-sitter
├── lem/core
├── tree-sitter-cl
└── alexandria

lem-json-mode
├── lem/core
├── lem-js-mode
└── lem-tree-sitter  # NEW

lem-yaml-mode
├── lem/core
└── lem-tree-sitter  # NEW

lem-markdown-mode
├── (existing deps)
└── lem-tree-sitter  # NEW
```

## 5. Data Model

### 5.1 Query Files (highlights.scm)

| Language | Path | Captures |
|----------|------|----------|
| JSON | `queries/json/highlights.scm` | string, number, boolean, property, punctuation |
| YAML | `queries/yaml/highlights.scm` | scalar types, comment, anchor, tag, property |
| Markdown | `queries/markdown/highlights.scm` | headers (h1-h6), code blocks, quotes, lists |

### 5.2 Capture → Attribute Mapping

| Capture Category | Lem Attribute |
|-----------------|---------------|
| `keyword*` | `syntax-keyword-attribute` |
| `string*` | `syntax-string-attribute` |
| `comment*` | `syntax-comment-attribute` |
| `number*` | `syntax-constant-attribute` |
| `function*` | `syntax-function-name-attribute` |
| `type*` | `syntax-type-attribute` |
| `markup.heading.N` | `document-headerN-attribute` |

Reference: `extensions/tree-sitter/highlight.lisp:26-143`

## 6. Configuration

### 6.1 有効化メカニズム

各言語モードの`define-major-mode`本体で呼び出し:

```lisp
;; extensions/json-mode/json-mode.lisp:58
(lem-tree-sitter:enable-tree-sitter-for-mode *json-syntax-table* "json")

;; extensions/yaml-mode/yaml-mode.lisp:48
(lem-tree-sitter:enable-tree-sitter-for-mode *yaml-syntax-table* "yaml")

;; extensions/markdown-mode/markdown-mode.lisp:21
(lem-tree-sitter:enable-tree-sitter-for-mode *markdown-syntax-table* "markdown")
```

### 6.2 条件付きビルド

- `lem.asd`で`#+nix-build`条件でのみ`lem-tree-sitter`を含める
- `extensions/tree-sitter/integration.lisp:155-168`でエラーを静かに処理

## 7. Build & Release

### 7.1 依存ライブラリ

- `qlfile:13`: `git tree-sitter-cl https://github.com/lem-project/tree-sitter-cl.git`
- Nixビルド: `flake.nix`で言語grammarを含める

### 7.2 テスト

- テストファイル: `extensions/tree-sitter/tests/main.lisp`
- テストシステム: `lem-tree-sitter/tests`
- テスト内容:
  - `test-tree-sitter-availability`: ライブラリ検出
  - `test-capture-mapping`: 属性マッピング
  - `test-parser-creation`: パーサー生成
  - `test-yaml-buffer-attributes`: 実際の構文ハイライト適用

## 8. Risks & Improvements

### 8.1 リスク

1. **内部シンボル依存** (`lem/buffer/internal::syntax-parser`, `lem/buffer/internal::%syntax-scan-region`)
   - `extensions/tree-sitter/integration.lisp:5,40`
   - 将来のリファクタリングで破損する可能性

2. **Byte offset計算の複雑さ**
   - `extensions/tree-sitter/integration.lisp:126-136`
   - `move-to-bytes`のoff-by-one動作への対処が複雑

3. **インクリメンタルパースの近似**
   - `extensions/tree-sitter/integration.lisp:207-213`
   - `old-len`がバイト数ではなく文字数のため正確な`old-end-point`計算ができない

4. **Nix限定の統合**
   - `lem.asd:304-305`で`#+nix-build`条件
   - Quicklisp環境での利用が制限される可能性

### 8.2 改善案

1. **公開APIの追加**: `syntax-parser`基底クラスと`%syntax-scan-region`をpublicにエクスポート
2. **バイト単位の編集追跡**: `after-change-functions`フックでバイトオフセットを提供
3. **非Nix環境サポート**: grammarのダイナミックローディング機構の追加
4. **追加言語対応**: Python, JavaScript, Common Lispのquery追加

## 9. Open Questions

1. tree-sitter-clの`tree-edit`は古いtreeをin-placeで変更するか、新しいtreeを返すか？
   - 現在の実装は前者を仮定: `integration.lisp:67`

2. Markdownのinline要素（bold, italic）がブロックgrammarのみで処理されるのは意図的か？
   - `queries/markdown/highlights.scm`にはinline要素のクエリがない

3. grammarが利用不可な場合のフォールバック動作は適切か？
   - 現在は静かに元のtmlanguageパーサーを使用

## 10. References

### コードエビデンス

| Description | Location |
|-------------|----------|
| treesitter-parser class definition | `extensions/tree-sitter/integration.lisp:5-25` |
| %syntax-scan-region implementation | `extensions/tree-sitter/integration.lisp:40-50` |
| enable-tree-sitter-for-mode | `extensions/tree-sitter/integration.lisp:151-169` |
| Capture mapping definitions | `extensions/tree-sitter/highlight.lisp:26-143` |
| Incremental edit recording | `extensions/tree-sitter/integration.lisp:190-221` |
| JSON mode integration | `extensions/json-mode/json-mode.lisp:58` |
| YAML mode integration | `extensions/yaml-mode/yaml-mode.lisp:48` |
| Markdown mode integration | `extensions/markdown-mode/markdown-mode.lisp:21` |
| Base syntax-parser class | `src/buffer/internal/syntax-parser.lisp:16-17` |
| %syntax-scan-region generic | `src/buffer/internal/syntax-parser.lisp:19` |
| qlfile tree-sitter-cl entry | `qlfile:13` |
| Conditional build in lem.asd | `lem.asd:304-305` |

### 変更されたファイル一覧

| File | Change Type |
|------|-------------|
| `extensions/tree-sitter/*` | NEW |
| `extensions/json-mode/json-mode.lisp` | MODIFIED |
| `extensions/yaml-mode/yaml-mode.lisp` | MODIFIED |
| `extensions/markdown-mode/markdown-mode.lisp` | MODIFIED |
| `extensions/*/lem-*-mode.asd` | MODIFIED (dependency) |
| `lem.asd` | MODIFIED |
| `qlfile` | MODIFIED |
| `flake.nix` | MODIFIED |

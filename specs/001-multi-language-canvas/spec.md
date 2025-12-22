# Feature Specification: Multi-Language Living Canvas

**Feature Branch**: `001-multi-language-canvas`
**Created**: 2025-12-23
**Status**: Draft
**Input**: User description: "Living Canvasは現状Common Lispのコードに対してしか使えないが、様々な言語で使えるものがほしい"

## Clarifications

### Session 2025-12-23

- Q: 解析アプローチの優先順位（静的解析 vs LSP） → A: 静的解析（AST/tree-sitter）を優先、LSPはオプショナルな拡張

## Overview

Living Canvasを拡張し、Common Lisp以外の複数のプログラミング言語でも関数呼び出しグラフを可視化できるようにする。現在の実装はSBCLのintrospection機能（sb-introspect、micros）に依存しているが、プラグイン可能な言語プロバイダーアーキテクチャを導入することで、各言語に適した解析バックエンドを利用可能にする。

## User Scenarios & Testing *(mandatory)*

### User Story 1 - 言語自動検出による統一的なユーザー体験 (Priority: P1)

開発者として、どの言語のファイルを開いていても、同じ`living-canvas-current-file`コマンドで言語に適したグラフが表示される。言語の違いを意識せずに操作できる。

**Why this priority**: 多言語対応の核心機能であり、これがなければ他のユーザーストーリーが成立しない。統一的なユーザー体験がこの機能の価値の根幹。

**Independent Test**: 異なる言語のファイル（Python, JS, Lispなど）を開き、同じコマンドを実行して各言語に適したグラフが表示されることを確認。

**Acceptance Scenarios**:

1. **Given** Pythonファイルが開いている状態, **When** `living-canvas-current-file`を実行, **Then** Pythonプロバイダーが自動選択されグラフが表示される
2. **Given** Common Lispファイルが開いている状態, **When** `living-canvas-current-file`を実行, **Then** 従来のLispプロバイダーが使用されグラフが表示される
3. **Given** サポートされていない言語のファイルが開いている状態, **When** `living-canvas-current-file`を実行, **Then** 「この言語はまだサポートされていません」というメッセージが表示される

---

### User Story 2 - Python開発者がPythonプロジェクトの関数グラフを表示 (Priority: P1)

Python開発者として、Pythonファイルを開いてLiving Canvasを起動すると、Pythonの関数・クラスメソッドの呼び出しグラフが表示される。ノードをクリックすると対応するソースコードにジャンプできる。

**Why this priority**: Pythonは最も広く使用されているプログラミング言語の一つであり、多くのLemユーザーがPython開発を行っている。Pythonサポートにより、機能の有用性が大幅に向上する。

**Independent Test**: Pythonファイルを開き、`living-canvas-current-file`コマンドを実行することで、Pythonの関数グラフが表示されることを確認できる。

**Acceptance Scenarios**:

1. **Given** Pythonファイルが開いている状態, **When** `living-canvas-current-file`を実行, **Then** そのファイル内の関数・メソッドがノードとして表示される
2. **Given** Pythonプロジェクト内にいる状態, **When** `living-canvas`コマンドでモジュール名を指定, **Then** 指定モジュール内の全関数のグラフが表示される
3. **Given** グラフが表示されている状態, **When** ノードをダブルクリック, **Then** 対応するPythonソースコードの関数定義にジャンプする

---

### User Story 3 - JavaScript/TypeScript開発者がJSプロジェクトの関数グラフを表示 (Priority: P2)

JavaScript/TypeScript開発者として、JS/TSファイルを開いてLiving Canvasを起動すると、関数やクラスメソッドの呼び出しグラフが表示される。ESモジュールのインポート関係も可視化される。

**Why this priority**: JavaScript/TypeScriptはWeb開発において最も重要な言語であり、複雑なモジュール依存関係の可視化ニーズが高い。

**Independent Test**: TypeScriptファイルを開き、`living-canvas-current-file`コマンドを実行することで、関数グラフが表示されることを確認できる。

**Acceptance Scenarios**:

1. **Given** TypeScriptファイルが開いている状態, **When** `living-canvas-current-file`を実行, **Then** export された関数・クラスがノードとして表示される
2. **Given** npmプロジェクト内にいる状態, **When** `living-canvas-system`でパッケージ名を指定, **Then** パッケージ内の関数グラフが表示される
3. **Given** グラフが表示されている状態, **When** ノードをダブルクリック, **Then** 対応するJS/TSソースコードの定義にジャンプする

---

### User Story 4 - 新しい言語プロバイダーの追加 (Priority: P3)

拡張開発者として、新しいプログラミング言語のサポートをプラグインとして追加できる。プロバイダーインターフェースに従って実装することで、Living Canvasの全機能が新言語で利用可能になる。

**Why this priority**: 将来の拡張性を確保し、コミュニティによる言語サポートの追加を可能にする。

**Independent Test**: サンプルプロバイダーを作成し、登録してテスト用ファイルでグラフが表示されることを確認。

**Acceptance Scenarios**:

1. **Given** プロバイダーAPIドキュメントがある状態, **When** 新言語プロバイダーを実装, **Then** 必要な関数を定義するだけでLiving Canvasに統合される
2. **Given** プロバイダーが登録されている状態, **When** 対応する言語のファイルを開く, **Then** 登録したプロバイダーが自動的に使用される

---

### Edge Cases

- **サポートされていない言語**: 適切なエラーメッセージを表示し、将来のサポート計画があれば案内する
- **解析エラー**: 構文エラーのあるファイルでも可能な範囲でグラフを表示し、エラー箇所を示す
- **大規模プロジェクト**: 数千のノードがある場合のパフォーマンス対策（クラスタリング、遅延読み込み）
- **言語サーバー未起動**: LSPサーバーに依存するプロバイダーで、サーバーが起動していない場合は静的解析にフォールバック
- **混合言語プロジェクト**: 単一ファイルでは単一言語を仮定し、プロジェクト全体では各ファイルの言語を個別判定

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: システムは、ファイルの拡張子またはメジャーモードから言語を自動検出しなければならない
- **FR-002**: システムは、プラグイン可能な言語プロバイダーアーキテクチャを提供しなければならない
- **FR-003**: 各言語プロバイダーは、ファイル解析機能（関数抽出）を実装しなければならない
- **FR-004**: 各言語プロバイダーは、呼び出し関係解析機能を実装しなければならない
- **FR-005**: 各言語プロバイダーは、ソース位置情報（ファイル:行番号）を提供しなければならない
- **FR-006**: システムは、Common Lisp、Python、JavaScript/TypeScriptの最低3言語を初期サポートしなければならない
- **FR-007**: 既存のCommon Lispプロバイダー（micros連携）は、新アーキテクチャでも動作し続けなければならない
- **FR-008**: ユーザーは、同じコマンドセット（`living-canvas`, `living-canvas-current-file`など）で全言語を操作できなければならない
- **FR-009**: プロバイダーが利用不可の場合、システムはユーザーに明確なエラーメッセージを表示しなければならない
- **FR-010**: 各プロバイダーは、ノード詳細情報（引数リスト、docstring相当、型情報）を可能な範囲で提供しなければならない

### Key Entities

- **Language Provider**: 特定の言語の解析機能を提供するプラグインコンポーネント。言語検出、関数抽出、呼び出し解析を担当
- **Graph Node**: 関数またはメソッドを表すノード。名前、型、ソース位置、言語固有のメタデータを持つ
- **Graph Edge**: 関数間の呼び出し関係を表すエッジ。呼び出し元と呼び出し先のノードIDを持つ
- **Call Graph**: ノードとエッジの集合。言語に依存しない共通データ構造
- **Provider Registry**: 登録された言語プロバイダーを管理し、言語検出に基づいて適切なプロバイダーを選択

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: ユーザーは、Python/JS/TSファイルからLiving Canvasを起動し、30秒以内に関数グラフを表示できる
- **SC-002**: ノードダブルクリックからソースジャンプまで1秒以内に完了する
- **SC-003**: 100関数以下のファイルでは、グラフ生成が3秒以内に完了する
- **SC-004**: 新しい言語プロバイダーを追加するために必要なコードは300行以下である
- **SC-005**: 既存のCommon Lispワークフローが新アーキテクチャ導入後も同一の操作で動作する

## Assumptions

- 各言語プロバイダーは静的解析（AST/tree-sitter）を主要な解析手段として使用する
- Python解析にはPython標準ライブラリのASTモジュールを使用する
- JavaScript/TypeScript解析にはtree-sitterを使用する
- LSP連携はオプショナルな拡張機能として提供し、より詳細な型情報や参照解析に活用する
- プロバイダーは外部プロセス（言語サーバー等）に依存せず単独で動作する
- 初期リリースでは実行追跡（Phase 2機能）は各言語プロバイダーでオプショナルとする

## Out of Scope

- 実行時トレース機能の多言語対応（Phase 2以降）
- SLDB統合ステップ実行の多言語対応（Phase 2.5以降）
- AI統合機能の多言語対応（Phase 3以降）
- 言語間の呼び出し関係の可視化（例：PythonからCの呼び出し）

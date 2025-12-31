# Clojure Sample Project

Sample Clojure project for testing `lem-clojure-mode`.

## Requirements

- Nix with flakes enabled (recommended)
- Or: Clojure CLI tools (`clj`) + clojure-lsp

## Setup with Nix Flakes

```bash
cd samples/clojure-sample

# Enter development shell (includes clojure, clojure-lsp, jdk21)
nix develop

# Or with direnv (automatic)
direnv allow
```

This provides:
- `clojure` / `clj` - Clojure CLI
- `clojure-lsp` - Language server for LSP features
- `jdk21` - Java runtime
- `babashka` - Fast Clojure scripting

## Usage

### Start nREPL Server

```bash
clj -M:nrepl
```

This starts an nREPL server with CIDER middleware on a random port.
Connect from Lem using `M-x clojure-connect`.

### Run Tests

```bash
clj -X:test
```

### Run Main

```bash
clj -M -m sample.core
```

## Features to Test in Lem

1. **Syntax Highlighting**
   - Open `src/sample/core.clj`
   - Verify keywords, strings, comments, special forms are highlighted

2. **Indentation**
   - Edit code and verify proper indentation with Tab

3. **nREPL Integration**
   - Start nREPL: `clj -M:nrepl`
   - Connect: `M-x clojure-connect`
   - Eval: `C-c C-e` (eval last sexp)
   - Eval defun: `C-c C-c`

4. **Documentation**
   - `M-x clojure-describe-symbol` on a symbol

5. **Macroexpand**
   - `M-x clojure-macroexpand-1` on a macro form

6. **Test Runner**
   - `M-x clojure-run-tests`

## Project Structure

```
clojure-sample/
├── flake.nix             # Nix flake (clojure, clojure-lsp, jdk21)
├── .envrc                # direnv configuration
├── deps.edn              # Clojure project configuration
├── src/
│   └── sample/
│       └── core.clj      # Main source file
└── test/
    └── sample/
        └── core_test.clj # Test file
```

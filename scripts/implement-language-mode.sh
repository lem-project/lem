#!/usr/bin/env bash
# implement-language-mode.sh
# Fully automated language mode implementation for Lem editor
#
# Usage:
#   ./scripts/implement-language-mode.sh [language]
#   ./scripts/implement-language-mode.sh           # Interactive: prompts for language
#   ./scripts/implement-language-mode.sh kotlin    # Direct: implements Kotlin mode
#
# Requirements:
#   - gh (GitHub CLI) authenticated
#   - claude (Claude Code CLI)
#   - git configured with push access

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
ISSUE_NUMBER="2066"
REPO="lem-project/lem"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() { echo -e "${BLUE}[INFO]${NC} $1" >&2; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1" >&2; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1" >&2; }
log_error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    if ! command -v gh &> /dev/null; then
        log_error "gh (GitHub CLI) is not installed"
        exit 1
    fi

    if ! gh auth status &> /dev/null; then
        log_error "gh is not authenticated. Run 'gh auth login' first"
        exit 1
    fi

    if ! command -v claude &> /dev/null; then
        log_error "claude (Claude Code CLI) is not installed"
        exit 1
    fi

    log_success "All prerequisites met"
}

# Get list of unimplemented languages from issue
get_unimplemented_languages() {
    log_info "Fetching unimplemented languages from issue #${ISSUE_NUMBER}..."

    # Get issue body and extract languages
    gh issue view "$ISSUE_NUMBER" --repo "$REPO" --json body -q '.body' 2>/dev/null || {
        log_error "Failed to fetch issue #${ISSUE_NUMBER}"
        exit 1
    }
}

# Select language interactively or from argument
select_language() {
    local language="${1:-}"

    if [[ -n "$language" ]]; then
        echo "$language"
        return
    fi

    # Common unimplemented languages (update this list periodically)
    local languages=(
        "kotlin"
        "clojure"
        "cpp"
        "csharp"
        "php"
        "perl"
        "latex"
        "r"
        "julia"
        "fsharp"
        "groovy"
        "scss"
        "dockerfile"
        "powershell"
        "objective-c"
        "crystal"
        "gleam"
        "fennel"
        "janet"
        "odin"
    )

    echo ""
    log_info "Available languages to implement:"
    for i in "${!languages[@]}"; do
        echo "  $((i+1)). ${languages[$i]}"
    done
    echo ""

    read -p "Enter language name or number: " selection

    if [[ "$selection" =~ ^[0-9]+$ ]] && (( selection >= 1 && selection <= ${#languages[@]} )); then
        echo "${languages[$((selection-1))]}"
    else
        echo "$selection"
    fi
}

# Normalize language name for branch/package naming
normalize_name() {
    local name="$1"
    # Convert to lowercase, replace spaces with hyphens
    echo "$name" | tr '[:upper:]' '[:lower:]' | tr ' ' '-' | tr '_' '-'
}

# Create feature branch
create_branch() {
    local language="$1"
    local normalized_name
    normalized_name=$(normalize_name "$language")
    local branch_name="feat/${normalized_name}-mode"

    log_info "Creating branch: $branch_name"

    cd "$PROJECT_ROOT"

    # Ensure we're on main and up to date
    git checkout main >&2
    git pull origin main >&2

    # Create and switch to new branch
    git checkout -b "$branch_name" >&2

    echo "$branch_name"
}

# Generate implementation prompt for Claude Code
generate_prompt() {
    local language="$1"
    local normalized_name
    normalized_name=$(normalize_name "$language")

    cat << EOF
Implement a new language mode for ${language} in the Lem editor.

## Requirements

1. Create the extension in \`extensions/${normalized_name}-mode/\`
2. Follow the structure in \`docs/extension-development.md\`
3. Reference existing implementations like \`extensions/zig-mode/\` or \`extensions/toml-mode/\`

## Implementation Checklist

1. **System definition** (\`lem-${normalized_name}-mode.asd\`):
   - Depend on "lem/core" and "lem-lsp-mode" if LSP is available
   - Include all source files
   - **Include test system definition** (see below)

2. **Package definition**:
   - Use \`lem-${normalized_name}-mode\` as package name
   - Export mode symbol and hook variable

3. **Syntax table**:
   - Define appropriate syntax characters
   - Set up comment syntax (line and block if applicable)
   - Configure string delimiters

4. **TextMate grammar** (if complex syntax):
   - Keywords, built-ins, operators
   - String and comment highlighting
   - Type and function name highlighting

5. **Major mode definition**:
   - Set appropriate parent mode (usually \`language-mode\`)
   - Configure indent settings
   - Set line comment string

6. **File type association**:
   - Register all common file extensions

7. **LSP configuration** (if language server exists):
   - Define language spec with command and language-id
   - Add root-uri-patterns for project detection
   - Include install-command if applicable

8. **Register in lem.asd**:
   - Add to \`lem/extensions\` system

9. **Tests** (REQUIRED):
   Create \`tests/main.lisp\` with the following tests:
   - Test mode registration (find-mode-from-name returns the mode)
   - Test mode inheritance (subtypep from language-mode)
   - Test syntax highlighting enabled
   - Test line comment character setting

   Add test system to .asd file:
   \`\`\`lisp
   (defsystem "lem-${normalized_name}-mode/tests"
     :depends-on ("lem-${normalized_name}-mode" "rove")
     :components ((:module "tests"
                   :components ((:file "main"))))
     :perform (test-op (op c) (symbol-call :rove '#:run c)))
   \`\`\`

   Reference \`extensions/toml-mode/tests/main.lisp\` for test structure:
   \`\`\`lisp
   (defpackage :lem-${normalized_name}-mode/tests
     (:use :cl :rove :lem :lem-${normalized_name}-mode))
   (in-package :lem-${normalized_name}-mode/tests)

   (deftest test-mode-activates
     (testing "mode activates for file extensions"
       (ok (lem:find-mode-from-name "ModeName")
           "Mode should be registered")))

   (deftest test-mode-inherits-from-language-mode
     (testing "mode inherits from language-mode"
       (ok (subtypep 'mode-symbol 'lem/language-mode:language-mode)
           "Should inherit from language-mode")))
   \`\`\`

## Quality Checks

- Follow contract.yml coding standards
- Use :keyword style for loop macros
- Ensure proper package structure
- Test that mode activates correctly for file extensions
- **Verify tests pass**: \`make test\` or run specific tests

After implementation, run:
\`\`\`
make build
\`\`\`
to verify no compilation errors.

Then run tests:
\`\`\`
.qlot/bin/rove extensions/${normalized_name}-mode/lem-${normalized_name}-mode.asd
\`\`\`

Commit with message format: "feat(${normalized_name}-mode): add ${language} language mode with LSP support"
EOF
}

# Implement using Claude Code
implement_with_claude() {
    local language="$1"
    local prompt
    prompt=$(generate_prompt "$language")

    log_info "Invoking Claude Code to implement ${language} mode..."

    cd "$PROJECT_ROOT"

    # Run Claude Code with the implementation prompt
    # Using --print for non-interactive mode, --dangerously-skip-permissions for automation
    echo "$prompt" | claude --print --dangerously-skip-permissions

    log_success "Implementation completed"
}

# Run tests for the implemented language mode
run_tests() {
    local language="$1"
    local normalized_name
    normalized_name=$(normalize_name "$language")

    log_info "Running tests for ${language} mode..."

    cd "$PROJECT_ROOT"

    local test_asd="extensions/${normalized_name}-mode/lem-${normalized_name}-mode.asd"

    # Check if test file exists
    if [[ ! -f "$test_asd" ]]; then
        log_error "Test system file not found: $test_asd"
        return 1
    fi

    # Check if tests directory exists
    if [[ ! -d "extensions/${normalized_name}-mode/tests" ]]; then
        log_error "Tests directory not found: extensions/${normalized_name}-mode/tests/"
        return 1
    fi

    # Run tests using rove
    if .qlot/bin/rove "$test_asd"; then
        log_success "All tests passed"
        return 0
    else
        log_error "Some tests failed"
        return 1
    fi
}

# Create pull request
create_pr() {
    local language="$1"
    local branch_name="$2"
    local normalized_name
    normalized_name=$(normalize_name "$language")

    log_info "Creating pull request..."

    cd "$PROJECT_ROOT"

    # Stage and commit any remaining changes
    if ! git diff --quiet || ! git diff --cached --quiet; then
        git add -A
        git commit -m "feat(${normalized_name}-mode): add ${language} language mode with LSP support

Implements ${language} language support including:
- Syntax highlighting
- LSP integration (if available)
- File type associations
- Indentation settings

Closes #${ISSUE_NUMBER} (partial)

Generated with Claude Code" || true
    fi

    # Push branch
    git push -u origin "$branch_name"

    # Create PR
    local pr_url
    pr_url=$(gh pr create \
        --repo "$REPO" \
        --title "feat(${normalized_name}-mode): add ${language} language mode with LSP support" \
        --body "$(cat << EOF
## Summary

- Adds ${language} language mode to Lem editor
- Includes syntax highlighting and LSP integration
- Includes unit tests for mode functionality
- Part of #${ISSUE_NUMBER}

## Changes

- New extension: \`lem-${normalized_name}-mode\`
- Registered in \`lem/extensions\` system
- Test suite in \`extensions/${normalized_name}-mode/tests/\`

## Test Plan

- [ ] Mode activates for ${language} file extensions
- [ ] Syntax highlighting works correctly
- [ ] LSP connects (if language server installed)
- [ ] Build succeeds: \`make build\`
- [ ] Tests pass: \`.qlot/bin/rove extensions/${normalized_name}-mode/lem-${normalized_name}-mode.asd\`

---
Generated with [Claude Code](https://claude.com/claude-code)
EOF
)" \
        --head "$branch_name" \
        --base main)

    log_success "Pull request created: $pr_url"
    echo "$pr_url"
}

# Merge PR and sync
merge_and_sync() {
    local pr_url="$1"

    log_info "Waiting for CI checks..."

    # Extract PR number from URL
    local pr_number
    pr_number=$(echo "$pr_url" | grep -oE '[0-9]+$')

    # Wait for checks to complete (with timeout)
    local max_wait=300  # 5 minutes
    local waited=0
    local check_interval=30

    while (( waited < max_wait )); do
        local status
        status=$(gh pr checks "$pr_number" --repo "$REPO" 2>/dev/null | grep -E '(pass|fail|pending)' | head -1 || echo "pending")

        if [[ "$status" == *"fail"* ]]; then
            log_error "CI checks failed. Please fix manually."
            exit 1
        elif [[ "$status" != *"pending"* ]]; then
            log_success "CI checks passed"
            break
        fi

        log_info "Waiting for CI... (${waited}s/${max_wait}s)"
        sleep "$check_interval"
        (( waited += check_interval ))
    done

    if (( waited >= max_wait )); then
        log_warn "CI check timeout. Proceeding with merge (may require manual approval)..."
    fi

    # Merge PR
    log_info "Merging pull request..."
    gh pr merge "$pr_number" --repo "$REPO" --squash --delete-branch || {
        log_error "Failed to merge PR. It may require manual approval."
        exit 1
    }

    log_success "Pull request merged"

    # Sync local repository
    log_info "Syncing local repository..."
    cd "$PROJECT_ROOT"
    git checkout main
    git pull origin main

    log_success "Local repository synced"
}

# Main execution
main() {
    local language="${1:-}"

    echo ""
    echo "=========================================="
    echo "  Lem Language Mode Auto-Implementation  "
    echo "=========================================="
    echo ""

    check_prerequisites

    # Select language
    language=$(select_language "$language")
    log_info "Selected language: $language"

    # Confirm before proceeding
    echo ""
    read -p "Proceed with implementing ${language} mode? [y/N] " confirm
    if [[ ! "$confirm" =~ ^[Yy]$ ]]; then
        log_info "Aborted"
        exit 0
    fi

    # Step 1-2: Create branch
    local branch_name
    branch_name=$(create_branch "$language")

    # Step 3: Implement
    implement_with_claude "$language"

    # Step 4: Run tests
    log_info "Verifying implementation with tests..."
    if ! run_tests "$language"; then
        log_warn "Tests failed or missing. You may need to fix the implementation."
        echo ""
        read -p "Continue with PR creation anyway? [y/N] " continue_confirm
        if [[ ! "$continue_confirm" =~ ^[Yy]$ ]]; then
            log_info "Aborted. Fix the tests and commit manually, then create PR."
            log_info "To run tests: .qlot/bin/rove extensions/$(normalize_name "$language")-mode/lem-$(normalize_name "$language")-mode.asd"
            exit 1
        fi
    fi

    # Step 5: Create PR
    local pr_url
    pr_url=$(create_pr "$language" "$branch_name")

    # Step 6-8: Merge and sync
    echo ""
    read -p "Merge the PR automatically? [y/N] " merge_confirm
    if [[ "$merge_confirm" =~ ^[Yy]$ ]]; then
        merge_and_sync "$pr_url"
    else
        log_info "PR created but not merged. Merge manually when ready."
        log_info "After merging, run: git checkout main && git pull"
    fi

    echo ""
    log_success "=========================================="
    log_success "  Language mode implementation complete!  "
    log_success "=========================================="
    echo ""
}

# Run main with all arguments
main "$@"

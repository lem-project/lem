#!/bin/bash
# MIT-licensed packaging regression; see ../../LICENSE.
# Run with SBCL_HOME and LEM_REAL_SBCL set for Homebrew's real runtime on macOS.
set -eu
root="$(cd "$(dirname "$0")/../.." && pwd)"
probe_dir="$(mktemp -d)"
trap 'rm -rf "$probe_dir"' EXIT
export LEM_REAL_SBCL="${LEM_REAL_SBCL:-$(command -v sbcl)}"
export LEM_OPENSSL_LIBDIR="$probe_dir"
cat > "$probe_dir/probe.lisp" <<'LISP'
(assert (null (sb-posix:getenv "DYLD_LIBRARY_PATH")))
(assert (equal (last sb-ext:*posix-argv* 2) '("with spaces" "literal$value")))
(with-output-to-string (out)
  (sb-ext:run-program "/usr/bin/env" nil :output out :wait t)
  (assert (not (search "DYLD_LIBRARY_PATH=" (get-output-stream-string out)))))
LISP
# The first command has an empty runtime-options array (Bash 3.2 + nounset).
bash "$root/scripts/macos-sbcl.bash" --script "$probe_dir/probe.lisp" 'with spaces' 'literal$value'
bash "$root/scripts/macos-sbcl.bash" --dynamic-space-size 128 --noinform --script "$probe_dir/probe.lisp" 'with spaces' 'literal$value'

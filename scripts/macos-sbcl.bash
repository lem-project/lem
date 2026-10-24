#!/bin/bash
# Select OpenSSL for dyld at process startup, without leaking it to tools such
# as Homebrew git, which may require a different OpenSSL ABI.
# Part of the MIT-licensed Lem packaging scripts; see ../LICENSE.
set -eu
: "${LEM_OPENSSL_LIBDIR:?OpenSSL library directory is required}"
: "${LEM_REAL_SBCL:?The original SBCL binary is required}"
export DYLD_LIBRARY_PATH="$LEM_OPENSSL_LIBDIR${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"

# Keep SBCL runtime flags (e.g. --dynamic-space-size) before Lisp options.
runtime_options=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --eval|--load|--script|--end-runtime-options) break ;;
  esac
  runtime_options+=("$1")
  shift
done
# dyld captures its search path at startup. Remove the environment variable
# before Lisp launches subprocesses, while retaining dyld's path for dlopen.
exec "$LEM_REAL_SBCL" ${runtime_options[@]+"${runtime_options[@]}"} \
  --eval '(require :sb-posix)' \
  --eval '(sb-posix:unsetenv "DYLD_LIBRARY_PATH")' "$@"

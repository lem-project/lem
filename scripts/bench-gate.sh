#!/usr/bin/env bash
# Copyright (c) 2026 Yuri Flores Albuquerque.
# SPDX-License-Identifier: FSL-1.1-MIT
# Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
# Competing commercial use is strictly prohibited.
#
# T10 benchmark gate. Measures BASE_REF and the working tree on the same machine, inside
# the test image (docker/Dockerfile.test), and fails on a p95 regression over 20% or a
# p95 over its absolute budget. If BASE_REF predates the benchmark suite, only the
# budgets are enforced.
# Usage: scripts/bench-gate.sh BASE_REF [OUT_DIR]     (run from the repository root)
set -euo pipefail

base_ref="$1"
out_dir="$(realpath -m "${2:-bench-out}")"
mkdir -p "$out_dir"
base_tree="$(mktemp -d)/base"

cleanup() { git worktree remove --force "$base_tree" >/dev/null 2>&1 || true; }
trap cleanup EXIT

run_bench() { # image args...
  local image="$1"; shift
  docker run --rm -v "$out_dir:/out" --entrypoint bash "$image" \
    -c "sbcl --script scripts/run-yeditor-bench.lisp $*"
}

git worktree add --detach "$base_tree" "$base_ref" >/dev/null
if [ -f "$base_tree/scripts/run-yeditor-bench.lisp" ]; then
  echo "== Benchmarking base ($base_ref)"
  docker build -q -f docker/Dockerfile.test -t yeditor-bench-base "$base_tree" >/dev/null
  run_bench yeditor-bench-base /out/base.json >/dev/null || true
else
  echo "== Base $base_ref has no benchmark suite; enforcing budgets only"
  rm -f "$out_dir/base.json"
fi

echo "== Benchmarking head"
docker build -q -f docker/Dockerfile.test -t yeditor-bench-head . >/dev/null
set +e
run_bench yeditor-bench-head /out/head.json /out/base.json | tee "$out_dir/report.txt"
status=${PIPESTATUS[0]}
set -e
exit "$status"

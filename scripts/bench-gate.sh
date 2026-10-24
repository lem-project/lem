#!/usr/bin/env bash
# Copyright (c) 2026 Yuri Flores Albuquerque.
# SPDX-License-Identifier: FSL-1.1-MIT
# Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
# Competing commercial use is strictly prohibited.
# T10: one measuring harness, two source revisions, on the same host.
# Usage: scripts/bench-gate.sh BASE_REF [OUT_DIR], from the repository root.
set -euo pipefail

base_ref="${1:?BASE_REF is required}"
out_dir="$(realpath -m "${2:-bench-out}")"
mkdir -p "$out_dir"
rm -f "$out_dir/base.json" "$out_dir/head.json" "$out_dir/report.txt" \
      "$out_dir/base.log" "$out_dir/head.log"
printf 'base_ref=%s\nharness=head\n' "$base_ref" > "$out_dir/metadata.txt"
stage="prepare baseline"
tmp_root="$(mktemp -d)"
base_tree="$tmp_root/base"
mkdir -p "$base_tree"

cleanup() {
  local status=$?
  if [ "$status" -ne 0 ]; then
    printf 'FAIL: benchmark gate failed during %s (exit %s); see measurement logs.\n' \
      "$stage" "$status" >> "$out_dir/report.txt"
  fi
  rm -rf "$tmp_root"
  exit "$status"
}
trap cleanup EXIT

run_bench() {
  local image="$1"; shift
  docker run --rm -v "$out_dir:/out" --entrypoint sbcl "$image" \
    --script scripts/run-yeditor-bench.lisp "$@"
}

# An archive needs no worktree or Git metadata and leaves the developer checkout alone.
git archive "$base_ref" | tar -x -C "$base_tree"
base_args=()
if [ -f "$base_tree/scripts/run-yeditor-bench.lisp" ]; then
  # Measure the same workload on both revisions, including the first real-theme run.
  # Production code/dependencies stay at BASE_REF; only the measuring harness is shared.
  for file in yeditor/core/bench/bench.lisp yeditor/core/bench/yeditor-bench.asd \
              scripts/run-yeditor-bench.lisp; do
    cp "$file" "$base_tree/$file"
  done
  stage="build baseline"
  docker build -q -f docker/Dockerfile.test -t yeditor-bench-base "$base_tree" 2>&1 | tee -a "$out_dir/base.log"
  base_args=(/out/base.json)
else
  printf 'Base %s predates the suite; enforcing absolute budgets only.\n' "$base_ref"
fi

stage="build head"
docker build -q -f docker/Dockerfile.test -t yeditor-bench-head . 2>&1 | tee "$out_dir/head.log"
# Do not insert a CPU/I/O-heavy image build between the two measurements.
# Both revisions should see the host after the same build phase.
if [ "${#base_args[@]}" -gt 0 ]; then
  stage="measure baseline"
  run_bench yeditor-bench-base --measure-only /out/base.json 2>&1 | tee -a "$out_dir/base.log"
fi
stage="measure and compare head"
run_bench yeditor-bench-head /out/head.json "${base_args[@]}" 2>&1 \
  | tee -a "$out_dir/head.log" | tee "$out_dir/report.txt"

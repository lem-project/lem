#!/usr/bin/env bash
# Copyright (c) 2026 Yuri Flores Albuquerque.
# SPDX-License-Identifier: FSL-1.1-MIT
# Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
# Competing commercial use is strictly prohibited.
#
# License gate. Fails when:
#   1. a file under yeditor/enterprise/ lacks the LicenseRef-Yeditor-Enterprise header,
#      or any other file under yeditor/ (core, effects, components, ...) lacks FSL-1.1-MIT;
#   2. a file carrying a Yeditor SPDX identifier sits inside an MIT L0 directory;
#   3. (--deps) a loaded dependency is GPL/AGPL, or LGPL/LLGPL/MPL/unknown and not a
#      recorded exception in scripts/ip-license-exceptions.txt.
# Usage: scripts/check-license-headers.sh [--deps]   (run from the repository root)
set -euo pipefail

COPYRIGHT='Copyright (c) 2026 Yuri Flores Albuquerque'
FSL='SPDX-License-Identifier: FSL-1.1-MIT'
ENT='SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise'
L0_DIRS=(src frontends extensions contrib tests resources roswell docker)
status=0

echo "[1/3] License headers in yeditor/"
bad=0
while IFS= read -r f; do
  case "$f" in
    yeditor/enterprise/*) want="$ENT" ;;
    *)                    want="$FSL" ;;
  esac
  head_text=$(head -n 8 "$f")
  if ! grep -qF "$want" <<<"$head_text" || ! grep -qF "$COPYRIGHT" <<<"$head_text"; then
    echo "  $f: expected '$want'"
    bad=$((bad + 1))
  fi
done < <(git ls-files --cached --others --exclude-standard yeditor)
if [ "$bad" -gt 0 ]; then status=1; else echo "  OK"; fi

echo "[2/3] No Yeditor-licensed files inside MIT L0 directories"
leaked=$(git grep --untracked -lE -e 'SPDX-License-Identifier: (FSL-1\.1-MIT|LicenseRef-Yeditor-)' -- "${L0_DIRS[@]}" || true)
if [ -n "$leaked" ]; then
  echo "$leaked" | sed 's/^/  Yeditor file in L0: /'
  status=1
else
  echo "  OK"
fi

if [ "${1:-}" = "--deps" ]; then
  echo "[3/3] Dependency licenses"
  sbcl --script scripts/check-dependency-licenses.lisp || status=1
else
  echo "[3/3] Dependency licenses: skipped (pass --deps)"
fi

exit "$status"

#!/usr/bin/env bash
# Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
# SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
#
# This file is part of Yeditor Core IP (Layers L1-L5).
# Unauthorized copying, modification, or distribution of this file,
# via any medium, is strictly prohibited without a written license agreement.
#
# IP compliance gate. Fails when:
#   1. a tracked file under yeditor/ lacks the proprietary copyright + SPDX header;
#   2. a proprietary (Yeditor Core IP) file sits inside an MIT L0 directory;
#   3. (--deps) a loaded dependency has a GPL/AGPL license, or an LGPL/LLGPL/MPL/unknown
#      license that is not a recorded exception.
# Usage: scripts/check-ip-compliance.sh [--deps]   (run from the repository root)
set -euo pipefail

SPDX='SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary'
COPYRIGHT='Copyright (c) 2026 Yuri Flores Albuquerque'
L0_DIRS=(src frontends extensions contrib tests resources roswell docker)
status=0

echo "[1/3] Proprietary headers in yeditor/"
missing=0
while IFS= read -r f; do
  head_text=$(head -n 10 "$f")
  if ! grep -qF "$SPDX" <<<"$head_text" || ! grep -qF "$COPYRIGHT" <<<"$head_text"; then
    echo "  missing header: $f"
    missing=$((missing + 1))
  fi
done < <(git ls-files --cached --others --exclude-standard yeditor)
if [ "$missing" -gt 0 ]; then status=1; else echo "  OK"; fi

echo "[2/3] No Core IP files inside MIT L0 directories"
leaked=$(git grep --untracked -lF -e "$SPDX" -- "${L0_DIRS[@]}" || true)
if [ -n "$leaked" ]; then
  echo "$leaked" | sed 's/^/  proprietary file in L0: /'
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

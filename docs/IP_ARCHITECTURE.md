# Yeditor — IP Architecture

*Prepared for technical due diligence. Owner: Yuri Flores Albuquerque. Last updated: 2026-10-17.*

## 1. Summary

Yeditor is a code editor built as a fork of Lem (MIT). The repository holds two
legally distinct bodies of code, separated by directory:

| Layer | Location | Origin | License |
|---|---|---|---|
| **L0** — inherited substrate (buffers, windows, frontends, language modes) | `src/`, `frontends/`, `extensions/`, `contrib/` and other inherited files | Lem (lem-project/lem) | MIT — see [THIRD_PARTY_NOTICES.md](../THIRD_PARTY_NOTICES.md) |
| **L1–L5** — Core IP (transactional engine, component context, journal, SDK, agent host, forge, design system) | `yeditor/` | Original work of Yuri Flores Albuquerque | Proprietary — see [LICENSE](../LICENSE) |

## 2. Layer boundary rules

1. **All original work lives in `yeditor/`.** New L1–L5 code is never placed in L0 directories.
2. **L0 changes are minimal.** Edits to Lem-derived files are limited to thin hooks and
   adapters that L1–L5 need. Each such edit is a separate commit whose message names the
   L0 file and the reason, so the full L0 delta can be listed with `git log -- src frontends extensions contrib`.
3. **Dependency direction is one-way.** L1–L5 call into L0 through its public package
   interface (`lem:` exported symbols); L0 never depends on `yeditor/`. CI rejects `lem::`
   internal access.
4. **No third-party code is copied into `yeditor/`.** Third-party libraries are consumed as
   dependencies, never vendored into the Core IP tree.

## 3. Licensing headers

Every file in `yeditor/`, and every Yeditor-original script (`scripts/run-yeditor-*`,
`scripts/benchmark-tx.lisp`), begins with:

```lisp
;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.
```

ASDF systems under `yeditor/` declare `:license "LicenseRef-Yeditor-Proprietary"`.

## 4. Dependency license policy

**Allowed:** MIT, BSD (2- and 3-clause), Apache-2.0, Boost, zlib, ISC, Public Domain.

**Prohibited for new dependencies:** GPL, AGPL, LGPL and LLGPL (any version), and any
license that imposes source-disclosure or relicensing obligations on Yeditor code.
File-level copyleft (MPL-2.0, EPL) requires explicit approval by the owner.

### Known exceptions (open)

The inherited L0 dependency graph — not Yeditor code — currently loads libraries outside
this policy (full table in [THIRD_PARTY_NOTICES.md](../THIRD_PARTY_NOTICES.md) §3):

- **LLGPL:** the `trivia` pattern-matching family and its support libraries
  (`lisp-namespace`, `trivial-cltl2`, `trivial-types`, `type-i`).
- **MPL-2.0:** `cl-change-case`.
- **License not stated:** `cl-package-locks`.

These must be resolved (replaced, removed, or cleared by counsel for distribution as part
of a saved SBCL image) before a commercial release. Until then, the repository does
**not** meet the zero-copyleft policy, and should not be represented as meeting it.

## 5. Automated enforcement (CI)

`scripts/check-ip-compliance.sh` runs on every push and pull request (`.github/workflows/test.yml`)
and fails the build when:

1. a file under `yeditor/` lacks the proprietary copyright and SPDX header;
2. a file carrying the Yeditor SPDX identifier appears in an MIT L0 directory
   (`src/`, `frontends/`, `extensions/`, `contrib/`, `tests/`, `resources/`, `roswell/`, `docker/`);
3. any system loaded with Lem and Yeditor declares a GPL/AGPL license, or an LGPL/LLGPL/MPL/EPL
   or unrecognised license that is not listed in `scripts/ip-license-exceptions.txt`
   (`scripts/check-dependency-licenses.lisp`). GPL/AGPL cannot be excepted.

Systems that declare no license (mostly Lem's own modules, SBCL contribs and subsystems) are
listed in the report for manual review rather than failing the build.

## 6. Provenance and history

- Repository history is preserved; authorship of L1–L5 is visible per commit.
- Before 2026-10-17, the repository `LICENSE` stated that Yeditor additions were MIT
  licensed, and the repository was public. Copies of `yeditor/` obtained before the
  license change may remain available to their recipients under MIT. The proprietary
  license applies from the commit that introduced it.

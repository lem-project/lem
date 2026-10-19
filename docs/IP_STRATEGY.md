# Yeditor — IP Strategy

*Prepared for technical due diligence. Owner: Yuri Flores Albuquerque. Last updated: 2026-10-17.*

## 1. Model: open core

Yeditor is a fork of Lem (MIT). The repository holds three legally distinct bodies of code,
separated by directory and by the SPDX identifier in each file header:

| Tier | Layers | Location | License | SPDX |
|---|---|---|---|---|
| Inherited substrate | L0 | `src/`, `frontends/`, `extensions/`, `contrib/` and other Lem files | MIT ([THIRD_PARTY_NOTICES.md](../THIRD_PARTY_NOTICES.md)) | — |
| **Yeditor Core** — transactional engine, component context, standard effects, SDK, components | L1–L3 | `yeditor/core/`, `yeditor/effects/`, `yeditor/components/` | Functional Source License 1.1, MIT future ([LICENSE-FSL](../LICENSE-FSL)) | `FSL-1.1-MIT` |
| **Yeditor Enterprise** — journal, agent host & multiplexer, transactional branching, Eval-to-Journal, forge | L4–L5 | `yeditor/enterprise/` | Proprietary, All Rights Reserved ([LICENSE-ENTERPRISE](../LICENSE-ENTERPRISE)) | `LicenseRef-Yeditor-Enterprise` |

[LICENSE](../LICENSE) at the repository root maps each part to its license.

### What each tier allows

- **Core (FSL-1.1-MIT).** Anyone may read, audit, build, modify, redistribute and use the code
  for any *Permitted Purpose* — internal use, non-commercial education and research, writing
  components, professional services. A *Competing Use* (a commercial product or service that
  substitutes for Yeditor or offers substantially similar functionality) is not permitted.
  **Each released version becomes MIT two years after it is made available**; this grant is
  irrevocable. The long-term moat is therefore the Enterprise tier and the pace of Core
  development, not perpetual exclusivity over Core.
- **Enterprise (proprietary).** No use without a written commercial license.

## 2. Build separation (ASDF)

| System | File | License | Contents |
|---|---|---|---|
| `yeditor-core` | `yeditor/core/yeditor-core.asd` | FSL-1.1-MIT | umbrella for `yeditor-tx`, `yeditor-context` (+ reactivity, loader, durability), `yeditor-effects` |
| `yeditor-enterprise` | `yeditor/enterprise/yeditor-enterprise.asd` | Enterprise | depends on `yeditor-core`; L4–L5 code (T17–T25, T18, T30) goes here |
| `yeditor` | `yeditor.asd` | FSL-1.1-MIT | open-core build: `lem` + `yeditor-core` |

Dependency direction is one-way: Enterprise → Core → L0. Core never depends on Enterprise, so:

- the open-core build compiles and runs with `yeditor/enterprise/` absent;
- `yeditor/enterprise/` can move to a private Git submodule without changing Core;
- Enterprise can ship as a closed native image (`sb-ext:save-lisp-and-die` with
  `:executable t :compression t`) or be activated by a license key.

Note: as of 2026-10-17 `yeditor/enterprise/` contains only the system definition and a
placeholder; no L4–L5 code has been written yet.

## 3. Layer boundary rules

1. Yeditor code lives under `yeditor/`; it is never placed in L0 directories.
2. L0 changes are minimal — thin hooks and adapters — each in its own commit naming the file
   and reason, so `git log -- src frontends extensions contrib` lists the full L0 delta.
3. Yeditor calls L0 only through its exported interface (`lem:`); CI rejects `lem::`.
4. Third-party code is consumed as dependencies, never copied into `yeditor/`.

## 4. File headers

Core (`yeditor/` except `enterprise/`, plus `yeditor.asd` and Yeditor tooling in `scripts/`):

```lisp
;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.
```

Enterprise (`yeditor/enterprise/`):

```lisp
;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).
```

ASDF systems declare the matching `:license` (`"FSL-1.1-MIT"` or `"LicenseRef-Yeditor-Enterprise"`).

## 5. Dependency license policy

**Allowed:** MIT, BSD (2- and 3-clause), Apache-2.0, Boost, zlib, ISC, Public Domain.

**Prohibited:** GPL and AGPL (any version) — never, no exceptions. LGPL, LLGPL, MPL, EPL and
undeclared licenses only as a recorded exception in `scripts/ip-license-exceptions.txt`.

### Known exceptions (open)

The inherited L0 dependency graph — not Yeditor code — loads:

- **LLGPL:** the `trivia` family, `lisp-namespace`, `trivial-cltl2`, `trivial-types`, `type-i`;
- **MPL-2.0:** `cl-change-case`;
- **license not stated:** `cl-package-locks`.

These must be replaced, removed or cleared by counsel (in particular for distribution inside a
saved SBCL image) before a commercial Enterprise release. Until then the repository does
**not** meet the no-copyleft policy and should not be represented as meeting it.

## 6. Automated enforcement (CI)

`scripts/check-license-headers.sh` runs on every push and pull request
(`.github/workflows/test.yml`) and fails the build when:

1. a file in `yeditor/enterprise/` lacks the Enterprise header, or any other file in `yeditor/`
   lacks the FSL header (both also require the copyright line);
2. a file carrying a Yeditor SPDX identifier appears in an MIT L0 directory;
3. a system in the ASDF dependency graph of `lem`, `yeditor` and `yeditor-enterprise` (read from
   the system definitions, without loading code, so every frontend is covered) is GPL/AGPL, or has an
   LGPL/LLGPL/MPL/EPL/unrecognised license not recorded as an exception
   (`scripts/check-dependency-licenses.lisp`). Systems that declare no license are listed for
   manual review.

## 7. Contributions

External contributions require the [CLA](../CLA.md): contributors keep their copyright and grant
the owner a perpetual, irrevocable license with the unrestricted right to sublicense under any
terms, including proprietary ones, so community contributions never block relicensing, financing
or an acquisition.

## 8. Provenance and license history

- Authorship of all Yeditor code is visible per commit.
- Until 2026-10-17 the repository was public and its `LICENSE` stated that Yeditor additions were
  MIT licensed. Copies obtained before the change may remain available to their recipients under
  MIT. Each later license applies from the commit that introduced it:
  `6f86edf0` (proprietary) and the open-core restructuring that followed.

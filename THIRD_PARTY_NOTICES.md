# Third-Party Notices

Yeditor is proprietary software (see [LICENSE](LICENSE)). It is built on top of
third-party open-source software, whose notices are reproduced below as required
by their licenses. Nothing in the Yeditor proprietary license restricts any right
granted by these third-party licenses for the third-party code itself.

## 1. Lem (layer L0)

Yeditor is a fork of **Lem** — https://github.com/lem-project/lem — by cxxxr and
contributors, licensed under the MIT License.

Code derived from Lem lives in the inherited layer L0, including (non-exhaustive):
`src/`, `frontends/`, `extensions/`, `contrib/`, `tests/`, `resources/`,
`roswell/`, `docker/`, `scripts/` (except files named `run-yeditor-*` and
`benchmark-tx.lisp`), `lem.asd`, `lem-tests.asd`, and inherited documentation
under `docs/`. Yeditor modifications to these files remain subject to the MIT
notice below for the Lem-derived portions.

```
The MIT License (MIT)

Copyright (c) 2015 cxxxr

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## 2. Conceptual references (no code incorporated)

The Yeditor architecture is inspired by the paper *A Programming Paradigm for
Spatiotemporal Composability* (Shi, Zhang, Cui — arXiv:2608.25512) and the Cordis
framework (MIT). No Cordis source code is included in this repository.

## 3. Runtime library dependencies (Quicklisp, via qlot)

Audit of the systems loaded by `(asdf:load-system :lem)` on 2026-10-17, using the
`:license` declared by each system. The majority are MIT or BSD. Systems under any
other license:

| License | Systems | Status |
|---|---|---|
| **LLGPL** (Lisp Lesser GPL — weak copyleft) | trivia, trivia.balland2006, trivia.level0, trivia.level1, trivia.level2, trivia.trivial, lisp-namespace, trivial-cltl2, trivial-types, type-i | **Open issue** — pulled in by L0; conflicts with the no-copyleft policy in [docs/IP_ARCHITECTURE.md](docs/IP_ARCHITECTURE.md) |
| **MPL-2.0** (file-level copyleft) | cl-change-case | **Open issue** — same as above |
| Not stated (copyright line only) | cl-package-locks | **To verify** |
| zlib | deploy, dissect, documentation-utils, float-features, pathname-utils, trivial-indent, trivial-mimes, trivial-utf-8 | Permissive |
| Apache-2.0 | sha1 | Permissive |
| Public Domain / WTFPL | anaphora, md5, trivial-garbage, introspect-environment | Permissive |

Native libraries used by frontends (ncurses, SDL2, WebKit/webview, libvterm,
tree-sitter) are linked dynamically and must be re-audited per release build.

# Nightly packaging repairs (2026-10-22)

The scheduled builds on main failed in run 39698229699:

- Ubuntu 22.04 supplies libvterm 0.1, whose screen attributes lack conceal,
  small and baseline. Its static archive also lacks PIC, so merely guarding
  the fields does not fix a statically linked terminal.so.
- The macOS Qlot install failed with an undefined ASN1_STRING_data symbol
  while using the automatically selected SSL library.

The terminal accessor regression compiles/runs against Ubuntu 22.04's actual
headers and library, plus the current Homebrew library. Missing attributes return
zero; supported attributes retain their values. AppImage packaging builds pinned
libvterm 0.3.3 with PIC and pins tree-sitter to the wrapper's 0.27 ABI.

macos-deploy.bash resolves Homebrew OpenSSL 3 before installing dependencies.
A temporary SBCL wrapper sets the dynamic library search path immediately before
executing the real SBCL, after Qlot's SIP-protected shell interpreters. It forwards
arguments and is removed on exit. The existing bundling step packages the same
OpenSSL 3 libraries.

Nightly packaging also runs on pull requests that change packaging inputs. Both
artifacts must pass --help smoke tests. Pull requests and manual validation builds
cannot publish releases or move nightly-latest; manual publishing requires the
explicit publish input on main. A skipped build no longer reports built=true.

Local validation: original terminal compilation reproduced the missing-member
failure in Ubuntu 22.04; attribute regressions passed on the old and current
libvterm headers/libraries. The old archive's non-PIC failure was also reproduced.
Shell syntax, workflow expressions (actionlint), and whitespace checks passed.
Full AppImage and macOS packaging are validated by the linked PR's GitHub jobs;
a successful compiler alone is not evidence that either packaged app starts.

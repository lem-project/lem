#!/bin/sh
# Set OpenSSL's search path after macOS SIP-protected shell interpreters.
# Part of the MIT-licensed Lem packaging scripts; see ../LICENSE.
set -eu
: "${LEM_OPENSSL_LIBDIR:?OpenSSL library directory is required}"
: "${LEM_REAL_SBCL:?The original SBCL binary is required}"
export DYLD_LIBRARY_PATH="$LEM_OPENSSL_LIBDIR${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
exec "$LEM_REAL_SBCL" "$@"

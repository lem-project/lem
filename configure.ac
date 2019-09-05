AC_INIT([lem],[1.6])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_FILES([Makefile contrib/Makefile scripts/gen-contrib/Makefile])

AC_CHECK_PROG([ROSWELL], [ros], [yes])

if test "x$ROSWELL" != xyes; then
  AC_MSG_ERROR([roswell required]);
fi

dnl [TBD] cross compile possible?
lisp_impl=sbcl-bin
frontend=ncurses
if test `ros roswell-internal-use uname -m` == "armhf"; then
  if test `ros roswell-internal-use uname` == "linux"; then
	lisp_impl=ccl-bin
  fi
fi

AC_SUBST([LISP_IMPL],$lisp_impl)
ncurses=no
AC_CHECK_HEADERS(ncurses.h,[ncurses=yes])
if test "x$ncurses" = xno; then
  if test frontend=ncurses; then
    AC_MSG_ERROR([ncurses required]);
  fi
fi

AC_ARG_WITH([name_launcher],
  [AC_HELP_STRING([--with-name-launcher=lem],[name launcher for lem frontend])],
  [LEM_LAUNCHER=$withval],[LEM_LAUNCHER="lem"])
AC_SUBST([LEM_LAUNCHER])

AC_ARG_WITH([default_frontend],
  [AC_HELP_STRING([--with-default-frontend=ncurses],[default frontend.])],
  [DEFAULT_FRONTEND=$withval],[DEFAULT_FRONTEND="$frontend"])
AC_SUBST([DEFAULT_FRONTEND])

AC_ARG_ENABLE([ncurses],
    AS_HELP_STRING([--disable-ncurses], [Disable ncurses]))
AM_CONDITIONAL(GENERATE_NCURSES,[test "x$enable_ncurses" != "xno"])

AC_ARG_ENABLE([rpc],
    AS_HELP_STRING([--enable-rpc], [Enable rpc]))
AM_CONDITIONAL(GENERATE_RPC,[test "x$enable_rpc" = "xyes"])

AC_OUTPUT

AC_INIT([lem],[1.1])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_FILES([Makefile])

AC_CHECK_PROG([ROSWELL], [ros], [yes])

if test "x$ROSWELL" != xyes; then
  AC_MSG_ERROR([roswell required]);
fi

AC_ARG_WITH([name_launcher],
  [AC_HELP_STRING([--with-name-launcher=lem],[name launcher for lem frontend])],
  [LEM_LAUNCHER=$withval],[LEM_LAUNCHER="lem"])
AC_SUBST([LEM_LAUNCHER])

AC_ARG_WITH([lisp_impl],
  [AC_HELP_STRING([--with-lisp-impl=sbcl-bin],[impl to compile.])],
  [LISP_IMPL=$withval],[LISP_IMPL="sbcl-bin"])
AC_SUBST([LISP_IMPL])

AC_OUTPUT

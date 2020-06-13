AC_INIT([async-process], [0.1])
AM_CONFIG_HEADER(config.h)

AM_INIT_AUTOMAKE([foreign])
LT_INIT(shared win32-dll)
AC_PROG_CC


AC_CHECK_HEADERS([fcntl.h stdlib.h string.h unistd.h windows.h])

AC_CHECK_HEADER_STDBOOL
AC_TYPE_PID_T

AC_FUNC_FORK
AC_FUNC_MALLOC
AC_CHECK_FUNCS([dup2 strerror])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

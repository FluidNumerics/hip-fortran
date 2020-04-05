AC_INIT([hip-fortran],[0.0.0],[joe@fluidnumerics.com])
AM_INIT_AUTOMAKE(-Wall -Werror foreign)

AC_PREFIX_DEFAULT(/usr/local/hip-fortran)
# Set up build system to generate libhipfortran.a
AC_PROG_RANLIB
AM_PROG_AR

#AC_CONFIG_MACRO_DIRS([m4])

# Find Fortran compilers
AC_PROG_FC([gfortran pgfortran ifort])

AC_LANG_PUSH([Fortran])

AC_CONFIG_FILES([
Makefile
src/Makefile
])

AC_OUTPUT

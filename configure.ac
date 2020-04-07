AC_INIT([hip-fortran],[0.0.0],[joe@fluidnumerics.com])
AM_INIT_AUTOMAKE(-Wall -Werror foreign)

AC_PREFIX_DEFAULT(/opt/hip-fortran)
# Set up build system to generate libhipfortran.a
AC_PROG_RANLIB
AM_PROG_AR

# Find Fortran compilers
AC_PROG_FC([gfortran pgfortran ifort])

AC_LANG_PUSH([Fortran])

# Enable nvcc build for Nvidia systems
AC_ARG_ENABLE(nvcc, AS_HELP_STRING([--enable-nvcc, default:no]))
AS_IF([test "x$enable_nvcc" = "xyes"],[FCFLAGS+=" -DNVCC"],
      [FCFLAGS+=" -DHCC"])

AC_CONFIG_FILES([
Makefile
src/Makefile
])

AC_OUTPUT

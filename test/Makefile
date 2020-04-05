

memcpy_test:
	gfortran -I/usr/local/hip-fortran/include -c memcpy_test.f03
	hipcc -lgfortran memcpy_test.o 

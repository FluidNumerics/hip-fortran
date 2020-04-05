
FC=gfortran-8

memcpy_test:
	${FC} ${HIPFORTRAN_INCLUDE} -c memcpy_test.f03
	hipcc -lgfortran memcpy_test.o ${HIPFORTRAN_INCLUDE} ${HIPFORTRAN_LIB} -o $@  

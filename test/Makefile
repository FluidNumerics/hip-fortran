
FC=gfortran-8
FC_LIB=-L/usr/lib/gcc/x86_64-linux-gnu/8/ -lgfortran
CPP_LIB=-lstdc++


memcpy_test_hcc: memcpy_test.f03
	${FC} ${HIPFORTRAN_INCLUDE} -c memcpy_test.f03
	hipcc -amdgpu-target=gfx803 memcpy_test.o ${HIPFORTRAN_INCLUDE} ${HIPFORTRAN_LIB} ${FC_LIB} ${CPP_LIB} -o $@  

memcpy_test_nvcc: memcpy_test.f03
	${FC} ${HIPFORTRAN_INCLUDE} -c memcpy_test.f03
	hipcc  memcpy_test.o ${HIPFORTRAN_INCLUDE} ${HIPFORTRAN_LIB} ${FC_LIB} ${CPPLIB} -o $@  


#FC=gfortran-8
#CXX=hipcc
##CXXFLAGS=-amdgpu-target=gfx803
#LIB=-L/usr/lib/gcc/x86_64-linux-gnu/8 -lgfortran -lstdc++ -lm
#INCLUDE=-I/usr/include
#OPT_FLAGS=-O0 -g
#INSTALL_PATH=./

LINK=${INCLUDE} ${HIPFORTRAN_INCLUDE} ${LIB} ${HIPFORTRAN_LIB}

tests:
	make test_memcpy &&\
	make test_kernel_launch


test_memcpy: memcpy/memcpy_test.f03
	${FC} ${OPT_FLAGS} ${LINK} -c memcpy/memcpy_test.f03
	${CXX} ${OPT_FLAGS} ${CXXFLAGS} memcpy_test.o ${LINK}  -o $@  
	mv $@ ${INSTALL_PATH}

test_kernel_launch: kernel_launch/kernel_launch_test.f03
	${CXX} ${OPT_FLAGS} ${CXXFLAGS} ${LINK} -c kernel_launch/kernel_hip.cpp
	${FC} ${OPT_FLAGS} ${LINK} -c kernel_launch/kernel_module.f03
	${FC} ${OPT_FLAGS} ${LINK} -c kernel_launch/kernel_launch_test.f03
	${CXX} ${OPT_FLAGS} ${CXX_FLAGS} kernel_launch_test.o kernel_module.o kernel_hip.o ${LINK} -o $@  
	mv $@ ${INSTALL_PATH}

test_dtkernel_launch: derivedtype_kernel_launch/kernel_launch_test.f03
	${CXX} ${OPT_FLAGS} ${CXXFLAGS} ${LINK} -c derivedtype_kernel_launch/kernel_hip.cpp
	${FC} ${OPT_FLAGS} ${LINK} -c derivedtype_kernel_launch/kernel_module.f03
	${FC} ${OPT_FLAGS} ${LINK} -c derivedtype_kernel_launch/kernel_launch_test.f03
	${CXX} ${OPT_FLAGS} ${CXX_FLAGS} kernel_launch_test.o kernel_module.o kernel_hip.o ${LINK} -o $@  
	mv $@ ${INSTALL_PATH}

.PHONY : clean
clean :
	rm *.o *.mod

FROM rocm/dev-centos-7

RUN ./configure --prefix=/usr/local/hip-fortran &&\
    make -j &&\
    make -j install

ENV HIPFORTRAN_LIB=-L/usr/local/hip-fortran/lib -lhipfortran
ENV HIPFORTRAN_INCLUDE=-I/usr/local/hip-fortran/include

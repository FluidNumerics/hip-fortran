FROM rocm/dev-centos-7 as devel

# Install hip-fortran
RUN mkdir -p /build/hip-fortran
COPY . /build/hip-fortran

RUN cd /build/hip-fortran &&\
    /build/hip-fortran/configure --prefix=/usr/local/hip-fortran &&\
    make && make install

FROM rocm/dev-centos-7

COPY --from=devel /usr/local/hip-fortran /usr/local/hip-fortran
COPY --from=devel /build/hip-fortran/testsuite /usr/local/hip-fortran/testsuite
ENV HIPFORTRAN_LIB="-L/usr/local/hip-fortran/lib -lhipfortran"
ENV HIPFORTRAN_INCLUDE="-I/usr/local/hip-fortran/include"
ENV HIP_PLATFORM=hcc

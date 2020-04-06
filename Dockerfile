FROM rocm/dev-centos-7 as devel
ARG CONFIGURE_FLAGS=""

RUN mkdir -p /build/hip-fortran
COPY . /build/hip-fortran

RUN cd /build/hip-fortran &&\
    /build/hip-fortran/configurei ${CONFIGURE_FLAGS} --prefix=/usr/local/hip-fortran &&\
    make && make install


FROM rocm/dev-centos-7

COPY --from=devel /usr/local/hip-fortran /usr/local/hip-fortran
ENV HIPFORTRAN_LIB="-L/usr/local/hip-fortran/lib -lhipfortran"
ENV HIPFORTRAN_INCLUDE="-I/usr/local/hip-fortran/include"

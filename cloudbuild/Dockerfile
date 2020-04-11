FROM gcr.io/hip-fortran/hip-base as devel
ARG CONFIGURE_FLAGS=""
ARG PLATFORM=""

# Install hip-fortran
RUN mkdir -p /build/hip-fortran
COPY . /build/hip-fortran

RUN cd /build/hip-fortran &&\
    /build/hip-fortran/configure ${CONFIGURE_FLAGS} --prefix=/usr/local/hip-fortran &&\
    make && make install

FROM gcr.io/hip-fortran/hip-base

COPY --from=devel /usr/local/hip-fortran /usr/local/hip-fortran
COPY --from=devel /build/hip-fortran/testsuite /usr/local/hip-fortran/testsuite
ENV HIPFORTRAN_LIB="-L/usr/local/hip-fortran/lib -lhipfortran"
ENV HIPFORTRAN_INCLUDE="-I/usr/local/hip-fortran/include"
ENV HIP_PLATFORM=${PLATFORM}

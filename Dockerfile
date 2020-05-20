FROM rocm/dev-centos-7
ARG CONFIGURE_FLAGS=""
ARG PLATFORM=""

# Install CUDA-Toolkit (for nvcc platforms)
RUN yum-config-manager --add-repo http://developer.download.nvidia.com/compute/cuda/repos/rhel7/x86_64/cuda-rhel7.repo &&\
    yum clean all && yum -y install cuda

# Install hip-fortran
RUN mkdir -p /build/hip-fortran
COPY . /build/hip-fortran

RUN cd /build/hip-fortran &&\
    /build/hip-fortran/configure ${CONFIGURE_FLAGS} --prefix=/usr/local/hip-fortran &&\
    make && make install

FROM rocm/dev-centos-7

COPY --from=devel /usr/local/cuda /usr/local/cuda
COPY --from=devel /usr/local/hip-fortran /usr/local/hip-fortran
COPY --from=devel /build/hip-fortran/testsuite /usr/local/hip-fortran/testsuite
ENV HIPFORTRAN_LIB="-L/usr/local/hip-fortran/lib -lhipfortran"
ENV HIPFORTRAN_INCLUDE="-I/usr/local/hip-fortran/include"
ENV HIP_PLATFORM=${PLATFORM}

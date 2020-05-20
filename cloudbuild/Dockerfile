FROM gcr.io/hip-fortran/hip-base as devel

# Install CUDA-Toolkit (for nvcc platforms)
RUN yum-config-manager --add-repo http://developer.download.nvidia.com/compute/cuda/repos/rhel7/x86_64/cuda-rhel7.repo &&\
    yum clean all && yum -y install cuda

# Install hip-fortran
RUN mkdir -p /build/hip-fortran
COPY . /build/hip-fortran

RUN cd /build/hip-fortran &&\
    /build/hip-fortran/configure --enable-nvcc --prefix=/usr/local/hip-fortran/nvcc &&\
    make && make install && make distclean

RUN cd /build/hip-fortran &&\
    /build/hip-fortran/configure --prefix=/usr/local/hip-fortran/hcc &&\
    make && make install && make distclean

FROM gcr.io/hip-fortran/hip-base

COPY --from=devel /usr/local/cuda /usr/local/cuda
COPY --from=devel /usr/local/hip-fortran /usr/local/hip-fortran
COPY --from=devel /build/hip-fortran/testsuite /usr/local/hip-fortran/testsuite
ENV HIPFORTRAN_NVCC_LIB="-L/usr/local/hip-fortran/nvcc/lib -lhipfortran"
ENV HIPFORTRAN_NVCC_INCLUDE="-I/usr/local/hip-fortran/nvcc/include"
ENV HIPFORTRAN_HCC_LIB="-L/usr/local/hip-fortran/hcc/lib -lhipfortran"
ENV HIPFORTRAN_HCC_INCLUDE="-I/usr/local/hip-fortran/hcc/include"

FROM rocm/dev-centos-7 as devel
ARG CONFIGURE_FLAGS=""
ARG PLATFORM=""

# Install CUDA-Toolkit
RUN yum-config-manager --add-repo http://developer.download.nvidia.com/compute/cuda/repos/rhel7/x86_64/cuda-rhel7.repo &&\
    yum clean all && yum -y install cuda

FROM rocm/dev-centos-7
COPY --from=devel /usr/local/cuda /usr/local/cuda

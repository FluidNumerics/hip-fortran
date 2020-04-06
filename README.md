# hip-fortran
Copyright 2020 Fluid Numerics LLC

A Fortran layer for [AMD HIP](https://github.com/ROCm-Developer-Tools/HIP) to enable GPU acceleration on Nvidia and AMD GPUs.

## Installation
hip-fortran uses an autoconf build system. By default, hip-fortran is installed under `/opt/hip-fortran`.
```
./configure [--enable-nvcc] [--prefix=]
make
sudo make install
```
The `--enable-nvcc` flag is required for building hip-fortran applications on systems with Nvidia hardware. Note that you will also want to have the `nvcc` compiler installed to build your applications with hip-fortran in this situation.

An environment module file is included under `modulefiles/`. This modulefile assumes the default install path (`/opt/hip-fortran`) and defines `HIPFORTRAN_INCLUDE` and `HIPFORTRAN_LIB`.

## Docker
You can obtain the latest Docker image build of hip-fortran with
```
docker pull fluidnumerics/hip-fortran:latest-hcc # For latest hcc-enabled build
docker pull fluidnumerics/hip-fortran:latest-nvcc # For latest nvcc-enabled build
```
Inside each container image: 
* hip-fortran is installed under `/opt/hip-fortran`
* `HIPFORTRAN_INCLUDE` is set to `/opt/hip-fortran/include`
* `HIPFORTRAN_LIB` is set to `/opt/hip-fortran/lib`

This repository also contains a [Dockerfile](./Dockerfile) that you can use to build the image on other branches of this repository.
If you are working with Google Cloud Build, a [cloudbuild.yaml](./cloudbuild.yaml) file is also included.

## Usage
Example usage of hip-fortran can be found under [`test/`](test/).

In general, you'll want to
* Add the `USE hip_fortran` statement to your Fortran programs or modules
* Change host (CPU) copies of `ALLOCATABLE` arrays to `ALLOCATABLE,TARGET` arrays so that `c_loc` can be used in `hfMemcpy` calls.
* Write HIP Kernels and HIP Kernel Wrappers in C++

### Example
A simple example program is shown that demonstrates how to : 
* Create device (GPU) pointers
* Copy data from host to device and device to host
* How to launch a simple HIP kernel from Fortran
* How to build a hip-fortran application

In this example, we have three files
* `main.f03` : The main Fortran program
* `my_module.f03` : A Fortran module that defines the kernel interface
* `my_module_hip.cpp` : The C++ code that defines the HIP Kernel and the kernel wrapper to launch the kernel

Assuming you
* Have installed hip-fortran under `/opt/hip-fortran`,
* Are using the gfortran compiler,
* Are using the included modulefile,
* Have the hipcc compiler and all necessary dependencies,
 
If you are building on a Nvidia system,
```
export HIP_PLATFORM=nvcc
export CUDA_PATH=/path/to/cuda
```
The `CUDA_PATH` should be set to the parent directory for `bin/nvcc`. For example, if `which nvcc` returns `/opt/cuda/bin/nvcc`, then set `CUDA_PATH=/opt/cuda`.
You can build this application with
```
gfortran ${HIPFORTRAN_INCLUDE} -c my_module.f03
gfortran ${HIPFORTRAN_INCLUDE} -c main.f03
hipcc -c my_module_hip.cpp
hipcc -lgfortran main.o my_module.o my_module_hip.o ${HIPFORTRAN_INCLUDE} ${HIPFORTRAN_LIB} -o hip_test 
```


*main.f03*
```fortran
PROGRAM main

USE hip_fortran
USE my_module

IMPLICIT NONE

  REAL(8), ALLOCATABLE, TARGET :: a(:,:)
  REAL(8), ALLOCATABLE, TARGET :: b(:,:)
  TYPE(c_ptr) :: a_dev = c_null_ptr
  TYPE(c_ptr) :: b_dev = c_null_ptr

    ! Allocate and initialize host array
    ALLOCATE(array(0:10,0:10), b(0:10,0:10))
    array = 10.0D0
 
    ! Allocate device array
    CALL hfMalloc(a_dev, SIZEOF(a))
    CALL hfMalloc(b_dev, SIZEOF(b))

    ! Copy host memory to device memory
    CALL hfMemcpy(a_dev, c_loc(a), SIZEOF(a), hipMemcpyHostToDevice)

    CALL myRoutine(a_dev,b_dev,N) 

    CALL hfMemcpy(c_loc(b), b_dev, SIZEOF(b), hipMemcpyDeviceToHost)

    CALL hfFree(a_dev)
    CALL hfFree(b_dev)
    DEALLOCATE(a, b)

END PROGRAM main
```
*my_module.f03*
```fortran
MODULE my_module

IMPLICIT NONE

  INTERFACE
    SUBROUTINE myRoutine(a,b,N)
      USE iso_c_binding
      IMPLICIT NONE
      TYPE(c_ptr) :: a, b
      INTEGER, VALUE :: N
    END SUBROUTINE myRoutine(a,b,N)
  END INTERFACE

END MODULE my_module
```

*my_module_hip.cpp*
```c
#include <hip/hip_runtime.h>

__global__ void myroutine_hipkernel(double *a, double *b, int n){

  size_t i  = hipBlockIdx_x*hipBlockDim_x + hipThreadIdx_x;
  if ( i < (n+1)*(n+1) ) {
    b[i] = 2.0*a[i];
  }

}

extern "C"
{
  void myroutine(double **a, double **b, int n)
  {
    int threadPerBlock = 256;
    int blockCount = (n+1)*(n+1)/256; 

    hipLaunchKernelGGL((myroutine_hipkernel), dim3(blockCount), dim3(threadPerBlock), 0, 0, *a, *b, n);
  }
}
```

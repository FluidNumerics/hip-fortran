# hip-fortran
Copyright 2020 Fluid Numerics LLC

A Fortran layer for [AMD HIP](https://github.com/ROCm-Developer-Tools/HIP) to enable GPU acceleration on Nvidia and AMD GPUs.

## Installation
hip-fortran uses an autoconf build system. By default, hip-fortran is installed under `/opt/hip-fortran`.
```
./configure
make
sudo make install
```
An environment module file is included under `modulefiles/`. This modulefile assumes the default install path (`/opt/hip-fortran`) and defines `HIPFORTRAN_INCLUDE` and `HIPFORTRAN_LIB`.

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
    CALL hfMalloc(array_dev, SIZEOF(array))

    ! Copy host memory to device memory
    CALL hfMemcpy(a_dev, c_loc(a), SIZEOF(a), hipMemcpyHostToDevice)

    CALL myRoutine(array_dev,b_dev,N) 

    CALL hfMemcpy(c_loc(b), b_dev, SIZEOF(b), hipMemcpyDeviceToHost)

    CALL hfFree(a_dev)
    CALL hfFree(b_dev)
    DEALLOCATE(a, b)

END PROGRAM main
```
*my_module.f03*
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
*my_module.cpp*
```c
#include <hip/hip_runtime.h>

__global__ void myroutine_hipkernel(double *a, double *b, int n){

  size_t i  = blockIdx.x*blockDim.x + threadIdx.x;
  if ( i < (n+1)*(n+1) ) {
    b[i] = 2.0*a[i];
  }

}

extern "C"
{
  void myroutine(double **a, double **b, int n)
  {
    int threadPerBlock = 256;
    int blockCount = (n+1)*(n+1)/256 

    hipLaunchKernelGGL((myroutine_hipkernel), dim3(blockCount), dim3(threadPerBlock, 0, 0, *a, *b, n);
  }
}

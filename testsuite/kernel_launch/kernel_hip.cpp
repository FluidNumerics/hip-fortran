#include <hip/hip_runtime.h>

__global__ void hip_kernel(double *a, double *b, int n){

  size_t i  = hipBlockIdx_x*hipBlockDim_x + hipThreadIdx_x;
  if ( i < (n+1)*(n+1) ) {
    b[i] = 2.0*a[i];
  }

}

extern "C"
{
  void kernel_gpu(double **a, double **b, int n)
  {
    int threadPerBlock = 256;
    int blockCount = (n+1)*(n+1)/256; 

    hipLaunchKernelGGL((hip_kernel), dim3(blockCount), dim3(threadPerBlock), 0, 0, *a, *b, n);
  }
}

#include <hip/hip_runtime.h>

__global__ void hip_kernel(double *a, double *b, double *out, int n, int m){

  size_t k  = hipBlockIdx_x;
  size_t j  = hipThreadIdx_x;

  double reduce = 0.0;
  for (int i = 0; i < n; i++) {
    reduce += a[i+j*(n+1)]*b[i+k*(n+1)];
  }
  out[j+k*(n+1)] = reduce;

}

extern "C"
{
  void dosomething_wrapper(double **a, double **b, double **out, int n, int m)
  {
    int threadPerBlock = n+1;
    int blockCount = m; 

    hipLaunchKernelGGL((hip_kernel), dim3(blockCount), dim3(threadPerBlock), 0, 0, *a, *b, *out, n, m);
//    hipGetLastError(); 
  }
}

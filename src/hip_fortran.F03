MODULE hip_fortran

USE iso_c_binding
USE hip_enum

IMPLICIT NONE

  INTERFACE
#ifdef NVCC
    FUNCTION hipGetLastError() bind(c, name="cudaGetLastError")
#else
    FUNCTION hipGetLastError() bind(c, name="hipGetLastError")
#endif
    !> interface
    !  Fortran interface for [hipGetLastError](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Error.html#hipgetlasterror)
      USE iso_c_binding
      USE hip_enum
      IMPLICIT NONE
      INTEGER(KIND(hipSuccess)) :: hipGetLastError
    END FUNCTION hipGetLastError
  END INTERFACE

  INTERFACE
#ifdef NVCC
    FUNCTION hipPeekAtLastError() bind(c, name="cudaPeekAtLastError")
#else
    FUNCTION hipPeekAtLastError() bind(c, name="hipPeekAtLastError")
#endif
    !> interface
    !  Fortran interface for [hipPeekAtLastError](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Error.html#hippeakatlasterror)
      USE iso_c_binding
      USE hip_enum
      IMPLICIT NONE
      INTEGER(KIND(hipSuccess)) :: hipPeekAtLastError
    END FUNCTION hipPeekAtLastError
  END INTERFACE

  INTERFACE
#ifdef NVCC
    FUNCTION hipGetErrorName(hipError) bind(c, name="cudaGetErrorName")
#else
    FUNCTION hipGetErrorName(hipError) bind(c, name="hipGetErrorName")
#endif
    !> interface
    !  Fortran interface for [hipGetErrorName](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Error.html#hipgeterrorname)
      USE iso_c_binding
      USE hip_enum
      IMPLICIT NONE
      INTEGER(KIND(hipSuccess)) :: hipError
      CHARACTER(KIND=c_char) :: hipGetErrorName
    END FUNCTION hipGetErrorName
  END INTERFACE

  INTERFACE
#ifdef NVCC
    FUNCTION hipGetErrorString(hipError) bind(c, name="cudaGetErrorString")
#else
    FUNCTION hipGetErrorString(hipError) bind(c, name="hipGetErrorString")
#endif
    !> interface
    !  Fortran interface for [hipGetLastError](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Error.html#hipgeterrorstring)
      USE iso_c_binding
      USE hip_enum
      IMPLICIT NONE
      INTEGER(KIND(hipSuccess)) :: hipError
      CHARACTER(KIND=c_char) :: hipGetErrorString
    END FUNCTION hipGetErrorString
  END INTERFACE

  INTERFACE
#ifdef NVCC
    FUNCTION hipDeviceSynchronize() bind(c, name="cudaDeviceSynchronize")
#else
    FUNCTION hipDeviceSynchronize() bind(c, name="hipDeviceSynchronize")
#endif
    !> interface
    !  Fortran interface for [hipDeviceSynchronize](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Device-management.html#hipdevicesynchronize)
      USE iso_c_binding
      USE hip_enum
      IMPLICIT NONE
      INTEGER(KIND(hipSuccess)) :: hipDeviceSynchronize
    END FUNCTION hipDeviceSynchronize
  END INTERFACE

  INTERFACE
#ifdef NVCC
    FUNCTION hipMalloc(ptr,sizeBytes) bind(c, name="cudaMalloc")
#else
    FUNCTION hipMalloc(ptr,sizeBytes) bind(c, name="hipMalloc")
#endif
    !> Fortran interface for [hipMalloc](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Memory-Management.html#hipmalloc)
      USE iso_c_binding
      USE hip_enum
      IMPLICIT NONE
      INTEGER(KIND(hipSuccess)) :: hipMalloc
      TYPE(c_ptr) :: ptr
      INTEGER(c_size_t), VALUE :: sizeBytes
    END FUNCTION hipMalloc
  END INTERFACE

  INTERFACE
#ifdef NVCC
    FUNCTION hipFree(ptr) bind(c, name="cudaFree")
#else
    FUNCTION hipFree(ptr) bind(c, name="hipFree")
#endif
    !> Fortran interface for [hipFree](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Memory-Management.html#hipfree)
      USE iso_c_binding
      USE hip_enum
      IMPLICIT NONE
      INTEGER(KIND(hipSuccess)) :: hipFree
      TYPE(c_ptr), VALUE :: ptr
    END FUNCTION hipFree
  END INTERFACE

  INTERFACE
#ifdef NVCC
    FUNCTION hipMemcpy(dst,src,sizeBytes,cpyKind) bind(c, name="cudaMemcpy")
#else
    FUNCTION hipMemcpy(dst,src,sizeBytes,cpyKind) bind(c, name="hipMemcpy")
#endif
    !> Fortran interface for [hipMemcpy](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Memory-Management.html#hipmemcpy)
      USE iso_c_binding
      USE hip_enum
      IMPLICIT NONE
      INTEGER(KIND(hipSuccess)) :: hipMemcpy
      TYPE(c_ptr), VALUE :: dst
      TYPE(c_ptr), VALUE :: src
      INTEGER(c_size_t), VALUE :: sizeBytes
      INTEGER(KIND(hipMemcpyHostToHost)), VALUE :: cpyKind
    END FUNCTION hipMemcpy
  END INTERFACE

CONTAINS

  SUBROUTINE hipFortran(hipError)
  !> routine
  !  Main wrapper routine for calling hip routines with error checking 
    IMPLICIT NONE
    INTEGER(KIND(hipSuccess)), INTENT(in) :: hipError
      IF(hipError /= hipSuccess)THEN
        WRITE(*,*) "HIP ERROR:(",hipError,")", hipGetErrorName(hipError),":", hipGetErrorString(hipError)
        CALL EXIT(hipError)
      ENDIF
  END SUBROUTINE hipFortran

  SUBROUTINE hfDeviceSynchronize()
  !> routine
  !  Executes hipDeviceSynchronize with error checking
    IMPLICIT NONE

      CALL hipFortran(hipDeviceSynchronize())
   
  END SUBROUTINE hfDeviceSynchronize

  SUBROUTINE hfMalloc(ptr,sizeBytes)
  !> routine
  !  Executes hipMalloc with error checking
    IMPLICIT NONE
    TYPE(c_ptr) :: ptr
    INTEGER(c_size_t), VALUE, INTENT(in) :: sizeBytes

      CALL hipFortran(hipMalloc(ptr,sizeBytes))

  END SUBROUTINE hfMalloc

  SUBROUTINE hfFree(ptr)
  !> routine
  !  Executes hipFree with error checking
    IMPLICIT NONE
    TYPE(c_ptr), VALUE :: ptr

      CALL hipFortran(hipFree(ptr))

  END SUBROUTINE hfFree

END MODULE hip_fortran

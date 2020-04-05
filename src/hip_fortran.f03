MODULE hip_fortran

USE iso_c_binding
USE hip_enum

IMPLICIT NONE

  INTERFACE
    FUNCTION hipGetLastError() bind(c, name="hipGetLastError")
    !> interface
    !  Fortran interface for [hipGetLastError](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Error.html#hipgetlasterror)
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipGetLastError
    END FUNCTION hipGetLastError
  END INTERFACE

  INTERFACE
    FUNCTION hipPeekAtLastError() bind(c, name="hipPeekAtLastError")
    !> interface
    !  Fortran interface for [hipPeekAtLastError](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Error.html#hippeakatlasterror)
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipPeekAtLastError
    END FUNCTION hipPeekAtLastError
  END INTERFACE

  INTERFACE
    FUNCTION hipGetErrorName(hipError) bind(c, name="hipGetErrorName")
    !> interface
    !  Fortran interface for [hipGetErrorName](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Error.html#hipgeterrorname)
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipError
      CHARACTER(KIND=c_char) :: hipGetErrorName
    END FUNCTION hipGetErrorName
  END INTERFACE

  INTERFACE
    FUNCTION hipGetErrorString(hipError) bind(c, name="hipGetErrorString")
    !> interface
    !  Fortran interface for [hipGetLastError](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Error.html#hipgeterrorstring)
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipError
      CHARACTER(KIND=c_char) :: hipGetErrorString
    END FUNCTION hipGetErrorString
  END INTERFACE

  INTERFACE
    FUNCTION hipDeviceSynchronize() bind(c, name="hipDeviceSynchronize")
    !> interface
    !  Fortran interface for [hipDeviceSynchronize](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Device-management.html#hipdevicesynchronize)
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipDeviceSynchronize
    END FUNCTION hipDeviceSynchronize
  END INTERFACE

  INTERFACE
    FUNCTION hipMalloc(ptr,sizeBytes) bind(c, name="hipMalloc")
    !> Fortran interface for [hipMalloc](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Memory-Management.html#hipmalloc)
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipMalloc
      TYPE(c_ptr), VALUE :: ptr
      INTEGER(c_size_t), VALUE :: sizeBytes
    END FUNCTION hipMalloc
  END INTERFACE

  INTERFACE
    FUNCTION hipFree(ptr) bind(c, name="hipFree")
    !> Fortran interface for [hipFree](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Memory-Management.html#hipfree)
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipFree
      TYPE(c_ptr), VALUE :: ptr
    END FUNCTION hipFree
  END INTERFACE

  INTERFACE
    FUNCTION hipMemcpy(dst,src,sizeBytes,cpyKind) bind(c, name="hipMemcpy")
    !> Fortran interface for [hipMemcpy](https://rocm-documentation.readthedocs.io/en/latest/ROCm_API_References/HIP_API/Memory-Management.html#hipmemcpy)
      USE iso_c_binding
      USE hip_enum
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
        WRITE(*,*) "HIP ERROR:", hipGetErrorName(hipError),":", hipGetErrorString(hipError)
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
    TYPE(c_ptr), VALUE :: ptr
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

  SUBROUTINE hfMemcpy(dst,src,sizeBytes,cpyKind)
  !> routine
  !  Executes hipMemcpy with error checking
    IMPLICIT NONE
    INTEGER(KIND(hipSuccess)) :: hipMemcpy
    TYPE(c_ptr), VALUE :: dst
    TYPE(c_ptr), VALUE :: src
    INTEGER(c_size_t), VALUE :: sizeBytes
    INTEGER(KIND(hipMemcpyHostToHost)), VALUE :: cpyKind

      CALL hipFortran(hipMemcpy(dst,src,sizeBytes,cpyKind))

  END SUBROUTINE hfMemcpy

  
END MODULE hip_fortran

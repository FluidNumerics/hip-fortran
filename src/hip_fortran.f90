MODULE hip_fortran

IMPLICIT NONE



  INTERFACE
    FUNCTION hipGetErrorString(hipError) bind(c, name="hipGetErrorString")
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipError
      CHARACTER(KIND=c_char) :: hipGetErrorString
    END FUNCTION hipGetErrorString
  END INTERFACE

  INTERFACE
    FUNCTION hipDeviceSynchronize() bind(c, name="hipDeviceSynchronize")
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND(hipSuccess)) :: hipDeviceSynchronize
    END FUNCTION hipDeviceSynchronize
  END INTERFACE

  INTERFACE
    FUNCTION hipMemcpy(dst,src,sizeBytes,cpyKind) bind(c, name="hipMemcpy")
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
    USE hip_enum
    IMPLICIT NONE
    INTEGER(KIND(hipSuccess)) :: hipError
      IF(hipError /= hipSuccess)THEN
        WRITE(*,*) "HIP ERROR : ", hipGetErrorString(hipError)
        CALL EXIT(hipError)
      ENDIF
  END SUBROUTINE hipFortran
  
END MODULE hip_fortran

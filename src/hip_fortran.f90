MODULE hip_fortran

IMPLICIT NONE


  INTERFACE
    FUNCTION hipDeviceSynchronize() bind(c, name="hipDeviceSynchronize")
      USE iso_c_binding
      USE hip_enum
      INTEGER(KIND=hipSuccess) :: hipDeviceSynchronize
    END FUNCTION hipDeviceSynchronize
  END INTERFACE

END MODULE hip_fortran

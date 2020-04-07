MODULE kernel_module

IMPLICIT NONE

  ! Set generic interface for "kernel" that selects between
  ! "kernel_cpu" and "kernel_gpu" based on the type of input
  ! passed.
  INTERFACE kernel

    ! CPU execution is carried out when a,b are of TYPE REAL(8)
    SUBROUTINE kernel_cpu(a,b,N)
      IMPLICIT NONE
      REAL(8) :: a(0:N), b(0:N)
      INTEGER :: N
    END SUBROUTINE kernel_cpu

    ! GPU execution is carried out when a,b are of TYPE c_ptr
    SUBROUTINE kernel_gpu(a,b,N)
      USE iso_c_binding
      IMPLICIT NONE
      TYPE(c_ptr) :: a, b
      INTEGER, VALUE :: N
    END SUBROUTINE kernel_gpu

  END INTERFACE

CONTAINS

  SUBROUTINE kernel_cpu(a,b,N)
    IMPLICIT NONE
    REAL(8) :: a(0:N), b(0:N)
    INTEGER :: N
    ! Local
    INTEGER :: i

      DO i = 0, N
        b(i) = 2.0*a(i)
      ENDDO

  END SUBROUTINE kernel_cpu

END MODULE kernel_module

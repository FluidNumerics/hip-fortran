MODULE kernel_module

USE hip_fortran

IMPLICIT NONE



  TYPE myderivedtype
    INTEGER :: N
    REAL(8), POINTER :: a(:,:)
    TYPE(c_ptr) :: a_dev

    CONTAINS
    
    PROCEDURE :: Build
    PROCEDURE :: Trash
    PROCEDURE :: UpdateHost
    PROCEDURE :: UpdateDevice

    ! "Type-bound generic procedure" !
    GENERIC, PUBLIC :: DoSomething => DoSomething_cpu, DoSomething_gpu 
    PROCEDURE, PRIVATE :: DoSomething_cpu, DoSomething_gpu
    ! ////////////////////////////// !

  END TYPE myderivedtype


  INTERFACE 
    SUBROUTINE DoSomething_wrapper(a_dev,b_dev,out_dev,N,M) bind(c, name="dosomething_wrapper")
      USE iso_c_binding
      IMPLICIT NONE
      TYPE(c_ptr) :: a_dev, b_dev, out_dev
      INTEGER, VALUE :: N, M
    END SUBROUTINE DoSomething_wrapper
  END INTERFACE


CONTAINS

  SUBROUTINE Build(this, N)
    IMPLICIT NONE
    CLASS(myderivedtype) :: this
    INTEGER :: N

      this % N = N
      ALLOCATE(this % a(0:N,0:N))
      
      CALL hfMalloc(this % a_dev, SIZEOF(this % a))
      this % a = 1.0D0

  END SUBROUTINE Build

  SUBROUTINE Trash(this)
    IMPLICIT NONE
    CLASS(myderivedtype) :: this

      DEALLOCATE(this % a)
      CALL hfFree(this % a_dev)

  END SUBROUTINE Trash

  SUBROUTINE UpdateDevice(this)
    IMPLICIT NONE
    CLASS(myderivedtype) :: this

      ! Copy host memory to device memory
      CALL hipFortran(hipMemcpy(this % a_dev, c_loc(this % a), SIZEOF(this % a), hipMemcpyHostToDevice))

  END SUBROUTINE UpdateDevice

  SUBROUTINE UpdateHost(this)
    IMPLICIT NONE
    CLASS(myderivedtype) :: this

      ! Copy device memory to host memory
      CALL hipFortran(hipMemcpy(c_loc(this % a), this % a_dev, SIZEOF(this % a), hipMemcpyDeviceToHost))

  END SUBROUTINE UpdateHost

  SUBROUTINE DoSomething_cpu(this,b,output,M)
    IMPLICIT NONE
    CLASS(myderivedtype) :: this
    REAL(8) :: b(0:this % N,1:M), output(0:this % N,1:M)
    INTEGER :: M
    ! Local
    INTEGER :: i, j, k
    REAL(8) :: reduce

      DO k = 1, M
        DO j = 0, this % N
          output(j,k) = 0.0D0
          reduce = 0.0D0
          DO i = 0, this % N
            reduce = reduce + this % a(i,j)*b(i,k)
          ENDDO
          output(j,k) = reduce
        ENDDO
      ENDDO

  END SUBROUTINE DoSomething_cpu

  SUBROUTINE DoSomething_gpu(this,b,output,M)
    IMPLICIT NONE
    CLASS(myderivedtype) :: this
    TYPE(c_ptr) :: b, output
    INTEGER :: M
    ! Local
    INTEGER :: i, j, k

      CALL DoSomething_wrapper(this % a_dev, b, output, this % N, M)

  END SUBROUTINE DoSomething_gpu

END MODULE kernel_module

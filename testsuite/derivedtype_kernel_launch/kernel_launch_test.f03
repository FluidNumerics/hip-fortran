PROGRAM memcpy_test

USE kernel_module
USE hip_fortran

IMPLICIT NONE


  INTEGER, PARAMETER :: N=255
  INTEGER, PARAMETER :: M=10000
  INTEGER, PARAMETER :: nTests=100
  TYPE(myderivedtype) :: this
  REAL(8), ALLOCATABLE, TARGET :: output(:,:)
  REAL(8), ALLOCATABLE, TARGET :: b(:,:)
  TYPE(c_ptr) :: output_dev = c_null_ptr
  TYPE(c_ptr) :: b_dev = c_null_ptr
  INTEGER :: i

    ! Build the derived type
    CALL this % Build(N)

    ! Update device arrays in derived type
    CALL this % UpdateDevice()

    ! Allocate and initialize host a and b
    ALLOCATE(output(0:N,1:M))
    ALLOCATE(b(0:N,1:M))
    b = 1.0D0

    ! Allocate device a and b
    CALL hfMalloc(output_dev, SIZEOF(output))
    CALL hfMalloc(b_dev, SIZEOF(b))

    ! Copy host memory to device memory
    CALL hipFortran(hipMemcpy(b_dev, c_loc(b), SIZEOF(b), hipMemcpyHostToDevice))

    ! Call the CPU version of the kernel
!    CALL this % DoSomething(b, output, M)

    DO i = 1, nTests
      ! Call the GPU version of the kernel
      CALL this % DoSomething(b_dev, output_dev, M)
      CALL hipFortran(hipGetLastError())
    ENDDO

!    ! Copy device memory to host memory
!    CALL hipFortran(hipMemcpy(c_loc(b), b_dev, SIZEOF(a), hipMemcpyDeviceToHost))

    ! Clean up host and device memory
    CALL this % Trash()
    CALL hfFree(output_dev)
    CALL hfFree(b_dev)
    DEALLOCATE(output)
    DEALLOCATE(b)

END PROGRAM memcpy_test

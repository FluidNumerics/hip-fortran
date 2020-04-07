PROGRAM memcpy_test

USE hip_fortran

IMPLICIT NONE


  REAL(8), ALLOCATABLE, TARGET :: a(:,:)
  REAL(8), ALLOCATABLE, TARGET :: b(:,:)
  TYPE(c_ptr) :: a_dev = c_null_ptr
  TYPE(c_ptr) :: b_dev = c_null_ptr

    ! Allocate and initialize host a and b
    ALLOCATE(a(0:10,0:10))
    ALLOCATE(b(0:10,0:10))
    a = 10.0D0
    b = 0.0D0
 
    ! Allocate device a and b
    CALL hfMalloc(a_dev, SIZEOF(a))
    CALL hfMalloc(b_dev, SIZEOF(a))

    ! Copy host memory to device memory
    CALL hipFortran(hipMemcpy(a_dev, c_loc(a), SIZEOF(a), hipMemcpyHostToDevice))

    ! Call the GPU version of the kernel
    CALL kernel(a=a_dev, b=b_dev, N=10)

    ! Copy device memory to host memory
    CALL hipFortran(hipMemcpy(c_loc(b), b_dev, SIZEOF(a), hipMemcpyDeviceToHost))

    ! Clean up host and device memory
    CALL hfFree(a_dev)
    CALL hfFree(b_dev)
    DEALLOCATE(a)
    DEALLOCATE(b)

END PROGRAM memcpy_test

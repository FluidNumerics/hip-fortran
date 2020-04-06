PROGRAM memcpy_test

USE hip_fortran

IMPLICIT NONE


  REAL(8), ALLOCATABLE, TARGET :: a(:,:)
  TYPE(c_ptr) :: a_dev = c_null_ptr

    ! Allocate and initialize host a
    ALLOCATE(a(0:10,0:10))
    a = 10.0D0
 
    ! Allocate device a
    CALL hfMalloc(a_dev, SIZEOF(a))

    ! Copy host memory to device memory
    ! a_dev = a
    CALL hipFortran(hipMemcpy(a_dev, c_loc(a), SIZEOF(a), hipMemcpyHostToDevice))

    ! Clean up host and device memory
    CALL hfFree(a_dev)
    DEALLOCATE(a)

END PROGRAM memcpy_test

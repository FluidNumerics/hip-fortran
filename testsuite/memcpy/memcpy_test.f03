PROGRAM memcpy_test

USE hip_fortran

IMPLICIT NONE


  REAL(8), ALLOCATABLE, TARGET :: a(:,:)
  TYPE(c_ptr) :: a_dev = c_null_ptr
  INTEGER(c_size_t) :: a_size

    ! Allocate and initialize host a
    ALLOCATE(a(0:10,0:10))
    a = 10.0D0
    a_size = SIZEOF(a)
 
    ! Allocate device a
    CALL hfMalloc(a_dev, a_size)

    ! Copy host memory to device memory
    CALL hipFortran(hipMemcpy(a_dev, c_loc(a), SIZEOF(a), hipMemcpyHostToDevice))

    ! Copy device memory to host memory
    CALL hipFortran(hipMemcpy(c_loc(a), a_dev, SIZEOF(a), hipMemcpyDeviceToHost))

    ! Clean up host and device memory
    CALL hfFree(a_dev)
    DEALLOCATE(a)

END PROGRAM memcpy_test

PROGRAM memcpy_test

USE hip_fortran

IMPLICIT NONE


  REAL(8) :: array(:,:)
  TYPE(c_ptr) :: array_dev = c_null_ptr

    ! Allocate and initialize host array
    ALLOCATE(array(0:10,0:10))
    array = 10.0D0
 
    ! Allocate device array
    CALL hfMalloc(array_dev, SIZEOF(array))

    ! Copy host memory to device memory
    CALL hfMemcpy(array_dev, array, SIZEOF(array), hipMemcpyHostToDevice)

    DEALLOCATE(array)

END PROGRAM memcpy_test

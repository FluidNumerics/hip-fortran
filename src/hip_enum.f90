MODULE hip_enum

IMPLICIT NONE

! From HIP/include/hip/hip_runtime_api.h

  enum, bind(c) !hipMemcpyKind
    enumerator :: hipMemcpyHostToHost = 0
    enumerator :: hipMemcpyHostToDevice = 1
    enumerator :: hipMemcpyDeviceToHost = 2
    enumerator :: hipMemcpyDeviceToDevice = 3
    enumerator :: hipMemcpyDefault = 4
  end enum

  enum, bind(c) ! hipComputeMode 
    enumerator :: hipComputeModeDefault = 0
    enumerator :: hipComputeModeExclusive = 1
    enumerator :: hipComputeModeProhibited = 2
    enumerator :: hipComputeModeExclusiveProcess = 3
  end enum

  enum, bind(c) ! hipError_t
    enumerator :: hipSuccess = 0  !< Successful completion.
    enumerator :: hipErrorInvalidValue = 1  !< One or more of the parameters passed to the API call is NULL
                                             !< or not in an acceptable range.
    enumerator :: hipErrorOutOfMemory = 2
    ! Deprecated
    enumerator :: hipErrorMemoryAllocation = 2  !< Memory allocation error.
    enumerator :: hipErrorNotInitialized = 3
    ! Deprecated
    enumerator :: hipErrorInitializationError = 3
    enumerator :: hipErrorDeinitialized = 4
    enumerator :: hipErrorProfilerDisabled = 5
    enumerator :: hipErrorProfilerNotInitialized = 6
    enumerator :: hipErrorProfilerAlreadyStarted = 7
    enumerator :: hipErrorProfilerAlreadyStopped = 8
    enumerator :: hipErrorInvalidConfiguration = 9
    enumerator :: hipErrorInvalidSymbol = 13
    enumerator :: hipErrorInvalidDevicePointer = 17  !< Invalid Device Pointer
    enumerator :: hipErrorInvalidMemcpyDirection = 21  !< Invalid memory copy direction
    enumerator :: hipErrorInsufficientDriver = 35
    enumerator :: hipErrorMissingConfiguration = 52
    enumerator :: hipErrorPriorLaunchFailure = 53
    enumerator :: hipErrorInvalidDeviceFunction = 98
    enumerator :: hipErrorNoDevice = 100  !< Call to hipGetDeviceCount returned 0 devices
    enumerator :: hipErrorInvalidDevice = 101  !< DeviceID must be in range 0...#compute-devices.
    enumerator :: hipErrorInvalidImage = 200
    enumerator :: hipErrorInvalidContext = 201  !< Produced when input context is invalid.
    enumerator :: hipErrorContextAlreadyCurrent = 202
    enumerator :: hipErrorMapFailed = 205
    ! Deprecated
    enumerator :: hipErrorMapBufferObjectFailed = 205  !< Produced when the IPC memory attach failed from ROCr.
    enumerator :: hipErrorUnmapFailed = 206
    enumerator :: hipErrorArrayIsMapped = 207
    enumerator :: hipErrorAlreadyMapped = 208
    enumerator :: hipErrorNoBinaryForGpu = 209
    enumerator :: hipErrorAlreadyAcquired = 210
    enumerator :: hipErrorNotMapped = 211
    enumerator :: hipErrorNotMappedAsArray = 212
    enumerator :: hipErrorNotMappedAsPointer = 213
    enumerator :: hipErrorECCNotCorrectable = 214
    enumerator :: hipErrorUnsupportedLimit = 215
    enumerator :: hipErrorContextAlreadyInUse = 216
    enumerator :: hipErrorPeerAccessUnsupported = 217
    enumerator :: hipErrorInvalidKernelFile = 218  !< In CUDA DRV it is CUDA_ERROR_INVALID_PTX
    enumerator :: hipErrorInvalidGraphicsContext = 219
    enumerator :: hipErrorInvalidSource = 300
    enumerator :: hipErrorFileNotFound = 301
    enumerator :: hipErrorSharedObjectSymbolNotFound = 302
    enumerator :: hipErrorSharedObjectInitFailed = 303
    enumerator :: hipErrorOperatingSystem = 304
    enumerator :: hipErrorInvalidHandle = 400
    ! Deprecated
    enumerator :: hipErrorInvalidResourceHandle = 400  !< Resource handle (hipEvent_t or hipStream_t) invalid.
    enumerator :: hipErrorNotFound = 500
    enumerator :: hipErrorNotReady = 600  !< Indicates that asynchronous operations enqueued earlier are not
                                           !< ready.  This is not actually an error but is used to distinguish
                                           !< from hipSuccess (which indicates completion).  APIs that return
                                           !< this error include hipEventQuery and hipStreamQuery.
    enumerator :: hipErrorIllegalAddress = 700
    enumerator :: hipErrorLaunchOutOfResources = 701  !< Out of resources error.
    enumerator :: hipErrorLaunchTimeOut = 702
    enumerator :: hipErrorPeerAccessAlreadyEnabled = 704  !< Peer access was already enabled from the current device.
    enumerator :: hipErrorPeerAccessNotEnabled = 705  !< Peer access was never enabled from the current device.
    enumerator :: hipErrorSetOnActiveProcess = 708
    enumerator :: hipErrorAssert = 710  !< Produced when the kernel calls assert.
    enumerator :: hipErrorHostMemoryAlreadyRegistered = 712  !< Produced when trying to lock a page-locked memory.
    enumerator :: hipErrorHostMemoryNotRegistered = 713  !< Produced when trying to unlock a non-page-locked memory.
    enumerator :: hipErrorLaunchFailure = 719  !< An exception occurred on the device while executing a kernel.
    enumerator :: hipErrorCooperativeLaunchTooLarge = 720 !< This error indicates that the number of blocks launched per grid for a kernel
                                                           !< that was launched via cooperative launch APIs exceeds the maximum number of
                                                           !< allowed blocks for the current device
    enumerator :: hipErrorNotSupported = 801  !< Produced when the hip API is not supported/implemented
    enumerator :: hipErrorUnknown = 999  !< Unknown error.
    ! HSA Runtime Error Codes start here.
    enumerator :: hipErrorRuntimeMemory = 1052  !< HSA runtime memory call returned error.  Typically not seen
                                                 !< in production systems.
    enumerator :: hipErrorRuntimeOther = 1053  !< HSA runtime call other than memory returned error.  Typically
                                                !< not seen in production systems.
    enumerator :: hipErrorTbd  !< Marker that more error codes are needed.
  end enum


  enum, bind(c) ! hipMemoryType
    enumerator :: hipMemoryTypeHost    !< Memory is physically located on host
    enumerator :: hipMemoryTypeDevice  !< Memory is physically located on device. (see deviceId for specific
                                        !< device)
    enumerator :: hipMemoryTypeArray  !< Array memory physically located on device. (see deviceId for specific
                                       !< device)
    enumerator :: hipMemoryTypeUn
  end enum

  enum, bind(c) !hipChannelFormatKind
    enumerator :: hipChannelFormatKindSigned = 0
    enumerator :: hipChannelFormatKindUnsigned = 1
    enumerator :: hipChannelFormatKindFloat = 2
    enumerator :: hipChannelFormatKindNone = 3
  end enum


END MODULE hip_enum

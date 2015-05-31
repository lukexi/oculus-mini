{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Oculus.API where
import Foreign
import Foreign.C

import Linear

-- | Opaque newtypes for data we need to transport but not inspect
newtype HMD                = HMD (Ptr HMD)

newtype OVREyeRenderDesc   = OVREyeRenderDesc (Ptr OVREyeRenderDesc)

newtype HMDToEyeViewOffset = HMDToEyeViewOffset (Ptr HMDToEyeViewOffset)

newtype OVRPose            = OVRPose { unOVRPose :: (Ptr OVRPose) }
newtype OVRTexture         = OVRTexture (Ptr OVRTexture)

newtype OVRFOVPort         = OVRFOVPort (Ptr OVRFOVPort)

newtype FramebufferTextureID = FramebufferTextureID CUInt

foreign import ccall "free" 
    freePtr :: Ptr a -> IO ()

-- | Initialize and create the HMD reference
foreign import ccall "createHMD" 
    createHMD :: IO HMD

foreign import ccall "getHMDResolution" 
    getHMDResolution_raw :: HMD -> IO (Ptr CInt)
getHMDResolution :: Num a => HMD -> IO (a, a)
getHMDResolution hmd = do
    resolutionPtr <- getHMDResolution_raw hmd
    [resolutionW, resolutionH] <- peekArray 2 resolutionPtr
    freePtr resolutionPtr
    return (fromIntegral resolutionW, fromIntegral resolutionH)    

-- | Configures the HMD with sane defaults, and returns the HMDToEyeViewOffsets needed for getEyePoses
foreign import ccall "configureHMD" 
    configureHMD_raw :: HMD -> Ptr CChar -> IO OVREyeRenderDesc

configureHMD :: HMD -> String -> IO OVREyeRenderDesc
configureHMD hmd windowName = withCString windowName (configureHMD_raw hmd)

foreign import ccall "getEyeRenderDesc_HmdToEyeViewOffsets" 
    getEyeRenderDesc_HmdToEyeViewOffsets :: OVREyeRenderDesc -> IO HMDToEyeViewOffset

foreign import ccall "getEyeRenderDesc_FOV" 
    getEyeRenderDesc_FOV :: OVREyeRenderDesc -> CInt -> IO OVRFOVPort

-- | Returns the size of the texture/framebuffer/renderbuffer you should create to render into.
foreign import ccall "getHMDRenderTargetSize" 
    getHMDRenderTargetSize_raw :: HMD -> IO (Ptr CInt)

getHMDRenderTargetSize :: HMD -> IO (CInt, CInt)
getHMDRenderTargetSize hmd = do
    renderTargetSizePtr <- getHMDRenderTargetSize_raw hmd
    [renderTargetSizeW, renderTargetSizeH] <- peekArray 2 renderTargetSizePtr
    freePtr renderTargetSizePtr
    return (renderTargetSizeW, renderTargetSizeH)

-- | Takes the framebuffer texture object and its dimensions, and creates a texture descriptor to pass to ovrHmd_EndFrame
foreign import ccall "createOVRTextureArray" 
    createOVRTextureArray :: FramebufferTextureID -> CInt -> CInt -> IO OVRTexture

-- | Normal renderloop usage is to call beginFrame, render using poses from getEyePoses, then ovrHmd_EndFrame
foreign import ccall "beginFrame"
    beginFrame :: HMD -> IO ()

-- | Takes the HMDToEyeViewOffset returned from configureHMD, returns the eye poses to render with and pass to ovrHmd_EndFrame
foreign import ccall "getEyePoses" 
    getEyePoses :: HMD -> HMDToEyeViewOffset -> IO OVRPose

-- | Takes the eye poses from getEyePoses and the texture descriptors created by createOVRTextureArray
-- and blits what you rendered into the framebuffer to the Rift display.
foreign import ccall "ovrHmd_EndFrame" 
    ovrHmd_EndFrame :: HMD -> OVRPose -> OVRTexture -> IO ()

-- | Gets a projection matrix from ovrMatrix4f_Projection in the from of a row-major array of floats
foreign import ccall "getEyeProjection"
    getEyeProjection_raw :: OVRFOVPort -> CFloat -> CFloat -> IO (Ptr Float)


getEyeProjection :: OVRFOVPort -> Float -> Float -> IO (M44 Float)
getEyeProjection fovPort zNear zFar = do
    m44FromFlatMatrixPtr =<< getEyeProjection_raw fovPort (realToFrac zNear) (realToFrac zFar)

foreign import ccall "getOrthoSubProjection"
    getOrthoSubProjection_raw :: OVREyeRenderDesc -> CFloat -> CFloat -> HMDToEyeViewOffset -> CInt -> IO (Ptr Float)
getOrthoSubProjection :: OVREyeRenderDesc -> Float -> Float -> HMDToEyeViewOffset -> Int -> IO (M44 Float)
getOrthoSubProjection eyeRenderDescs zNear zFar hmdToEyeViewOffsets eyeIndex = do
    m44FromFlatMatrixPtr =<< getOrthoSubProjection_raw 
        eyeRenderDescs (realToFrac zNear) (realToFrac zFar) 
        hmdToEyeViewOffsets (fromIntegral eyeIndex)

m44FromFlatMatrixPtr :: Storable a => Ptr a -> IO (M44 a)
m44FromFlatMatrixPtr matrixPtr = do
    matrix <- m44FromList <$> peekArray 16 matrixPtr
    freePtr matrixPtr
    return matrix


m44FromList :: [a] -> M44 a
m44FromList [a,b,c,d
            ,e,f,g,h
            ,i,j,k,l
            ,m,n,o,p] = V4 (V4 a b c d)
                           (V4 e f g h)
                           (V4 i j k l)
                           (V4 m n o p)
m44FromList _ = error "Invalid list length for m44FromList"


foreign import ccall "getPoses_OrientationAndPositionForEye"
    getPoses_OrientationAndPositionForEye_raw :: OVRPose -> Int -> IO (Ptr Float)

getPoses_OrientationAndPositionForEye :: OVRPose -> Int -> IO (Quaternion Float, V3 Float)
getPoses_OrientationAndPositionForEye pose eyeIndex = do
    orientPosPtr <- getPoses_OrientationAndPositionForEye_raw pose eyeIndex
    [oX, oY, oZ, oW, pX, pY, pZ] <- peekArray 7 orientPosPtr
    freePtr orientPosPtr
    let orientation = Quaternion oW (V3 oX oY oZ)
        position    = V3 pX pY pZ
    return (orientation, position)



foreign import ccall "ovrHmd_DismissHSWDisplay" 
    dismissHSWDisplay :: HMD -> IO ()

foreign import ccall "ovrHmd_RecenterPose"
    recenterPose :: HMD -> IO ()
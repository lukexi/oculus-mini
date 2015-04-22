{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Oculus where
import Foreign
import Foreign.C
import Linear

-- | Opaque newtypes for data we need to transport but not inspect
newtype HMD                = HMD (Ptr HMD)

newtype OVREyeRenderDesc   = OVREyeRenderDesc (Ptr OVREyeRenderDesc)

newtype HMDToEyeViewOffset = HMDToEyeViewOffset (Ptr HMDToEyeViewOffset)

newtype OVRPose            = OVRPose (Ptr OVRPose)
newtype OVRTexture         = OVRTexture (Ptr OVRTexture)

newtype OVRFOVPort         = OVRFOVPort (Ptr OVRFOVPort)

newtype FramebufferTextureID = FramebufferTextureID CUInt

-- | Initialize and create the HMD reference
foreign import ccall "createHMD" 
    createHMD :: IO HMD

-- | Configures the HMD with sane defaults, and returns the HMDToEyeViewOffsets needed for getEyePoses
foreign import ccall "configureHMD" 
    configureHMD :: HMD -> IO OVREyeRenderDesc

foreign import ccall "getEyeRenderDesc_HmdToEyeViewOffsets" 
    getEyeRenderDesc_HmdToEyeViewOffsets :: OVREyeRenderDesc -> IO HMDToEyeViewOffset

foreign import ccall "getEyeRenderDesc_FOV" 
    getEyeRenderDesc_FOV :: OVREyeRenderDesc -> CInt -> IO OVRFOVPort

-- | Returns the size of the texture/framebuffer/renderbuffer you should create to render into.
foreign import ccall "getHMDRenderTargetSize" 
    getHMDRenderTargetSize :: HMD -> IO (Ptr CInt)

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

foreign import ccall "free" 
    freeEyePoses :: OVRPose -> IO ()

-- | Gets a projection matrix from ovrMatrix4f_Projection in the from of a row-major array of floats
foreign import ccall "getEyeProjection"
    getEyeProjection_raw :: OVRFOVPort -> CFloat -> CFloat -> IO (Ptr Float)

getEyeProjection :: OVRFOVPort -> Float -> Float -> IO (M44 Float)
getEyeProjection fovPort zNear zFar = do
    matrixPtr <- getEyeProjection_raw fovPort (realToFrac zNear) (realToFrac zFar)
    m44FromList <$> peekArray 16 matrixPtr

m44FromList :: [a] -> M44 a
m44FromList [a,b,c,d
            ,e,f,g,h
            ,i,j,k,l
            ,m,n,o,p] = V4 (V4 a b c d)
                           (V4 e f g h)
                           (V4 i j k l)
                           (V4 m n o p)


foreign import ccall "getPoses_OrientationAndPositionForEye"
    getPoses_OrientationAndPositionForEye_raw :: OVRPose -> Int -> IO (Ptr Float)

getPoses_OrientationAndPositionForEye pose eyeIndex = do
    [oX, oY, oZ, oW, pX, pY, pZ] <- peekArray 7 =<< getPoses_OrientationAndPositionForEye_raw pose eyeIndex
    let orientation = Quaternion oW (V3 oX oY oZ)
        position    = V3 pX pY pZ
    return (orientation, position)

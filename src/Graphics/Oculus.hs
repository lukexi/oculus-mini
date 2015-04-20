{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Oculus where
import Foreign
import Foreign.C

-- | Opaque newtypes for data we need to transport but not inspect
newtype HMD                = HMD (Ptr HMD)
newtype HMDToEyeViewOffset = HMDToEyeViewOffset (Ptr HMDToEyeViewOffset)
newtype OVRPose            = OVRPose (Ptr OVRPose)
newtype OVRTexture         = OVRTexture (Ptr OVRTexture)

newtype FramebufferTextureID = FramebufferTextureID CUInt

-- | Initialize and create the HMD reference
foreign import ccall "createHMD" 
    createHMD :: IO HMD

-- | Configures the HMD with sane defaults, and returns the HMDToEyeViewOffsets needed for getEyePoses
foreign import ccall "configureHMD" 
    configureHMD :: HMD -> IO HMDToEyeViewOffset

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
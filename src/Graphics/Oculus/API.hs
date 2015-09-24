{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module Graphics.Oculus.API where
import Foreign
import Foreign.C

import Linear
import Control.Monad.Trans

-- | Opaque newtypes for data we need to transport but not inspect
newtype HMDInfo                = HMDInfo (Ptr HMDInfo)
newtype LayerTextureIndex      = LayerTextureIndex Int deriving (Eq, Show, Ord)
newtype LayerTextureID         = LayerTextureID {unLayerTextureID :: Int}

-- We pass this in from Graphics.GL to avoid needing to
-- import GL headers in oculus.c (to avoid GLEW, etc.)
type GLsRGB8Alpha8 = Int

foreign import ccall "free" 
    freePtr :: Ptr a -> IO ()

-- | Initialize the Oculus hardware and create the HMDInfo reference
foreign import ccall "createHMDInfo" 
    createHMDInfo ::  GLsRGB8Alpha8 -> IO HMDInfo

-- | Call each of these at the beginning of each frame
-- to update the eye poses and find out which renderbuffer
-- to render into
foreign import ccall "calcEyePoses"
    calcEyePoses_raw :: HMDInfo -> IO ()

calcEyePoses :: MonadIO m => HMDInfo -> m ()
calcEyePoses = liftIO . calcEyePoses_raw

foreign import ccall "getFrameLayerTextureIndex"
    getFrameLayerTextureIndex_raw :: HMDInfo -> IO LayerTextureIndex

getFrameLayerTextureIndex :: MonadIO m => HMDInfo -> m LayerTextureIndex
getFrameLayerTextureIndex = liftIO . getFrameLayerTextureIndex_raw

-- Call at the end of the frame to send it to the HMD
foreign import ccall "submitFrame"
    submitFrame_raw :: HMDInfo -> IO ()
submitFrame :: MonadIO m => HMDInfo -> m ()
submitFrame = liftIO . submitFrame_raw

foreign import ccall "getLayerTextureCount"
    getLayerTextureCount :: HMDInfo -> IO Int

foreign import ccall "getLayerTextureIDAtIndex"
    getLayerTextureIDAtIndex :: HMDInfo -> LayerTextureIndex -> IO LayerTextureID

foreign import ccall "createMirrorTexture"
    createMirrorTexture :: HMDInfo -> GLsRGB8Alpha8 -> IO LayerTextureID

-- | Returns the size of the texture/framebuffer/renderbuffer you should create to render into.
foreign import ccall "getHMDBufferSize" 
    getHMDBufferSize_raw :: HMDInfo -> IO (Ptr CInt)

getHMDBufferSize :: Num t => HMDInfo -> IO (t, t)
getHMDBufferSize hmd = do
    [renderTargetSizeW, renderTargetSizeH] <- fmap fromIntegral <$> (peekArray 2 =<< getHMDBufferSize_raw hmd)
    return (renderTargetSizeW, renderTargetSizeH)

foreign import ccall "getLayerViewportForEye" 
    getLayerViewportForEye_raw :: HMDInfo -> Int -> IO (Ptr CInt)

getLayerViewportForEye :: Num t => HMDInfo -> Int -> IO (t, t, t, t)
getLayerViewportForEye hmd i = do
    [x,y,w,h] <- fmap fromIntegral <$> (fromMallocedArray 4 =<< getLayerViewportForEye_raw hmd i)
    return (x,y,w,h) 

-- | Gets a projection matrix from ovrMatrix4f_Projection in the from of a row-major array of floats
foreign import ccall "createProjectionForEye"
    createProjectionForEye_raw :: HMDInfo -> Float -> Float -> Int -> IO (Ptr Float)

getEyeProjection :: HMDInfo -> Float -> Float -> Int -> IO (M44 Float)
getEyeProjection hmdInfo zNear zFar eyeIndex = do
    m44FromFlatMatrixPtr =<< createProjectionForEye_raw hmdInfo
         (realToFrac zNear) (realToFrac zFar)
         (fromIntegral eyeIndex)

foreign import ccall "createOrthoSubProjectionForEye"
    createOrthoSubProjection_raw :: HMDInfo -> CFloat -> CFloat -> CInt -> IO (Ptr Float)

getOrthoSubProjection :: HMDInfo -> Float -> Float -> Int -> IO (M44 Float)
getOrthoSubProjection hmdInfo zNear zFar eyeIndex = 
    m44FromFlatMatrixPtr =<< createOrthoSubProjection_raw hmdInfo
        (realToFrac zNear) (realToFrac zFar) 
        (fromIntegral eyeIndex)

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


foreign import ccall "getOrientationAndPositionForEye"
    getOrientationAndPositionForEye_raw :: HMDInfo -> Int -> IO (Ptr Float)

getOrientationAndPositionForEye :: MonadIO m => HMDInfo -> Int -> m (Quaternion Float, V3 Float)
getOrientationAndPositionForEye pose eyeIndex = liftIO $
    orientationAndPositionFromMallocedArray $ 
        getOrientationAndPositionForEye_raw pose eyeIndex

foreign import ccall "getHMDPose"
    getHMDPose_raw :: HMDInfo -> IO (Ptr Float)

-- | Returns the last read position and orientation of the headset.
-- NOTE: You'll normally want to use getEyePoses to get orientations/positions for rendering, 
-- as it is meant to be used with beginFrame/endFrame.
-- This function is instead for times when it's useful to just generally query the position of the headset,
-- for example for calculating physics or networking the player's head position.
getHMDPose :: HMDInfo -> IO (Quaternion Float, V3 Float)
getHMDPose hmd = 
    orientationAndPositionFromMallocedArray $ getHMDPose_raw hmd

orientationAndPositionFromMallocedArray :: IO (Ptr Float) -> IO (Quaternion Float, V3 Float)
orientationAndPositionFromMallocedArray action = do
    [oX, oY, oZ, oW, pX, pY, pZ] <- fromMallocedArray 7 =<< action
    let orientation = Quaternion oW (V3 oX oY oZ)
        position    = V3 pX pY pZ
    return (orientation, position)

-- | Peeks into a malloc'd result array pointer and then frees the array pointer
fromMallocedArray :: Storable a => Int -> Ptr a -> IO [a]
fromMallocedArray len ptr = do
    results <- peekArray len ptr
    freePtr ptr
    return results

foreign import ccall "recenterPose"
    recenterPose_raw :: HMDInfo -> IO ()

data PerformanceHUDMode = PerfHUDModeOff
                        | PerfHUDModeLatency
                        | PerfHUDModeRender deriving (Eq, Show, Ord, Enum)

foreign import ccall "setPerformanceHUD"
    setPerformanceHUD_raw :: HMDInfo -> Int -> IO ()

{-# LANGUAGE RecordWildCards #-}
module Graphics.Oculus.Scaffold (RenderHMD, configureHMDRendering, renderHMDFrame, renderHMDEyes) where

import Graphics.Oculus.API
import Graphics.GL
import Linear

import Foreign
import Control.Monad
import Control.Monad.Trans
import Data.Maybe

overPtr :: (Storable a) => (Ptr a -> IO b) -> IO a
overPtr f = (alloca (\p -> f p >> peek p))

-- | A description of one eye
data Eye = Eye 
    { eyeIndex      :: Int
    , eyeProjection :: M44 GLfloat 
    , eyeViewport   :: (GLint, GLint, GLsizei, GLsizei)
    }

-- | The data necessary to render to the HMD
data RenderHMD = RenderHMD 
    { renHMD                   :: HMD
    , renEyeViewOffsets        :: HMDToEyeViewOffset 
    , renFrameBuffer           :: GLuint
    , renFrameBufferDescriptor :: OVRTexture
    , renEyes                  :: [Eye]
    }

renderHMDEyes :: MonadIO m => RenderHMD -> OVRPose -> (M44 GLfloat -> m b) -> m ()
renderHMDEyes renderHMD eyePoses action = forM_ (renEyes renderHMD) $ \eye -> do
    -- Get its orientation and position
    (eyeOrientation, eyePosition) <- liftIO $ getPoses_OrientationAndPositionForEye eyePoses (eyeIndex eye)

    let -- Convert the eye pose into a transformation matrix
        eyeTransform  = mkTransformation eyeOrientation eyePosition
        -- Invert eye transform to get correct head movement
        eyeTransformI = fromMaybe eyeTransform (inv44 eyeTransform)
        -- Get the perspective transform for this eye
        projection    = eyeProjection eye !*! eyeTransformI
        -- Get this eye's viewport to render into
        (x,y,w,h)     = eyeViewport eye
    glViewport x y w h

    action projection

-- | Call with an action to render a single frame to the HMD.
-- You'll be passed an OVRPose that you can then pass to renderHMDEyes to 
-- render to each eye after you've done any initial setup.
renderHMDFrame :: MonadIO m => RenderHMD -> (OVRPose -> m a) -> m a
renderHMDFrame RenderHMD{..} action = do
    -- Tell OVR API we're about to render a frame
    liftIO $ beginFrame renHMD

    -- Bind the eye texture as the frame buffer to render into
    glBindFramebuffer GL_FRAMEBUFFER renFrameBuffer

    -- Get the current orientation and position of the HMD
    eyePoses <- liftIO $ getEyePoses renHMD renEyeViewOffsets

    -- Call the render action with the eyePoses, which should then call renderHMDEyes
    result <- action eyePoses
    
    -- Unbind the eye texture
    glBindFramebuffer GL_FRAMEBUFFER 0

    -- Tell OVR API that we've finished our frame, passing the eyePoses and render texture so it can
    -- distort, timewarp, and blit to the screen.
    liftIO $ ovrHmd_EndFrame renHMD eyePoses renFrameBufferDescriptor
    -- Free the eye poses we allocated
    liftIO $ freePtr (unOVRPose eyePoses)

    return result

-- | Given the HMD and your window's name (needed for Direct Mode on Windows)
--  Configures the HMD and returns a struct of info needed to render frames to it
configureHMDRendering :: HMD -> String -> IO RenderHMD
configureHMDRendering hmd windowName = do
    -- Configure the HMD with sane defaults
    eyeRenderDescs <- configureHMD hmd windowName
    eyeViewOffsets <- getEyeRenderDesc_HmdToEyeViewOffsets eyeRenderDescs

    -- Find out how large our renderbuffer should be
    (renderTargetSizeW, renderTargetSizeH) <- getHMDRenderTargetSize hmd

    -- Create a framebuffer that we'll render into and pass to the Oculus SDK
    (frameBuffer, frameBufferTexture) <- createFrameBuffer (fromIntegral renderTargetSizeW) (fromIntegral renderTargetSizeH)

    -- Create the descriptors to tell the Oculus SDK about our framebuffer
    frameBufferDescriptor <- createOVRTextureArray (FramebufferTextureID (fromIntegral frameBufferTexture)) renderTargetSizeW renderTargetSizeH

    eyes <- makeEyes eyeRenderDescs renderTargetSizeW renderTargetSizeH

    return RenderHMD 
        { renHMD                   = hmd
        , renEyeViewOffsets        = eyeViewOffsets 
        , renFrameBuffer           = frameBuffer
        , renFrameBufferDescriptor = frameBufferDescriptor
        , renEyes                  = eyes
        }

-- | Package up descriptions of each eye's perspective projection and render viewport
makeEyes :: Integral a => OVREyeRenderDesc -> a -> a -> IO [Eye]
makeEyes eyeRenderDescs renderTargetSizeW renderTargetSizeH = do

    -- Get recommended FOV for each eye
    leftEyeFOV     <- getEyeRenderDesc_FOV eyeRenderDescs 0
    rightEyeFOV    <- getEyeRenderDesc_FOV eyeRenderDescs 1

    -- Get the perspective projection matrix for each eye
    leftEyeProjection  <- getEyeProjection leftEyeFOV  0.01 1000
    rightEyeProjection <- getEyeProjection rightEyeFOV 0.01 1000

    return  [ Eye { eyeIndex = 0
                  , eyeProjection = (fmap . fmap) realToFrac leftEyeProjection
                  , eyeViewport = 
                        (0, 0,
                         fromIntegral renderTargetSizeW `div` 2, fromIntegral renderTargetSizeH)
                    }
            , Eye { eyeIndex = 1
                  , eyeProjection = (fmap . fmap) realToFrac rightEyeProjection
                  , eyeViewport = 
                        (fromIntegral renderTargetSizeW `div` 2, 0,
                         fromIntegral renderTargetSizeW `div` 2, fromIntegral renderTargetSizeH)
                    }
            ]

-- | Create and configure the texture to use for our framebuffer
createFrameBufferTexture :: GLsizei -> GLsizei -> IO GLuint
createFrameBufferTexture sizeX sizeY = do
    texID <- overPtr (glGenTextures 1)
    
    glBindTexture   GL_TEXTURE_2D texID
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
    glTexStorage2D  GL_TEXTURE_2D 1 GL_RGBA8 sizeX sizeY
    glBindTexture   GL_TEXTURE_2D 0
    
    return texID

-- | Create the framebuffer we'll render into and pass to the Oculus SDK
createFrameBuffer :: GLsizei -> GLsizei -> IO (GLuint, GLuint)
createFrameBuffer sizeX sizeY = do
    frameBufferTexture <- createFrameBufferTexture sizeX sizeY

    frameBuffer <- overPtr (glGenFramebuffers 1)

    -- Attach the eye texture as the color buffer
    glBindFramebuffer GL_FRAMEBUFFER frameBuffer
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D frameBufferTexture 0

    -- Generate a render buffer for depth
    renderBuffer <- overPtr (glGenRenderbuffers 1)

    -- Configure the depth buffer dimensions to match the eye texture
    glBindRenderbuffer GL_RENDERBUFFER renderBuffer
    glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 sizeX sizeY
    glBindRenderbuffer GL_RENDERBUFFER 0

    -- Attach the render buffer as the depth target
    glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER renderBuffer

    -- Unbind the framebuffer
    glBindFramebuffer GL_FRAMEBUFFER 0

    return (frameBuffer, frameBufferTexture)

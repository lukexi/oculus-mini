{-# LANGUAGE RecordWildCards #-}
module Graphics.Oculus.Scaffold 
  ( HMD(..)
  , Eye(..)
  , createHMD 
  , renderHMDFrame 
  , renderHMDEyes
  , recenterPose
  , renderHMDMirror
  ) where

import Graphics.Oculus.API
import Graphics.GL
import Linear

import Foreign
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }

overPtr :: (Storable a) => (Ptr a -> IO b) -> IO a
overPtr f = (alloca (\p -> f p >> peek p))

-- | A description of one eye
data Eye = Eye 
  { eyeIndex      :: Int
  , eyeProjection :: M44 GLfloat 
  , eyeViewport   :: (GLint, GLint, GLsizei, GLsizei)
  }

-- | The data necessary to render to the HMD
data HMD = HMD 
  { hmdInfo                  :: HMDInfo
  , hmdFramebuffers          :: Map LayerTextureIndex Framebuffer
  , hmdEyes                  :: [Eye]
  , hmdMirrorFramebuffer     :: Framebuffer
  , hmdBufferSize            :: (GLint, GLint)
  }


-- | Package up descriptions of each eye's perspective projection and render viewport
createHMD :: IO HMD
createHMD = do

  hmdInfo_ <- createHMDInfo GL_SRGB8_ALPHA8
  (w,h) <- getHMDBufferSize hmdInfo_

  numTextures <- getLayerTextureCount hmdInfo_


  let createLayerFramebuffer framebuffers textureIndex = do
        let layerTextureIndex = LayerTextureIndex textureIndex
        texID       <- getLayerTextureIDAtIndex hmdInfo_ layerTextureIndex
        framebuffer <- createFrameBuffer w h (fromIntegral . unLayerTextureID $ texID) True
        return (Map.insert layerTextureIndex framebuffer framebuffers)
  framebuffers <- foldM createLayerFramebuffer mempty [0..numTextures]

  viewportLeft  <- getLayerViewportForEye hmdInfo_ 0
  viewportRight <- getLayerViewportForEye hmdInfo_ 1

  -- Get the perspective projection matrix for each eye
  eyeProjectionLeft  <- getEyeProjection hmdInfo_ 0.1 1000 0
  eyeProjectionRight <- getEyeProjection hmdInfo_ 0.1 1000 1

  let eyes = [ Eye { eyeIndex = 0
                   , eyeProjection = (fmap . fmap) realToFrac eyeProjectionLeft
                   , eyeViewport = viewportLeft
                   }
             , Eye { eyeIndex = 1
                   , eyeProjection = (fmap . fmap) realToFrac eyeProjectionRight
                   , eyeViewport = viewportRight
                   }
             ]

  -- Create a mirror framebuffer for mirroring into the GLFW window
  mirrorTexID <- createMirrorTexture hmdInfo_ GL_SRGB8_ALPHA8
  mirrorFramebuffer <- createFrameBuffer w h (fromIntegral . unLayerTextureID $ mirrorTexID) False
  
  return HMD
    { hmdInfo = hmdInfo_
    , hmdFramebuffers = framebuffers
    , hmdEyes = eyes
    , hmdMirrorFramebuffer = mirrorFramebuffer
    , hmdBufferSize = (w,h)
    }


getEyeViewsForFrame :: MonadIO m => HMD -> m [(Eye, M44 GLfloat)]
getEyeViewsForFrame HMD{..} = forM hmdEyes $ \eye -> do
  -- Get its orientation and position
  (eyeOrientation, eyePosition) <- getOrientationAndPositionForEye hmdInfo (eyeIndex eye)

  let -- Convert the eye pose into a transformation matrix
    eyeTransform  = mkTransformation eyeOrientation eyePosition
    -- Invert eye transform to get correct head movement
    eyeView       = fromMaybe eyeTransform (inv44 eyeTransform)
  return (eye, eyeView)

-- | It is usually more efficient to call this many times per frame
-- rather than once at the top, to save the cost of e.g. switching shaders
-- twice as often
renderHMDEyes :: MonadIO m => [(Eye, M44 GLfloat)] -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderHMDEyes eyesForFrame action = forM_ eyesForFrame $ \(eye, eyeView) -> do
  setViewportForEye eye
  action (eyeProjection eye) eyeView  

-- | Call with an action to render a single frame to the HMD.
-- You'll be passed an OVRPose that you can then pass to renderHMDEyes to 
-- render to each eye after you've done any initial setup.
renderHMDFrame :: MonadIO m => HMD -> ([(Eye, M44 GLfloat)] -> m a) -> m a
renderHMDFrame hmd@HMD{..} action = do

  -- Tell OVR API we're about to render a frame
  calcEyePoses hmdInfo
  -- Find which framebuffer we should render into
  layerTextureIndex <- getFrameLayerTextureIndex hmdInfo
  let framebuffer = fromMaybe (error "No framebuffer found for LayerTextureIndex") 
                      $ Map.lookup layerTextureIndex hmdFramebuffers


  -- Bind the eye texture as the frame buffer to render into
  glBindFramebuffer GL_FRAMEBUFFER (unFramebuffer framebuffer)

  -- Get the current orientation and position of the HMD
  eyeViews <- getEyeViewsForFrame hmd

  -- Call the render action with the eyeViews
  result <- action eyeViews
  
  -- Unbind the eye texture
  glBindFramebuffer GL_FRAMEBUFFER 0

  -- Tell OVR API that we've finished our frame, passing the eyePoses and render texture so it can
  -- distort, timewarp, and blit to the screen.
  submitFrame hmdInfo

  return result

-- | Blits the Mirroring framebuffer to the default framebuffer
-- Be sure to call your windowing API's swap function
-- after calling this (e.g. GLFW.swapBuffers)
renderHMDMirror :: MonadIO m => HMD -> m ()
renderHMDMirror HMD{..} = do
  -- Render into the mirror framebuffer
  glBindFramebuffer GL_READ_FRAMEBUFFER (unFramebuffer hmdMirrorFramebuffer)
  let (w,h) = hmdBufferSize
  glBlitFramebuffer 
    0 0 w h
    0 h w 0
    GL_COLOR_BUFFER_BIT GL_NEAREST 
  glBindFramebuffer GL_FRAMEBUFFER 0

-- | Create the framebuffer we'll render into and pass to the Oculus SDK
createFrameBuffer :: GLsizei -> GLsizei -> GLuint -> Bool -> IO Framebuffer
createFrameBuffer sizeX sizeY frameBufferTexture giveDepthBuffer = do

  frameBuffer <- overPtr (glGenFramebuffers 1)

  -- Attach the eye texture as the color buffer
  glBindFramebuffer GL_FRAMEBUFFER frameBuffer
  glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D frameBufferTexture 0

  when giveDepthBuffer $ do
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

  return $ Framebuffer frameBuffer

recenterPose :: MonadIO m => HMD -> m ()
recenterPose = liftIO . recenterPose_raw . hmdInfo

setViewportForEye :: MonadIO m => Eye -> m ()
setViewportForEye eye = glViewport x y w h
  where (x,y,w,h) = eyeViewport eye
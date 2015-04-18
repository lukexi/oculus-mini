import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Bindings.OculusRift
import Bindings.OculusRift.Types hiding (trackingCaps, distortionCaps, hmdCaps, texID)
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Foreign

overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f =
  liftIO (alloca (\p ->
                    do _ <- f p
                       peek p))

main :: IO ()
main = do

    _ <- ovr_Initialize

    hmd     <- ovrHmd_CreateDebug ovrHmd_DK2
    hmdDesc <- castToOvrHmdDesc hmd
    let hmdRes = resolution hmdDesc

    win <- setupGLFW (si_w hmdRes) (si_h hmdRes)

    (fbo, eyeTexture, eyeViewOffsets) <- setupOculus hmd

    glClearColor 0 1 1 1

    mainLoop win hmd fbo eyeTexture eyeViewOffsets 0
    return ()

setupGLFW :: Int -> Int -> IO GLFW.Window
setupGLFW desiredW desiredH = do
    _ <- GLFW.init

    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2

    let (halfW, halfH) = (desiredW `div` 2, desiredH `div` 2)
    Just win <- GLFW.createWindow desiredW desiredH "CrashTest" Nothing Nothing
    (frameW, frameH) <- GLFW.getFramebufferSize win
    -- Compensate for retina framebuffers on Mac
    when (frameW > desiredW && frameH > desiredH) $ GLFW.setWindowSize win halfW halfH

    GLFW.makeContextCurrent (Just win)

    GLFW.swapInterval 1
    return win

mainLoop :: GLFW.Window -> OvrHmd -> GLuint -> [OvrTexture] -> [OvrVector3f] -> Word32 -> IO a
mainLoop win hmd fbo eyeTexture eyeViewOffsets frameNo = do

    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Tell OVR API we're about to render a frame
    ovrHmd_BeginFrame hmd frameNo

    -- Bind the eye texture as the frame buffer to render into
    glBindFramebuffer GL_FRAMEBUFFER fbo

    -- Get the current orientation and position of the HMD
    eyePoses <- ovrHmd_GetEyePoses hmd frameNo eyeViewOffsets

    -- Normally we'd render something here beyond just clearing the screen to a color
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    
    -- Unbind the eye texture
    glBindFramebuffer GL_FRAMEBUFFER 0

    -- Tell OVR API that we've finished our frame, passing the eyePoses and render texture so it can
    -- distort, timewarp, and blit to the screen.
    ovrHmd_EndFrame hmd eyePoses eyeTexture

    mainLoop win hmd fbo eyeTexture eyeViewOffsets (succ frameNo)


setupOculus :: OvrHmd -> IO (GLuint, [OvrTexture], [OvrVector3f])
setupOculus hmd = do
  hmdDesc <- castToOvrHmdDesc hmd
  
  let trackingCaps =  ovrTrackingCap_Orientation  
                  .|. ovrTrackingCap_MagYawCorrection
                  .|. ovrTrackingCap_Position
  _ <- ovrHmd_ConfigureTracking hmd trackingCaps ovrTrackingCap_None
  
  (eyeTextures, fbo) <- genTextureAndFramebuffer hmd

  let configHeader  = OvrRenderAPIConfigHeader
                        ovrRenderAPI_OpenGL
                        (resolution hmdDesc) 
                        0 --  1 <- multisampling on/off
      apiConfig     = OvrRenderAPIConfig configHeader Nothing Nothing
      distortionCaps = ovrDistortionCap_TimeWarp
                   .|. ovrDistortionCap_Vignette
                   .|. ovrDistortionCap_Overdrive 
                   .|. ovrDistortionCap_HqDistortion
  
  lfv <- ovrHmd_GetDefaultFov hmd ovrEye_Left
  rfv <- ovrHmd_GetDefaultFov hmd ovrEye_Right
  (_, eyeRD) <- ovrHmd_ConfigureRendering hmd (Just apiConfig) distortionCaps [lfv,rfv]
  
  let hmdCaps = ovrHmdCap_ExtendDesktop 
             .|. ovrHmdCap_LowPersistence
             .|. ovrHmdCap_DynamicPrediction
  ovrHmd_SetEnabledCaps hmd hmdCaps

  ovrHmd_RecenterPose hmd

  return (fbo, eyeTextures, map hmdToEyeViewOffset eyeRD)

genTextureAndFramebuffer :: OvrHmd -> IO ([OvrTexture], GLuint)
genTextureAndFramebuffer hmd = do
  recommenedTex0Size <- ovrHmd_GetDefaultFovTextureSize hmd ovrEye_Left 1.0
  recommenedTex1Size <- ovrHmd_GetDefaultFovTextureSize hmd ovrEye_Right 1.0
  let renderTargetSizeW = si_w recommenedTex0Size
                        + si_w recommenedTex1Size
      renderTargetSizeH = si_h recommenedTex0Size `max` 
                          si_h recommenedTex1Size
      renderTargetSizeWI = fromIntegral renderTargetSizeW
      renderTargetSizeHI = fromIntegral renderTargetSizeH
  eyeTextureObject <- genColorTexture renderTargetSizeWI renderTargetSizeHI
  fbo              <- genColorFrameBuffer eyeTextureObject renderTargetSizeWI renderTargetSizeHI
  --
  let eyeTextures = genEyeTextureData eyeTextureObject renderTargetSizeW renderTargetSizeH
  return (eyeTextures, fbo)

genColorTexture :: GLsizei -> GLsizei -> IO GLuint
genColorTexture sizeX sizeY = do
    texID <- overPtr (glGenTextures 1)
    
    glBindTexture   GL_TEXTURE_2D texID
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
    glTexStorage2D  GL_TEXTURE_2D 1 GL_RGBA8 sizeX sizeY
    glBindTexture   GL_TEXTURE_2D 0
    
    return texID

genColorFrameBuffer :: GLuint -> GLsizei -> GLsizei -> IO GLuint
genColorFrameBuffer eyeTextureObject sizeX sizeY = do
    fbo <- overPtr (glGenFramebuffers 1)

    -- Attach the eye texture as the color buffer
    glBindFramebuffer GL_FRAMEBUFFER fbo
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D eyeTextureObject 0

    -- Generate a render buffer for depth
    rbo <- overPtr (glGenRenderbuffers 1)

    -- Configure the depth buffer dimensions to match the eye texture
    glBindRenderbuffer GL_RENDERBUFFER rbo
    glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 sizeX sizeY
    glBindRenderbuffer GL_RENDERBUFFER 0

    -- Attach the render buffer as the depth target
    glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER rbo

    -- Unbind the framebuffer
    glBindFramebuffer GL_FRAMEBUFFER 0

    return fbo

genEyeTextureData :: GLuint -> Int -> Int -> [OvrTexture]
genEyeTextureData textureID width height = 
  [ OvrTexture hd0 (fromIntegral textureID) , OvrTexture hd1 (fromIntegral textureID) ]
  where
    vpSize = OvrSizei (div width 2) height
    hd0 = OvrTextureHeader
             { apiT = ovrRenderAPI_OpenGL
             , textureSize = OvrSizei width height
             , renderViewport = OvrRecti (OvrVector2i 0 0) vpSize
             }
    hd1 = OvrTextureHeader
             { apiT = ovrRenderAPI_OpenGL
             , textureSize = OvrSizei width height
             , renderViewport = OvrRecti (OvrVector2i (div width 2) 0) vpSize
             }

{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Foreign
import Foreign.C


newtype HMD                = HMD (Ptr HMD)
newtype HMDToEyeViewOffset = HMDToEyeViewOffset (Ptr HMDToEyeViewOffset)
newtype OVRPose            = OVRPose (Ptr OVRPose)
newtype OVRTexture         = OVRTexture (Ptr OVRTexture)

type FrameIndex = CUInt

foreign import ccall "createHMD" 
    createHMD :: IO HMD

foreign import ccall "configureHMD" 
    configureHMD :: HMD -> IO HMDToEyeViewOffset

foreign import ccall "getHMDRenderTargetSize" 
    getHMDRenderTargetSize :: HMD -> IO (Ptr CInt)

foreign import ccall "createOVRTextureArray" 
    createOVRTextureArray :: GLuint -> CInt -> CInt -> IO OVRTexture

foreign import ccall "ovrHmd_BeginFrame" 
    ovrHmd_BeginFrame :: HMD -> FrameIndex -> IO CInt

foreign import ccall "beginFrame" 
    beginFrame :: HMD -> IO ()

foreign import ccall "getEyePoses" 
    getEyePoses :: HMD -> HMDToEyeViewOffset -> IO OVRPose

foreign import ccall "ovrHmd_EndFrame" 
    ovrHmd_EndFrame :: HMD -> OVRPose -> OVRTexture -> IO ()

foreign import ccall "free" 
    freeEyePoses :: OVRPose -> IO ()

overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f =
  liftIO (alloca (\p ->
                    do _ <- f p
                       peek p))

main :: IO ()
main = do

    hmd <- createHMD

    -- Should extract this from the HMD
    let (resX, resY) = (1920, 1080)
    win <- setupGLFW resX resY

    eyeViewOffsets <- configureHMD hmd

    [renderTargetSizeW, renderTargetSizeH] <- peekArray 2 =<< getHMDRenderTargetSize hmd

    (frameBuffer, frameBufferTexture) <- createFrameBuffer (fromIntegral renderTargetSizeW) (fromIntegral renderTargetSizeH)

    ovrTextureArray <- createOVRTextureArray frameBufferTexture renderTargetSizeW renderTargetSizeH
    
    glClearColor 0 1 1 1

    forever $ mainLoop win hmd frameBuffer ovrTextureArray eyeViewOffsets
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

mainLoop :: GLFW.Window -> HMD -> GLuint -> OVRTexture -> HMDToEyeViewOffset -> IO ()
mainLoop _win hmd frameBuffer frameBufferTexture eyeViewOffsets = do
    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Tell OVR API we're about to render a frame
    beginFrame hmd

    -- Bind the eye texture as the frame buffer to render into
    glBindFramebuffer GL_FRAMEBUFFER frameBuffer

    -- Get the current orientation and position of the HMD
    eyePoses <- getEyePoses hmd eyeViewOffsets

    -- Normally we'd render something here beyond just clearing the screen to a color
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    
    -- Unbind the eye texture
    glBindFramebuffer GL_FRAMEBUFFER 0

    -- Tell OVR API that we've finished our frame, passing the eyePoses and render texture so it can
    -- distort, timewarp, and blit to the screen.
    ovrHmd_EndFrame hmd eyePoses frameBufferTexture
    freeEyePoses eyePoses

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



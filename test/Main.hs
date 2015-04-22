
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.Oculus
import Data.Bits
import Control.Monad
import Foreign
import Linear
import Data.Foldable

import Cube
import Data.Time



data Eye = Eye { eyeIndex      :: Int
               , eyeProjection :: M44 GLfloat 
               , eyeViewport   :: (GLint, GLint, GLsizei, GLsizei)
               }

main :: IO ()
main = do

    hmd <- createHMD

    -- Should extract this from the HMD
    let (resX, resY) = (1920, 1080)
    win <- setupGLFW resX resY

    -- Oculus setup
    eyeRenderDescs <- configureHMD hmd
    eyeViewOffsets <- getEyeRenderDesc_HmdToEyeViewOffsets eyeRenderDescs
    leftEyeFOV  <- getEyeRenderDesc_FOV eyeRenderDescs 0
    rightEyeFOV <- getEyeRenderDesc_FOV eyeRenderDescs 1

    leftEyeProjection  <- getEyeProjection leftEyeFOV  0.01 1000
    rightEyeProjection <- getEyeProjection rightEyeFOV 0.01 1000

    [renderTargetSizeW, renderTargetSizeH] <- peekArray 2 =<< getHMDRenderTargetSize hmd

    (frameBuffer, frameBufferTexture) <- createFrameBuffer (fromIntegral renderTargetSizeW) (fromIntegral renderTargetSizeH)

    ovrTextureArray <- createOVRTextureArray (FramebufferTextureID (fromIntegral frameBufferTexture)) renderTargetSizeW renderTargetSizeH
    

    -- Scene rendering setup
    shader <- createShaderProgram "test/cube.v.glsl" "test/cube.f.glsl"
    cubeVAO <- makeCube shader

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    let eyes = [ Eye { eyeIndex = 0
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

    _ <- forever $ 
        mainLoop win hmd frameBuffer ovrTextureArray eyeViewOffsets eyes shader cubeVAO
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
    Just win <- GLFW.createWindow desiredW desiredH "Oculus Mini" Nothing Nothing
    (frameW, frameH) <- GLFW.getFramebufferSize win
    -- Compensate for retina framebuffers on Mac
    when (frameW > desiredW && frameH > desiredH) $ GLFW.setWindowSize win halfW halfH

    GLFW.makeContextCurrent (Just win)

    GLFW.swapInterval 1
    return win

mainLoop :: GLFW.Window -> HMD -> GLuint -> OVRTexture -> HMDToEyeViewOffset 
         -> [Eye]
         -> GLProgram -> VertexArrayObject -> IO ()
mainLoop _win hmd frameBuffer frameBufferTexture eyeViewOffsets eyes shader cubeVAO = do
    glGetErrors
    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Tell OVR API we're about to render a frame
    beginFrame hmd

    -- Bind the eye texture as the frame buffer to render into
    glBindFramebuffer GL_FRAMEBUFFER frameBuffer

    -- Get the current orientation and position of the HMD
    eyePoses <- getEyePoses hmd eyeViewOffsets

    -- Clear the framebuffer
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Normally we'd render something here beyond just clearing the screen to a color
    glUseProgram (fromIntegral (unGLProgram shader))
    uniform_mvp <- getShaderUniform shader "mvp"


    time <- realToFrac . utctDayTime <$> getCurrentTime
    let zoom = (* 8) . (subtract 1.2) . sin $ time
        rot  = axisAngle (V3 0 1 0) time

    forM_ eyes $ \eye -> do

        (eyeOrientation, eyePosition) <- getPoses_OrientationAndPositionForEye eyePoses (eyeIndex eye)
        let eyeTransform = mkTransformation eyeOrientation eyePosition
            projection   = eyeProjection eye
            model        = mkTransformation rot (V3 0 0 zoom)
            view         = lookAt (V3 0 2 0) (V3 0 0 zoom) (V3 0 1 0)
            mvp          = projection !*! eyeTransform !*! view !*! model
            (x,y,w,h)    = eyeViewport eye
        glViewport x y w h

        withArray (concatMap toList (transpose mvp)) $ \mvpPointer ->
            glUniformMatrix4fv (fromIntegral (unUniformLocation uniform_mvp)) 1 GL_FALSE mvpPointer
        glBindVertexArray (unVertexArrayObject cubeVAO)
        glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_INT nullPtr
        glBindVertexArray 0
    
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




import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.Oculus.API
import Data.Bits
import Control.Monad
import Linear
import Data.Time

import SetupGLFW
import ShaderLoader
import Cube

-----------------------------------------------------
-- A test of the raw Graphics.Oculus.API module.
-- You can use Graphics.Oculus.Scaffold for an easier
-- API based on this example.
-----------------------------------------------------


data Eye = Eye { eyeIndex      :: Int
               , eyeProjection :: M44 GLfloat 
               , eyeViewport   :: (GLint, GLint, GLsizei, GLsizei)
               }

main :: IO a
main = do
    -- Initialize the HMD
    hmd <- createHMD

    (resX, resY) <- getHMDResolution hmd
    
    -- Create our window
    win <- setupGLFW "Raw API" resX resY

    -- Configure the HMD with sane defaults
    eyeRenderDescs <- configureHMD hmd "Raw API"
    eyeViewOffsets <- getEyeRenderDesc_HmdToEyeViewOffsets eyeRenderDescs
    
    leftEyeFOV     <- getEyeRenderDesc_FOV eyeRenderDescs 0
    rightEyeFOV    <- getEyeRenderDesc_FOV eyeRenderDescs 1

    -- Get the perspective projection matrix for each eye
    leftEyeProjection  <- getEyeProjection leftEyeFOV  0.01 1000
    rightEyeProjection <- getEyeProjection rightEyeFOV 0.01 1000

    -- Find out how large our renderbuffer should be
    (renderTargetSizeW, renderTargetSizeH) <- getHMDRenderTargetSize hmd

    -- Create a framebuffer that we'll render into and pass to the Oculus SDK
    (frameBuffer, frameBufferTexture) <- createFrameBuffer (fromIntegral renderTargetSizeW) (fromIntegral renderTargetSizeH)

    -- Create the descriptors to tell the Oculus SDK about our framebuffer
    ovrTextureArray <- createOVRTextureArray (FramebufferTextureID (fromIntegral frameBufferTexture)) renderTargetSizeW renderTargetSizeH

    -- Load the shaders and geometry for our scene
    shader <- createShaderProgram "test/cube.v.glsl" "test/cube.f.glsl"
    cube   <- makeCube shader

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    -- Package up descriptions of each eye's perspective projection and render viewport
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

    -- Begin our renderloop
    forever $ 
        mainLoop win hmd frameBuffer ovrTextureArray eyeViewOffsets eyes shader cube

mainLoop :: GLFW.Window -> HMD -> GLuint -> OVRTexture -> HMDToEyeViewOffset 
         -> [Eye]
         -> GLProgram -> Cube -> IO ()
mainLoop _win hmd frameBuffer frameBufferTexture eyeViewOffsets eyes shader cube = do
    -- glGetErrors

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

    ------------------
    -- Render our cube
    ------------------

    -- Use the cube's shader
    glUseProgram (fromIntegral (unGLProgram shader))

    -- Vary the zoom and rotation of the cube based on time
    time <- realToFrac . utctDayTime <$> getCurrentTime
    let zoom = (* 8) . (subtract 1.2) . sin $ time
        rot  = axisAngle (V3 0 1 0) time

    -- For each eye...
    forM_ eyes $ \eye -> do

        -- Get its orientation and position
        (eyeOrientation, eyePosition) <- getPoses_OrientationAndPositionForEye eyePoses (eyeIndex eye)

            -- Convert the eye pose into a transformation matrix
        let eyeTransform = mkTransformation eyeOrientation eyePosition
            -- Get the perspective transform for this eye
            projection   = eyeProjection eye
            -- Rotate and zoom the cube
            model        = mkTransformation rot (V3 0 0 zoom)
            -- Look at the cube's position
            view         = lookAt (V3 0 2 0) (V3 0 0 zoom) (V3 0 1 0)
            -- Create the final model-view-project matrix
            mvp          = projection !*! eyeTransform !*! view !*! model
            -- Get this eye's viewport to render into
            (x,y,w,h)    = eyeViewport eye
        glViewport x y w h

        renderCube cube mvp
    
    -- Unbind the eye texture
    glBindFramebuffer GL_FRAMEBUFFER 0

    -- Tell OVR API that we've finished our frame, passing the eyePoses and render texture so it can
    -- distort, timewarp, and blit to the screen.
    ovrHmd_EndFrame hmd eyePoses frameBufferTexture
    -- Free the eye poses we allocated
    freePtr (unOVRPose eyePoses)

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



import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.Oculus

import Data.Bits
import Control.Monad
import Linear
--import Data.Time

import SetupGLFW
import ShaderLoader
import Cube

--------------------------------------
-- A test of the simple "Scaffold" API
--------------------------------------

main :: IO a
main = do

    window <- setupGLFW "Scaffold" 1024 768
    putStrLn "A"
    hmd <- createHMD
    GLFW.setWindowSize window 
        (fromIntegral . fst . hmdBufferSize $ hmd) 
        (fromIntegral . snd . hmdBufferSize $ hmd)
    putStrLn "B"
    recenterPose hmd

    -- Scene rendering setup
    shader <- createShaderProgram "test/cube.v.glsl" "test/cube.f.glsl"
    
    cube <- makeCube shader

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    putStrLn "we're here"
    forever $ 
        mainLoop window hmd shader cube


mainLoop :: GLFW.Window -> HMD -> GLProgram -> Cube -> IO ()
mainLoop window hmd shader cube = do

    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Vary the zoom and rotation of the cube based on time
    --time <- realToFrac . utctDayTime <$> getCurrentTime
    let zoom = 3 --(* 8) . (subtract 1.2) . sin $ time
        rot  = axisAngle (V3 0 1 0) 0--time
    -- Use the cube's shader
    glUseProgram (fromIntegral (unGLProgram shader))

    renderHMDFrame hmd $ \eyeViews -> do
        -- Clear the framebuffer
        glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

        let model        = mkTransformation rot (V3 0 0 zoom)
            -- Look at the cube's position
            view         = lookAt (V3 0 2 0) (V3 0 0 zoom) (V3 0 1 0)
        
        renderHMDEyes eyeViews $ \projection eyeView -> do
            let finalView = eyeView !*! view 
            let mvp = projection !*! finalView !*!  model
            renderCube cube mvp

    -- We must call swapBuffers ourselves for this one!
    renderHMDMirror hmd
    GLFW.swapBuffers window

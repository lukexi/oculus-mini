import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.Oculus

import Data.Bits
import Control.Monad
import Linear
import Data.Time

import SetupGLFW
import ShaderLoader
import Cube

-------------------------------------------------------------
-- A test to make sure our rendering works without the Oculus
-------------------------------------------------------------

main :: IO a
main = do

    hmd <- createHMD
    (resX, resY) <- getHMDResolution hmd
    _win <- setupGLFW "Scaffold" resX resY

    renderHMD <- configureHMDRendering hmd "Scaffold"

    -- Scene rendering setup
    shader <- createShaderProgram "test/cube.v.glsl" "test/cube.f.glsl"
    
    cube <- makeCube shader

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    forever $ 
        mainLoop renderHMD shader cube


mainLoop :: RenderHMD -> GLProgram -> Cube -> IO ()
mainLoop renderHMD shader cube = do

    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Vary the zoom and rotation of the cube based on time
    time <- realToFrac . utctDayTime <$> getCurrentTime
    let zoom = (* 8) . (subtract 1.2) . sin $ time
        rot  = axisAngle (V3 0 1 0) time
    -- Use the cube's shader
    glUseProgram (fromIntegral (unGLProgram shader))

    renderHMDFrame renderHMD $ \eyePoses -> do
        -- Clear the framebuffer
        glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

        let model        = mkTransformation rot (V3 0 0 zoom)
            -- Look at the cube's position
            view         = lookAt (V3 0 2 0) (V3 0 0 zoom) (V3 0 1 0)
        
        renderHMDEyes renderHMD eyePoses $ \projection -> do
            let mvp = projection !*! view !*! model
            renderCube cube mvp
    

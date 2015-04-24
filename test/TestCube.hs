import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL

import Data.Bits
import Control.Monad
import Linear

import SetupGLFW
import ShaderLoader
import Cube

-------------------------------------------------------------
-- A test to make sure our rendering works without the Oculus
-------------------------------------------------------------

main :: IO a
main = do

    let (resX, resY) = (1920, 1080)

    win <- setupGLFW "Cube" resX resY

    -- Scene rendering setup
    shader <- createShaderProgram "test/cube.v.glsl" "test/cube.f.glsl"
    
    cube <- makeCube shader

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    forever $ 
        mainLoop win shader cube


mainLoop :: GLFW.Window -> GLProgram -> Cube -> IO ()
mainLoop win shader cube = do
    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Clear the framebuffer
    glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

    -- Normally we'd render something here beyond just clearing the screen to a color
    glUseProgram (fromIntegral (unGLProgram shader))

    let projection = perspective 45 (1920/1080) 0.01 1000
        model      = mkTransformation 1 (V3 0 0 (-4))
        view       = lookAt (V3 0 2 0) (V3 0 0 (-4)) (V3 0 1 0)
        mvp        = projection !*! view !*! model
        (x,y,w,h)  = (0,0,1920,1080)

    glViewport x y w h

    renderCube cube mvp
    
    GLFW.swapBuffers win




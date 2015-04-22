import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL

import Data.Bits
import Control.Monad
import Foreign
import Linear
import Data.Foldable

import Cube


main :: IO ()
main = do

    -- Should extract this from the HMD
    let (resX, resY) = (1920, 1080)
    win <- setupGLFW resX resY

    -- Scene rendering setup
    shader <- createShaderProgram "test/cube.v.glsl" "test/cube.f.glsl"
    cubeVAO <- makeCube shader

    glClearColor 0 1 1 1
    glEnable GL_DEPTH_TEST

    _ <- forever $ 
        mainLoop win shader cubeVAO
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
    Just win <- GLFW.createWindow desiredW desiredH "Cube" Nothing Nothing
    (frameW, frameH) <- GLFW.getFramebufferSize win
    -- Compensate for retina framebuffers on Mac
    when (frameW > desiredW && frameH > desiredH) $ GLFW.setWindowSize win halfW halfH

    GLFW.makeContextCurrent (Just win)

    GLFW.swapInterval 1
    return win

mainLoop :: GLFW.Window -> GLProgram -> VertexArrayObject -> IO ()
mainLoop win shader cubeVAO = do
    glGetErrors
    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Clear the framebuffer
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Normally we'd render something here beyond just clearing the screen to a color
    glUseProgram (fromIntegral (unGLProgram shader))
    uniform_mvp <- getShaderUniform shader "mvp"

    let projection = perspective 45 (1920/1080) 0.01 1000
        model      = mkTransformation 1 (V3 0 0 (-4))
        view       = lookAt (V3 0 2 0) (V3 0 0 (-4)) (V3 0 1 0)
        mvp        = projection !*! view !*! model
        (x,y,w,h)  = (0,0,1920,1080)
    glViewport x y w h

    withArray (concatMap toList (transpose mvp)) $ \mvpPointer ->
        glUniformMatrix4fv (fromIntegral (unUniformLocation uniform_mvp)) 1 GL_FALSE mvpPointer
    glBindVertexArray (unVertexArrayObject cubeVAO)
    glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0
    
    GLFW.swapBuffers win
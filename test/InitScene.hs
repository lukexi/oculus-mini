module InitScene where

import qualified Graphics.UI.GLFW as GLFW

import Control.Monad

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
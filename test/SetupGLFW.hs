module SetupGLFW where

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

    
    Just win <- GLFW.createWindow desiredW desiredH "Cube" Nothing Nothing
    

    -- Compensate for retina framebuffers on Mac
    (frameW, frameH) <- GLFW.getFramebufferSize win
    when (frameW > desiredW && frameH > desiredH) $
        GLFW.setWindowSize win (desiredW `div` 2) (desiredH `div` 2)
    
    GLFW.makeContextCurrent (Just win)

    GLFW.swapInterval 1
    return win
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL

import Data.Bits
import Control.Monad
import Foreign
import Linear
import Data.Foldable

import ShaderLoader
import InitScene

import Cube




main :: IO a
main = do

    -- Should extract this from the HMD
    let resX  =  1920
        resY  =  1080

    win <- setupGLFW resX resY

    -- Scene rendering setup
    shader <- createShaderProgram "test/cube.v.glsl" "test/cube.f.glsl"
    
    cubeVAO <- makeCube shader

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    forever $ 
        mainLoop win shader cubeVAO


mainLoop :: GLFW.Window -> GLProgram -> VertexArrayObject -> IO ()

mainLoop win shader cubeVAO = do

    glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Clear the framebuffer
    glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

    -- Normally we'd render something here beyond just clearing the screen to a color
    glUseProgram ( fromIntegral ( unGLProgram shader ) )

    uniformMVP <- getShaderUniform shader "mvp"

    let projection = perspective 45 (1920/1080) 0.01 1000
        model      = mkTransformation 1 (V3 0 0 (-4))
        view       = lookAt (V3 0 2 0) (V3 0 0 (-4)) (V3 0 1 0)
        mvp        = projection !*! view !*! model
        (x,y,w,h)  = (0,0,1920,1080)

    glViewport x y w h

    withArray ( concatMap toList ( transpose mvp ) ) $ 

            glUniformMatrix4fv ( fromIntegral ( unUniformLocation uniformMVP ) ) 1 GL_FALSE


    glBindVertexArray ( unVertexArrayObject cubeVAO )

    glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_INT nullPtr

    glBindVertexArray 0
    
    GLFW.swapBuffers win












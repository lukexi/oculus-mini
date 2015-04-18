import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Bindings.OculusRift
import Data.Bits

main :: IO ()
main = do

    _ <- ovr_Initialize

    _ <- GLFW.init

    glClearColor 0 1 1 1

    mainLoop 0
    return ()

mainLoop :: Integer -> IO a
mainLoop frameNo = do
    GLFW.pollEvents

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    mainLoop (succ frameNo)
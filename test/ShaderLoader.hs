module ShaderLoader where

import Graphics.GL

import Control.Monad
import Control.Monad.Trans
import Foreign

import Foreign.C.String

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f = liftIO (alloca (\p -> f p >> peek p))

newtype GLProgram         = GLProgram           { unGLProgram           :: GLuint }
newtype VertexArrayObject = VertexArrayObject   { unVertexArrayObject   :: GLuint }
newtype AttributeLocation = AttributeLocation   { unAttributeLocation   :: GLint  }
newtype UniformLocation   = UniformLocation     { unUniformLocation     :: GLint  }
newtype TextureID         = TextureID           { unTextureID           :: GLuint }

---------------
-- Load shaders
---------------

createShaderProgram :: FilePath -> FilePath -> IO GLProgram

createShaderProgram vertexShaderPath fragmentShaderPath =
   
    do vertexShader <- glCreateShader GL_VERTEX_SHADER
       compileShader vertexShaderPath vertexShader
       fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
       compileShader fragmentShaderPath fragmentShader
       shaderProg <- glCreateProgram
       glAttachShader shaderProg vertexShader
       glAttachShader shaderProg fragmentShader
       glLinkProgram shaderProg
       linked <- overPtr (glGetProgramiv shaderProg GL_LINK_STATUS)
       when (linked == GL_FALSE)
            (do maxLength <- overPtr (glGetProgramiv shaderProg GL_INFO_LOG_LENGTH)
                logLines <- allocaArray
                              (fromIntegral maxLength)
                              (\p ->
                                 alloca (\lenP ->
                                           do glGetProgramInfoLog shaderProg maxLength lenP p
                                              len <- peek lenP
                                              peekCStringLen (p,fromIntegral len)))
                putStrLn logLines)
       return (GLProgram shaderProg)
    where compileShader path shader =
            do src <- Text.readFile path
               BS.useAsCString
                 (Text.encodeUtf8 src)
                 (\ptr ->
                    withArray [ptr]
                              (\srcs ->
                                 glShaderSource shader 1 srcs nullPtr))
               glCompileShader shader
               when True
                    (do maxLength <- overPtr (glGetShaderiv shader GL_INFO_LOG_LENGTH)
                        logLines <- allocaArray
                                      (fromIntegral maxLength)
                                      (\p ->
                                         alloca (\lenP ->
                                                   do glGetShaderInfoLog shader maxLength lenP p
                                                      len <- peek lenP
                                                      peekCStringLen (p,fromIntegral len)))
                        putStrLn logLines)


getShaderAttribute :: GLProgram -> String -> IO AttributeLocation
getShaderAttribute (GLProgram prog) attributeName = do
    location <- withCString attributeName $ \attributeNameCString -> 
        glGetAttribLocation prog attributeNameCString
    when (location == -1) $ error $ "Coudn't bind attribute: " ++ attributeName 
    return (AttributeLocation location)

getShaderUniform :: GLProgram -> String -> IO UniformLocation
getShaderUniform (GLProgram prog) uniformName = do
    location <- withCString uniformName $ \uniformNameCString -> 
        glGetUniformLocation prog uniformNameCString
    when (location == -1) $ error $ "Coudn't bind uniform: " ++ uniformName 
    return (UniformLocation location)

glGetErrors :: IO ()
glGetErrors = do
  code <- glGetError
  case code of
    GL_NO_ERROR -> return ()
    e -> do
      case e of
        GL_INVALID_ENUM -> putStrLn "* Invalid Enum"
        GL_INVALID_VALUE -> putStrLn "* Invalid Value"
        GL_INVALID_OPERATION -> putStrLn "* Invalid Operation"
        GL_INVALID_FRAMEBUFFER_OPERATION -> putStrLn "* Invalid Framebuffer Operation"
        GL_OUT_OF_MEMORY -> putStrLn "* OOM"
        GL_STACK_UNDERFLOW -> putStrLn "* Stack underflow"
        GL_STACK_OVERFLOW -> putStrLn "* Stack overflow"
        _ -> return ()
      glGetErrors




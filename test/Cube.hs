module Cube where

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

newtype GLProgram         = GLProgram { unGLProgram :: GLuint }
newtype VertexArrayObject = VertexArrayObject { unVertexArrayObject :: GLuint}
newtype AttributeLocation = AttributeLocation { unAttributeLocation :: GLint }
newtype UniformLocation   = UniformLocation { unUniformLocation :: GLint }

---------------------------------------------------------
-- Load shaders
---------------------------------------------------------

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
               glCompileShader shader -- GL.get (GL.shaderInfoLog shader) >>= putStrLn


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

----------------------------------------------------------
-- Make Cube
----------------------------------------------------------
makeCube :: GLProgram -> IO VertexArrayObject
makeCube program = do
    attribute_coord3d <- getShaderAttribute program "coord3d"
    attribute_v_color <- getShaderAttribute program "v_color"
    -- Setup a VAO
    vao_cube <- overPtr (glGenVertexArrays 1)
    glBindVertexArray vao_cube
    -- printf("%i\n", vao_cube);
    
    -- Buffer the cube vertices
    let cube_vertices = [
            -- front
            -1.0, -1.0,  1.0,
             1.0, -1.0,  1.0,
             1.0,  1.0,  1.0,
            -1.0,  1.0,  1.0,
            -- back
            -1.0, -1.0, -1.0,
             1.0, -1.0, -1.0,
             1.0,  1.0, -1.0,
            -1.0,  1.0, -1.0
            ] :: [GLfloat]
    vbo_cube_vertices <- overPtr (glGenBuffers 1)
    glBindBuffer GL_ARRAY_BUFFER vbo_cube_vertices
    let cube_vertices_size = fromIntegral (sizeOf (undefined :: GLfloat) * length cube_vertices)
    withArray cube_vertices $ \cube_vertices_ptr ->
        glBufferData GL_ARRAY_BUFFER cube_vertices_size (castPtr cube_vertices_ptr) GL_STATIC_DRAW

    -- Describe our vertices array to OpenGL (it can't guess its format automatically)
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation attribute_coord3d))
    glVertexAttribPointer
        (fromIntegral (unAttributeLocation attribute_coord3d)) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    -- Buffer the cube colors
    let cube_colors = [
            -- front colors
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0,
            1.0, 1.0, 1.0,
            -- back colors
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0,
            1.0, 1.0, 1.0
            ] :: [GLfloat]
    vbo_cube_colors <- overPtr (glGenBuffers 1)
    glBindBuffer GL_ARRAY_BUFFER vbo_cube_colors
    let cube_colors_size = fromIntegral (sizeOf (undefined :: GLfloat) * length cube_colors)
    withArray cube_colors $ \cube_colors_ptr ->
        glBufferData GL_ARRAY_BUFFER cube_colors_size (castPtr cube_colors_ptr) GL_STATIC_DRAW
    
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation attribute_v_color))
    glVertexAttribPointer
        (fromIntegral (unAttributeLocation attribute_v_color)) -- attribute
        3                 -- number of elements per vertex, here (R,G,B)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    -- Buffer the cube indices
    let cube_elements = [
            -- front
            0, 1, 2,
            2, 3, 0,
            -- top
            1, 5, 6,
            6, 2, 1,
            -- back
            7, 6, 5,
            5, 4, 7,
            -- bottom
            4, 0, 3,
            3, 7, 4,
            -- left
            4, 5, 1,
            1, 0, 4,
            -- right
            3, 2, 6,
            6, 7, 3
            ] :: [GLuint]
    ibo_cube_elements <- overPtr (glGenBuffers 1)
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo_cube_elements
    let cube_elements_size = fromIntegral (sizeOf (undefined::GLuint) * length cube_elements)
    withArray cube_elements $ \cube_elements_ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER cube_elements_size (castPtr cube_elements_ptr) GL_STATIC_DRAW
    
    glBindVertexArray 0

    return (VertexArrayObject vao_cube)

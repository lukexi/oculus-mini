module Cube where

import ShaderLoader

import Graphics.GL
import Foreign



----------------------------------------------------------
-- Make Cube
----------------------------------------------------------

makeCube :: GLProgram -> IO VertexArrayObject

makeCube program = do

    aPosition      <- getShaderAttribute program "aPosition"
    aColor         <- getShaderAttribute program "aColor"
    aID            <- getShaderAttribute program "aID"

    -- Setup a VAO
    vaoCube <- overPtr ( glGenVertexArrays 1 )

    glBindVertexArray vaoCube



    {-

        Cube Positions

    -}
    
    -- Buffer the cube vertices
    let cubeVertices = 


            
            [ 

            --- front
              -1.0 , -1.0 ,  1.0  
            ,  1.0 , -1.0 ,  1.0  
            ,  1.0 ,  1.0 ,  1.0  
            , -1.0 ,  1.0 ,  1.0  

            --- back
            , -1.0 , -1.0 , -1.0  
            ,  1.0 , -1.0 , -1.0  
            ,  1.0 ,  1.0 , -1.0  
            , -1.0 ,  1.0 , -1.0 

            ] :: [GLfloat]



    vaoCubeVertices <- overPtr ( glGenBuffers 1 )

    glBindBuffer GL_ARRAY_BUFFER vaoCubeVertices



    let cubeVerticesSize = fromIntegral ( sizeOf ( undefined :: GLfloat ) * length cubeVertices )

    withArray cubeVertices $ 
        
        \cubeVerticesPtr ->
            glBufferData GL_ARRAY_BUFFER cubeVerticesSize (castPtr cubeVerticesPtr ) GL_STATIC_DRAW 
        


    -- Describe our vertices array to OpenGL (it can't guess its format automatically)
    glEnableVertexAttribArray ( fromIntegral ( unAttributeLocation aPosition ) )

    glVertexAttribPointer
        ( fromIntegral ( unAttributeLocation aPosition ) ) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element



    {-

        Cube Colors

    -}

    -- Buffer the cube colors
    let cubeColors = 

            [

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


    vboCubeColors <- overPtr ( glGenBuffers 1 )

    glBindBuffer GL_ARRAY_BUFFER vboCubeColors


    let cubeColorsSize = fromIntegral ( sizeOf ( undefined :: GLfloat ) * length cubeColors )
    
    withArray cubeColors $

         \cubeColorsPtr ->
            glBufferData GL_ARRAY_BUFFER cubeColorsSize ( castPtr cubeColorsPtr ) GL_STATIC_DRAW

    
    glEnableVertexAttribArray ( fromIntegral ( unAttributeLocation aColor ) )

    glVertexAttribPointer
        ( fromIntegral ( unAttributeLocation aColor ) ) -- attribute
        3                 -- number of elements per vertex, here (R,G,B)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element



    {-

        Cube IDs

    -}

    -- Buffer the cube colors
    let cubeIDs = 

            [ 0
            , 1 
            , 2
            , 3
            , 4
            , 5
            ] :: [GLfloat]


    vboCubeIDs <- overPtr ( glGenBuffers 1 )

    glBindBuffer GL_ARRAY_BUFFER vboCubeIDs


    let cubeIDsSize = fromIntegral ( sizeOf ( undefined :: GLfloat ) * length cubeIDs )
    
    withArray cubeIDs $

         \cubeIDsPtr ->
            glBufferData GL_ARRAY_BUFFER cubeIDsSize ( castPtr cubeIDsPtr ) GL_STATIC_DRAW

    
    glEnableVertexAttribArray ( fromIntegral ( unAttributeLocation aID ) )

    glVertexAttribPointer
        ( fromIntegral ( unAttributeLocation aID ) ) -- attribute
        1                 -- number of elements per vertex, here (R,G,B)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element



    {-

        Cube Indicies

    -}

    -- Buffer the cube indices
    let cubeIndices = 
            [
                
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
    
    iboCubeElements <- overPtr ( glGenBuffers 1 )
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER iboCubeElements

    let cubeElementsSize = fromIntegral ( sizeOf ( undefined::GLuint ) * length cubeIndices )
    
    withArray cubeIndices $ 

        \cubeIndicesPtr ->
            glBufferData GL_ELEMENT_ARRAY_BUFFER cubeElementsSize ( castPtr cubeIndicesPtr ) GL_STATIC_DRAW
    
    glBindVertexArray 0

    return ( VertexArrayObject vaoCube )



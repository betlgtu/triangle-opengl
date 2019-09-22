module Shaders where

import Graphics.UI.GLUT
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable()
import Foreign.C.Types()
import qualified Data.ByteString as BS
import System.IO
import Control.Monad

data State = State 
    {
        vertexBuffer :: BufferObject,
        gpuProgram :: Program
    }

triangleVertexes :: [GLfloat]
triangleVertexes = [
        0.0,  0.5,   0.0, 1.0,
        0.5, -0.366, 0.0, 1.0,
       -0.5, -0.366, 0.0, 1.0,
        1.0,  0.0,   0.0, 1.0,
        0.0,  1.0,   0.0, 1.0,
        0.0,  0.0,   1.0, 1.0
    ]

fragmentShaderFilePath :: FilePath
fragmentShaderFilePath = "shader.frag"

vertexShaderFilePath :: FilePath
vertexShaderFilePath = "shader.vert"

createVertexBuffer :: [GLfloat] -> IO BufferObject
createVertexBuffer vertexes = do
    bufferObject <- genObjectName
    bindBuffer ArrayBuffer $= Just bufferObject
    withArrayLen vertexes $ \count arr ->
        bufferData ArrayBuffer $= (fromIntegral count * 4, arr, StaticDraw)
    setVertexAttribArray Enabled
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor vertexNumComponents Float 0 nullPtr)
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor colorNumComponents Float 0 (plusPtr nullPtr 48))
    return bufferObject

vertexNumComponents :: NumComponents
vertexNumComponents = 4

colorNumComponents :: NumComponents
colorNumComponents = 4

initializeState :: IO State
initializeState = do
    bufferObject <- createVertexBuffer triangleVertexes
    program <- initGPUProgram
    return $ State 
        {
            vertexBuffer = bufferObject,
            gpuProgram = program
        }

loadShader :: ShaderType -> FilePath -> IO Shader
loadShader t path = do
    shader <- createShader t
    source <- BS.readFile path
    shaderSourceBS shader $= source
    compileShader shader
    status <- get (compileStatus shader)
    unless status $ putStrLn . (("message" ++ " log: ") ++) =<< get (shaderInfoLog shader)
    return shader

initGPUProgram :: IO Program
initGPUProgram = do
    vertexShader <- loadShader VertexShader vertexShaderFilePath
    fragmentShader <- loadShader FragmentShader fragmentShaderFilePath
    let shaders = [vertexShader, fragmentShader]
    program <- createProgram
    attachShader program vertexShader
    attachShader program fragmentShader
    linkProgram program
    mapM_ (detachShader program) shaders
    return program

display :: State -> DisplayCallback
display state = do
    clearColor $= Color4 1.0 0.0 1.0 1.0
    clear [ ColorBuffer ]
    bindBuffer ArrayBuffer $= Just (vertexBuffer state)
    setVertexAttribArray Enabled
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor vertexNumComponents Float 0 nullPtr)
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor colorNumComponents Float 0 (plusPtr nullPtr 48))
    drawArrays Triangles 0 3
    setVertexAttribArray Disabled
    currentProgram $= Just (gpuProgram state)
    swapBuffers
    checkError "display"

setVertexAttribArray :: Capability -> IO ()
setVertexAttribArray capability = do
    vertexAttribArray (AttribLocation 0) $= capability
    vertexAttribArray (AttribLocation 1) $= capability

reshape :: State -> ReshapeCallback
reshape state size = viewport $= (Position 0 0, size)

checkError :: String -> IO ()
checkError functionName = get errors >>= mapM_ reportError
    where reportError e = putStrLn (showError e ++ " detected in " ++ functionName)
          showError (Error category message) = "GL error " ++ show category ++ " (" ++ message ++ ")"

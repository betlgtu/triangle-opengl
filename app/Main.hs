module Main where

import Shaders
import Graphics.UI.GLUT

main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithAlphaComponent, WithDepthBuffer ]
    _ <- createWindow progName
    state <- initializeState
    displayCallback $= display state
    reshapeCallback $= Just (reshape state)
    mainLoop
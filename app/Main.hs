{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import SDL
import Data.Word
import System.Environment (getArgs)
import Control.Monad (unless)

windowWidth = 640
windowHeight = 480

tileWidth = 16
tileHeight = 16

tilesPerRow = 16

screenWidth = windowWidth `div` tileWidth
screenHeight = windowHeight `div` tileHeight

main :: IO ()
main = do
    args <- getArgs
    initializeAll
    window <- createWindow "SDL Test" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    rendererLogicalSize renderer $= Just (V2 windowWidth windowHeight)
    startTick <- ticks
    tileset <- loadImg renderer (head args)
    let eventIsQPress event = 
                  case eventPayload event of
                    KeyboardEvent ke -> 
                        keyboardEventKeyMotion ke == Pressed &&
                        keysymKeycode (keyboardEventKeysym ke) == KeycodeQ
                    _ -> False
        mainLoop = 
          do
            events <- pollEvents
            
            clear renderer
            drawScreen renderer tileset debugScreen
            present renderer
            unless (any eventIsQPress events) mainLoop
      in mainLoop
    
loadImg :: Renderer -> String -> IO Texture
loadImg r file = do
    surf <- loadBMP file
    surfaceColorKey surf $= Just magenta
    createTextureFromSurface r surf

drawScreen :: Renderer -> Texture -> Screen -> IO ()    
drawScreen r tileset screen = 
  let
    coords = [ V2 x y | x <- [0..screenWidth], y <- [0..screenHeight] ]
    drawOne coord@(V2 x y) = 
      let 
        (Cell ch fg bg) = screen (fmap fromIntegral coord)
        tileCoord = V2 (fromIntegral $ ch `mod` tilesPerRow) 
                       (fromIntegral $ ch `div` tilesPerRow) 
        tileSz = V2 tileWidth tileHeight
        srcRect = Rectangle (P (tileCoord * tileSz)) tileSz
        dstRect = Rectangle (P (coord * tileSz)) tileSz
      in do
        textureColorMod tileset $= noAlpha fg
        rendererDrawColor r $= bg
        fillRect r (Just dstRect)
        rendererDrawBlendMode r $= BlendAlphaBlend
        copy r tileset (Just srcRect) (Just dstRect) 
  in
    mapM_ drawOne coords
        
debugScreen :: Screen
debugScreen (V2 x y) = 
    let 
        c = fromIntegral ((x + y) `mod` 256) :: Word8 
    in 
        Cell { ch = c, fg = white, bg = black }


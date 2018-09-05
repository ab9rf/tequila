{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import SDL
import Data.Word
import System.Environment (getArgs)
import Control.Monad (unless)

main :: IO ()
main = do
    args <- getArgs
    initializeAll
    window <- createWindow "SDL Test" defaultWindow { windowVisible = False }
    renderer <- createRenderer window (-1) defaultRenderer
    startTick <- ticks
    tileset <- loadImg renderer (head args)
    tilesetInfo <- queryTexture tileset
    let 
      tilesPerRow = 16
      tilesPerColumn = 256 `div` tilesPerRow
      tileWidth = (fromIntegral (textureWidth tilesetInfo)) `div` tilesPerRow
      tileHeight = (fromIntegral (textureHeight tilesetInfo)) `div` tilesPerColumn
      screenWidth = 80
      screenHeight = 25
      windowWidth = screenWidth * tileWidth
      windowHeight = screenHeight * tileHeight
      eventIsQPress event = 
                case eventPayload event of
                  KeyboardEvent ke -> 
                      keyboardEventKeyMotion ke == Pressed &&
                      keysymKeycode (keyboardEventKeysym ke) == KeycodeQ
                  _ -> False
      drawScreen :: Renderer -> Texture -> Screen -> IO ()    
      drawScreen r tileset screen = 
        let
          coords = [ V2 x y | x <- [0..screenWidth], y <- [0..screenHeight] ]
          drawOne coord@(V2 x y) = 
            let 
              (Cell ch fg bg) = screen (fmap fromIntegral coord)
              tileCoord = fmap fromIntegral $ V2 
                ((fromIntegral ch) `mod` tilesPerRow) 
                ((fromIntegral ch) `div` tilesPerRow) 
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
      mainLoop = 
        do
          events <- pollEvents
          clear renderer
          drawScreen renderer tileset debugScreen
          present renderer
          unless (any eventIsQPress events) mainLoop
      in do 
        windowSize window $= (V2 windowWidth windowHeight)
        rendererLogicalSize renderer $= Just (V2 windowWidth windowHeight)
        showWindow window
        mainLoop
    
loadImg :: Renderer -> String -> IO Texture
loadImg r file = do
    surf <- loadBMP file
    surfaceColorKey surf $= Just magenta
    createTextureFromSurface r surf

        
debugScreen :: Screen
debugScreen (V2 x y) = 
    let 
        c = fromIntegral ((x + y) `mod` 256) :: Word8 
    in 
        Cell { ch = c, fg = white, bg = black }


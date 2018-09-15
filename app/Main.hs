{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Control.Monad.State
import Control.Monad.Zip
import Data.Word
import Lib
import SDL
import System.Environment (getArgs)

data RendererState = RendererState
  { rsWindow :: Window
  , rsRenderer :: Renderer
  , rsTileSet :: Texture
  , rsTileSetInfo :: TextureInfo
  , rsTileSize :: V2 Int
  } deriving (Eq)

type RSIO a = StateT RendererState IO a

findTileSize :: TextureInfo -> V2 Int
findTileSize tilesetInfo =
  let tilesPerRow = 16
      tilesPerColumn = 256 `div` tilesPerRow
      tileWidth = (fromIntegral (textureWidth tilesetInfo)) `div` tilesPerRow
      tileHeight =
        (fromIntegral (textureHeight tilesetInfo)) `div` tilesPerColumn
   in V2 tileWidth tileHeight

main :: IO ()
main = do
  args <- getArgs
  initializeAll
  window <-
    createWindow
      "SDL Test"
      defaultWindow {windowVisible = False, windowResizable = True}
  renderer <- createRenderer window (-1) defaultRenderer
  tileset <- loadImg renderer (head args)
  tilesetInfo <- queryTexture tileset
  let tileSize = findTileSize tilesetInfo
      winSize = fmap fromIntegral (tileSize * (V2 80 25))
      renderState = RendererState window renderer tileset tilesetInfo tileSize
   in runStateT
        (do windowSize window $= winSize
            rendererLogicalSize renderer $= Just winSize
            showWindow window
            mainLoop)
        renderState
  return ()

mainLoop :: RSIO ()
mainLoop =
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent ke ->
            keyboardEventKeyMotion ke == Pressed &&
            keysymKeycode (keyboardEventKeysym ke) == KeycodeQ
          _ -> False
      isWindowResizeEvent event =
        case eventPayload event of
          WindowResizedEvent re -> True
          _ -> False
      mainLoop = do
        window <- rsWindow <$> Control.Monad.State.get
        renderer <- rsRenderer <$> Control.Monad.State.get
        events <- pollEvents
        winSize <- SDL.get (windowSize window)
        rendererLogicalSize renderer $= Just winSize
        clear renderer
        drawScreen debugScreen
        present renderer
        unless (any eventIsQPress events) mainLoop
   in do mainLoop

drawScreen :: Screen -> RSIO ()
drawScreen screen = do
  rs <- Control.Monad.State.get
  winSize <- SDL.get (windowSize (rsWindow rs))
  let r = rsRenderer rs
      tileset = rsTileSet rs
      tileSz = fmap fromIntegral (rsTileSize rs)
      V2 screenWidth screenHeight = mzipWith div winSize tileSz
      coords = [V2 x y | x <- [0 .. screenWidth], y <- [0 .. screenHeight]]
      drawOne coord@(V2 x y) =
        let tilesPerRow = 16
            (Cell ch fg bg) = screen (fmap fromIntegral coord)
            tileCoord =
              fmap fromIntegral $
              V2
                ((fromIntegral ch) `mod` tilesPerRow)
                ((fromIntegral ch) `div` tilesPerRow)
            srcRect = Rectangle (P (tileCoord * tileSz)) tileSz
            dstRect = Rectangle (P (coord * tileSz)) tileSz
         in do textureColorMod tileset $= noAlpha fg
               rendererDrawColor r $= bg
               fillRect r (Just dstRect)
               rendererDrawBlendMode r $= BlendAlphaBlend
               copy r tileset (Just srcRect) (Just dstRect)
   in mapM_ drawOne coords

loadImg :: (MonadIO m) => Renderer -> String -> m Texture
loadImg r file = do
  surf <- loadBMP file
  surfaceColorKey surf $= Just magenta
  createTextureFromSurface r surf

debugScreen :: Screen
debugScreen (V2 x y) =
  let c = fromIntegral ((x + y) `mod` 256) :: Word8
   in Cell {ch = c, fg = white, bg = black}

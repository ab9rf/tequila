module Lib
  ( Screen(..)
  , Cell(..)
  , Color(..)
  , white
  , black
  , magenta
  , noAlpha
  ) where

import Data.Word (Word8)
import SDL.Vect (V2, V3(V3), V4(V4))

type Color = V4 Word8

type Screen = V2 Int -> Cell

data Cell = Cell
  { ch :: Word8 -- not character!
  , fg :: Color
  , bg :: Color
  }

white :: Color
white = V4 255 255 255 255

black :: Color
black = V4 0 0 0 255

magenta :: Color
magenta = V4 255 0 255 255

noAlpha :: V4 a -> V3 a
noAlpha (V4 r g b _) = V3 r g b

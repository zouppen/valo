module LightSource where

import DMX
import Data.Word


data Color = Color Word8 Word8 Word8

data Light = Light {
      address :: Int
    , bus     :: Bus
}

setLight (Light addr bus) (Color r g b) = do
    setChannel bus (addr+1) r
    setChannel bus (addr+2) g
    setChannel bus (addr+3) b

-- |Common operations for all lights.
module Light where

import Data.Word
import Common (iMult)

class Light t where
  setRGB :: t -> Word8 -> Word8 -> Word8 -> IO ()
  setRGB t r g b = setIntensity t $ 0.30 `iMult` r + 0.59 `iMult` g + 0.11 `iMult` b
  
  --setHSV :: t -> Word8 -> Word8 -> Word8 -> IO ()
  
  setIntensity :: t -> Word8 -> IO ()  
  setIntensity t i = setRGB t i i i



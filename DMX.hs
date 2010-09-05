module DMX where

import Data.Word
import System.IO
import Data.Array.IO

data Bus = Bus {
      devH   :: Handle
    , states :: IOUArray Int Word8
}

busLength = 512 + 5 -- Hard-coded 512-length bus plus header.
label     = 6       -- Mystical constant

createBus device = do
  h <- openFile device WriteMode
  arr <- newArray (0,busLength) 0
  
  -- Setting static values for DMX bus.
  writeArray arr 0 0x7e
  writeArray arr 1 label
  writeArray arr 3 0x02 -- Data length: 512
 
  return $ Bus h arr

closeBus bus = do
  hClose $ devH bus

sendDMX bus = do
  hPutArray (devH bus) (states bus) busLength

